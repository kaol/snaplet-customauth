{-# LANGUAGE OverloadedStrings #-}

module Piperka.Maint.Splices (renderGenentry) where

import Control.Error.Util
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as B8
import Data.List (partition)
import qualified Data.Map.Lazy as M
import Data.Map.Syntax
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.Lazy as L
import Data.UUID
import qualified Data.Vector as V
import Heist
import Heist.Compiled
import Heist.Compiled.Extra (eitherDeferMap)
import qualified HTMLEntities.Text as HTML
import Network.Mail.SMTP (Address(..), sendMail, simpleMail, plainTextPart)
import Snap
import qualified Text.XmlHtml as X

import Application
import Piperka.API.Submit.Banner (saveFromSubmit)
import Piperka.ComicInfo.Epedia (eEntries)
import Piperka.Maint.Types
import Piperka.Maint.Query

-- Restrict for now to kaol
renderGenentry
  :: RuntimeAppHandler MyData
renderGenentry n = do
  node <- getParamNode
  let (special, content) =
        partition (maybe False (`elem` ["success", "failure"]) . X.tagName) $
        X.childNodes node
      part x = X.childNodes $ head $ filter ((== (Just x)) . X.tagName) special
      success = withSplices (runNodeList $ part "success") $ do
        "href" ## pureSplice . textSplice $
          ("info.html?cid=" <>) . T.pack . show
      failure = withSplices (runNodeList $ part "failure") $ do
        "message" ## pureSplice . textSplice $ id
  eitherDeferMap (return . bool (Left ()) (Right ()) . (/= 3) . uid)
    (const $ do
        tpl1 <- eitherDeferMap processSubmit
                failure success n
        tpl2 <- withLocalSplices precrawl mempty $ runNodeList content
        return $ yieldRuntime $ do
          isSubmit <- (== (Just "genentry")) <$> lift (getPostParam "formtype")
          codeGen $ if isSubmit then tpl1 else tpl2
    )
    (const $ return $ yieldRuntimeText $ do
        lift $ modifyResponse $ setResponseStatus 403 "Forbidden"
        return "Forbidden") n

precrawl
  :: Splices (Splice AppHandler)
precrawl = do
  "newCid" ## return $ yieldRuntimeText $
    T.pack . show . either (const (-1)) id <$> (lift nextCid)
  "precrawl" ## manyWithSplices runChildren
    ("page" ## pureSplice . textSplice $ maybe "<i>NULL</i>" HTML.text)
    (either (const V.empty) id <$> (lift precrawlArchive))

processSubmit
  :: MyData
  -> RuntimeSplice AppHandler (Either Text Int)
processSubmit u = do
  params <- lift getParams
  csrf <- (fromASCIIBytes =<<) <$> lift (getParam "csrf_ham")
  let lookupSingle x = listToMaybe =<< M.lookup x params
      lookupText x = hush . decodeUtf8' =<< lookupSingle x
      lookupBool = pure . maybe False (== "1") . lookupSingle
      lookupInt x = (fromIntegral . fst) <$> (B8.readInt =<< lookupSingle x)
      nullToNothing n = Just $ if T.null n then Nothing else Just n
      tgs = maybe [] (map (fromIntegral . fst) . catMaybes . map B8.readInt) $
            M.lookup "category" params
      eps = eEntries params
      genentry = Genentry
        <$> lookupInt "sid"
        <*> lookupInt "cid"
        <*> (T.strip <$> lookupText "title")
        <*> (T.stripStart <$> lookupText "homepage")
        <*> (nullToNothing =<< T.stripStart <$> lookupText "fixed_head")
        <*> (T.stripStart <$> lookupText "url_base")
        <*> (lookupText "url_tail")
        <*> lookupBool "acceptbanner"
        <*> (T.strip <$> lookupText "description")
        <*> (T.strip <$> lookupText "email")
        <*> lookupBool "want_email"
        <*> lookupText "email_subject"
        <*> lookupText "email_message"
        <*> (nullToNothing =<< T.strip <$> lookupText "bookmark_regexp")
        <*> lookupInt "parser_id"
        <*> (nullToNothing =<< lookupText "extra_data")
        <*> (nullToNothing =<< lookupText "extra_url")
        <*> pure tgs
        <*> pure eps
  lift $ runExceptT $ do
    when (csrf /= (Just $ ucsrfToken u)) $ throwE "csrf fail"
    entry <- hoistEither $ note "parameter parsing failed" genentry
    let newCid = cid entry
    when (T.null $ title entry) $ throwE "title missing"
    when (T.null $ homepage entry) $ throwE "homepage missing"
    when (T.null $ urlBase entry) $ throwE "url_base missing"
    withExceptT (T.pack . show) $ insertComic entry
    when (acceptbanner entry) $ withExceptT (T.pack . show) $
      ExceptT $ saveFromSubmit (fromIntegral $ sid entry) (fromIntegral newCid)
    withExceptT (T.pack . show) $ do
      deleteSubmit entry
      refreshAlphabet
      commit
      comicTitles
    when (wantsEmail entry) $ do
      let msg = T.replace "__NEWCID__" (T.pack $ show $ newCid)
                (emailMessage entry)
          mail = simpleMail (Address Nothing "piperka@piperka.net")
                 [Address Nothing (email entry)] [] []
                 (emailSubject entry)
                 [plainTextPart $ L.fromStrict msg]
      liftIO $ sendMail "localhost" mail
    return newCid
