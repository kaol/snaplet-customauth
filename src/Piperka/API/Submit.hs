{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Piperka.API.Submit (receiveSubmit) where

import Contravariant.Extras.Contrazip
import Control.Applicative ((<|>))
import Control.Error.Util
import Control.Lens (set)
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Aeson hiding (Success)
import Data.Binary.Builder (putStringUtf8, toLazyByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import Data.Functor.Contravariant
import Data.Int
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Textual (fromString)
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session hiding (run, sql)
import Heist.Compiled
import qualified HTMLEntities.Text as HTML
import Network.IP.Addr
import Snap
import Snap.Snaplet.Hasql
import Snap.Snaplet.Heist

import Application
import Piperka.API.Common
import Piperka.API.Submit.Banner
import Piperka.API.Submit.Types
import Piperka.Util (getCid, getParamInt)

rqRemote
  :: Request
  -> NetAddr IP
rqRemote rq = let
  client = rqClientAddr rq
  addr = B.unpack $ client
  isLocal = B.take 4 client == "127."
  forwarded = B.unpack <$> getHeader "X-Proxy-Forward" rq
  parse a =
    (((flip netAddr 128 . IPv6) <$> (fromString a :: Maybe IP6)) <|>
     ((flip netAddr 32 . IPv4) <$> (fromString a :: Maybe IP4))) :: Maybe (NetAddr IP)
  in fromJust $
     (if isLocal then maybe id (\x -> (parse x <|>)) forwarded else id) $
     parse addr

receiveSubmit
  :: AppHandler ()
receiveSubmit = do
  b <- receiveBanner
  formType <- getPostParam "formtype"
  let handle b' = case formType of
        Just "submit" -> handleSubmit b'
        Just "editinfo" -> handleEditInfo b'
        _ -> returnMessage InvalidFormType
  maybe (handle Nothing)
    (either (returnMessage . BValidation) (handle . Just)) b

existCheck
  :: Text
  -> ExceptT SubmitValidation AppHandler ()
existCheck t = (maybe (return ()) (throwE . ComicExists)) =<<
  (withExceptT Database $
   ExceptT $ run $ query t $ statement sql
   (EN.value EN.text)
   (DE.maybeRow ((,)
                  <$> (T.pack . show <$> DE.value DE.int4)
                  <*> (HTML.text <$> DE.value DE.text))) True)
  where
    sql = "SELECT cid, title FROM comics WHERE lower(title)=lower($1)"

insertSubmit
  :: Maybe Text
  -> Maybe Text
  -> NetAddr IP
  -> Bool
  -> Maybe Text
  -> Text
  -> Text
  -> Maybe UserID
  -> AppHandler (Either Error Int32)
insertSubmit first desc ip wantEmail email title home u =
  run $ query (first, desc, ip, wantEmail, email, title, home, u) $ statement sql
  (contrazip8
   (EN.nullableValue EN.text)
   (EN.nullableValue EN.text)
   (EN.value EN.inet)
   (EN.value EN.bool)
   (EN.nullableValue EN.text)
   (EN.value EN.text)
   (EN.value EN.text)
   (EN.nullableValue EN.int4))
  (DE.singleRow $ DE.value DE.int4) True
  where
    sql = "INSERT INTO submit (first_page, description, from_ip, \
          \want_email, email, title, homepage, uid) VALUES \
          \($1,$2,$3,$4,$5,$6,$7,$8) RETURNING sid"

returnMessage
  :: SubmitResult
  -> AppHandler ()
returnMessage status = do
  st <- getHeistState
  modify $ set submitResult $ Just status
  msg <- (decodeUtf8 . toStrict . toLazyByteString) <$>
    (maybe (return $ putStringUtf8 $ show status) fst $
     renderTemplate st "submitMessage_")
  let isSuccess = isSubmitSuccess status
      msgField = if isSuccess then "msg" else "errmsg"
      addOk = if isSuccess then ("ok" .= True :) else id
  writeLBS $ encode $ object $ addOk $ [msgField .= msg]

handleSubmit
  :: Maybe Banner
  -> AppHandler ()
handleSubmit b = do
  params <- getParams
  remote <- rqRemote <$> getRequest
  let lookupText n = (hush . decodeUtf8' =<<) . listToMaybe =<< M.lookup n params
  validation <- runExceptT $ do
    [title, url] <- hoistEither $ do
      xs <- forM ["title", "url"]
        (maybe (throwError TitleOrUrl) (return . T.strip) . lookupText)
      when (any T.null xs) $ throwError TitleOrUrl
      return xs
    existCheck title
    return (title, url)
  let submit (title, url) = runMaybeUserQueries $ \u -> do
        sid <- ExceptT $ insertSubmit
          (lookupText "first_page")
          (lookupText "description")
          remote
          ((Just ["1"]) == (M.lookup "want_notify" params))
          (lookupText "email")
          title
          url
          (uid <$> u)
        maybe (return Nothing)
          (\b' -> (ExceptT $ submitBanner sid b') >> return Nothing) b
        insertTagsEpedias sid params
        return (Success SubmissionAccepted)
  returnMessage =<< either (return . SValidation) submit validation

existCheck'
  :: Int32
  -> AppHandler (Either Error Bool)
existCheck' c = run $ query c $
  statement sql (EN.value EN.int4)
  (DE.singleRow $ DE.value DE.bool) True
  where
    sql = "SELECT $1 IN (SELECT cid FROM comics)"

insertEdit
  :: Maybe UserID
  -> Int32
  -> NetAddr IP
  -> Maybe Text
  -> AppHandler (Either Error Int32)
insertEdit u c ip desc = run $ query (u, c, ip, desc) $ statement sql
  (contrazip4
   (EN.nullableValue EN.int4)
   (EN.value EN.int4)
   (EN.value EN.inet)
   (EN.nullableValue EN.text))
  (DE.singleRow $ DE.value DE.int4) True
  where
    sql = "INSERT INTO user_edit (uid, cid, from_ip, description) VALUES \
          \($1, $2, $3, $4) RETURNING sid"

handleEditInfo
  :: Maybe Banner
  -> AppHandler ()
handleEditInfo b = do
  params <- getParams
  remote <- rqRemote <$> getRequest
  let lookupText n = (hush . decodeUtf8' =<<) . listToMaybe =<< M.lookup n params
  validation <- runExceptT $ do
    c <- maybe (throwE NoComic) (return . fromIntegral . snd) =<< lift getCid
    either (throwE . Database2) (bool (throwE NoComic) (return ())) =<<
      (lift $ existCheck' c)
    return c
  let submit c = runMaybeUserQueries $ \u -> do
        sid <- ExceptT $ insertEdit
          (uid <$> u)
          c
          remote
          (lookupText "description")
        maybe (return ()) (ExceptT . submitBanner sid) b
        insertTagsEpedias sid params
        let editSuccess = return $ Success (EditSubmitted c)
        maybe editSuccess
          (\u' -> bool editSuccess
                  (saveEdit u' c sid)
                  (moderator u')) u
  returnMessage =<< either (return . EValidation) submit validation

saveEdit
  :: MyData
  -> Int32
  -> Int32
  -> ExceptT Error AppHandler SubmitResult
saveEdit u c sid = do
  userSid <- lift $ (fromIntegral . snd <$>) <$> getParamInt "usersid"
  maybe ( return ())
    (\sid' -> do
        acceptBanner <- lift $ (== (Just "1")) <$> getParam "acceptbanner"
        when (not acceptBanner) $
          ExceptT (deleteBanner c) >> ExceptT (saveFromSubmit sid' c)
        ExceptT $ run $ query sid' $ statement
          "DELETE FROM user_edit WHERE sid=$1"
          (EN.value EN.int4) DE.unit True) userSid
  ExceptT $ run $ query (uid u, c, sid) $ statement
    "SELECT edit_entry($1, $2, $3)"
    (let e = EN.value EN.int4 in contrazip3 e e e)
    DE.unit True
  return $ Success (EditedSuccessfully c)

insertTagsEpedias
  :: Int32
  -> Params
  -> ExceptT Error AppHandler ()
insertTagsEpedias sid params = do
  let encodeArray = EN.value . EN.array . EN.arrayDimension foldl . EN.arrayValue
  ExceptT $ run $ query (sid, tags) $ statement sql1
    (contrazip2 (EN.value EN.int4) (encodeArray EN.int4))
    DE.unit True
  ExceptT $ run $ query (sid, eEntries) $ statement sql2
    (contrazip2 (EN.value EN.int4)
     (unzip >$< contrazip2 (encodeArray EN.int4) (encodeArray EN.text)))
    DE.unit True
  where
    tags = maybe [] (map (fromIntegral . fst) . catMaybes . map B.readInt) $
      M.lookup "category" params
    epedias = maybe [] id $ M.lookup "epedia" params
    buildEntry n entry = do
      i <- fromIntegral . fst <$> B.readInt n
      (i,) <$> (hush . decodeUtf8' =<< entry)
    eEntries = catMaybes $
      map (\n -> buildEntry n . listToMaybe =<<
            M.lookup ("epedia-entry-"<>n) params) epedias
    sql1 = "INSERT INTO submit_tag (sid, tagid) SELECT $1, tagid FROM \
           \unnest($2) AS tagid"
    sql2 = "INSERT INTO external_entry_submit (sid, epid, entry) \
           \SELECT $1, epid, entry FROM unnest($2, $3) AS u (epid, entry)"
