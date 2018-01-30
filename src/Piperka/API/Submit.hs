{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Piperka.API.Submit (receiveSubmit) where

import Contravariant.Extras.Contrazip
import Control.Error.Util
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Functor.Contravariant
import Data.Int
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session hiding (run, sql)
import qualified HTMLEntities.Text as HTML
import Network.IP.Addr
import Snap
import Snap.Snaplet.Hasql

import Application
import Piperka.API.Common
import Piperka.API.Submit.Banner
import Piperka.Util (getCid)

receiveSubmit
  :: AppHandler ()
receiveSubmit = do
  formType <- getParam "formtype"
  case formType of
    Just "submit" -> handleSubmit
    Just "editinfo" -> handleEditInfo
    _ -> simpleFail 404 "Required parameter formtype invalid or missing"

data SubmitValidation =
    TitleOrUrl
  | ComicExists (Text, Text)
  | Database Error

validationMsg
  :: SubmitValidation
  -> Text
validationMsg TitleOrUrl =
  "<h2>Can't do that</h2>\
  \<p>I'd like to see both a title and the home page URL for your submission.</p>"
validationMsg (ComicExists (c,t)) =
  "<h2>That looks familiar<h2>\
  \<p>There is already a comic named as <a href='info.html?cid="
  <>c<>"'>"<>t<>
  "listed on Piperka.</p>\
  \<p>The comic has not been submitted.  If you think that there's been some error \
  \or your comic just happens to have the same name as an existing comic listed on \
  \Piperka, then please send me an email about it.  Sorry for the inconvenience."
validationMsg (Database e) =
  "<h2>Database error</h2>\
  \<p>An error occurred when trying to check the submission: "
  <>(HTML.text $ T.pack $ show e)<>"</p>"

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
  -> Maybe (NetAddr IP)
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
   (EN.nullableValue EN.inet)
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

handleSubmit
  :: AppHandler ()
handleSubmit = do
  params <- getParams
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
          Nothing  -- TODO remote IP
          ((Just ["1"]) == (M.lookup "want_notify" params))
          (lookupText "email")
          title
          url
          (uid <$> u)
        insertTagsEpedias sid params
        b <- lift receiveBanner
        maybe (lift $ return ())
          (either (const $ lift $ return ()) (ExceptT . saveBanner sid)) b
        lift $ writeLBS $ encode $ object $
          [ "ok" .= True
          , "msg" .= ("Your submission has been accepted and queued." :: Text)
          ]
  either (\m -> writeLBS $ encode $ object
                ["msg" .= validationMsg m]) submit validation

existCheck'
  :: Int32
  -> AppHandler (Either Error Bool)
existCheck' c = run $ query c $
  statement sql (EN.value EN.int4)
  (DE.singleRow $ DE.value DE.bool) True
  where
    sql = "SELECT $1 IN (SELECT cid FROM comics)"

data EditValidation = NoComic | Database2 Error

validationMsg'
  :: EditValidation
  -> Text
validationMsg' NoComic = "No such comic."
validationMsg' (Database2 e) =
  "<h2>Database error</h2>\
  \<p>An error occurred when trying to check the submission: "
  <>(HTML.text $ T.pack $ show e)<>"</p>"

insertEdit
  :: Maybe UserID
  -> Int32
  -> Maybe (NetAddr IP)
  -> Maybe Text
  -> AppHandler (Either Error Int32)
insertEdit u c ip desc = run $ query (u, c, ip, desc) $ statement sql
  (contrazip4
   (EN.nullableValue EN.int4)
   (EN.value EN.int4)
   (EN.nullableValue EN.inet)
   (EN.nullableValue EN.text))
  (DE.singleRow $ DE.value DE.int4) True
  where
    sql = "INSERT INTO user_edit"

handleEditInfo
  :: AppHandler ()
handleEditInfo = do
  params <- getParams
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
          Nothing -- TODO remote IP
          (lookupText "description")
        insertTagsEpedias sid params
--        when ((moderator <$> u) == (Just True)) saveEdit
        lift $ writeLBS $ encode $ object $
          [ "ok" .= True
          , "msg" .=
            ("Your submission has been sent to our \
             \moderators for consideration.\
             \<a href='/info.html?cid="<>(T.pack $ show c)<>
             "'>Return to the comic's info page</a>.")]
  either (\m -> writeLBS $ encode $ object
                ["msg" .= validationMsg' m]) submit validation

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
