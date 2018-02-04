{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS -Wno-unused-top-binds #-}

module Piperka.API.SubmitInfo (
    readSubmit
  , readUserEdit
  , dropUserEdit
  , viewSubmitBanner
  ) where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Text (Text)
import GHC.Generics
import Hasql.Decoders as DE
import Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session hiding (run, sql)
import Network.IP.Addr
import Snap
import Snap.Snaplet.Hasql
import System.Exit
import System.IO
import System.Process

import Application
import Piperka.API.Common
import Piperka.Util (getParamInt)

data UserSubmit = UserSubmit
  { sid :: Int
  , title :: Text
  , homepage :: Text
  , first_page :: Maybe Text
  , description :: Maybe Text
  , submitted_on :: UTCTime
  , user :: Maybe Text
  , want_email :: Bool
  , email :: Maybe Text
  , banner_url :: Maybe Text
  , from_ip :: Maybe (NetAddr IP)
  , newbanner :: Bool
  } deriving (Generic)

instance ToJSON UserSubmit where
  toEncoding = genericToEncoding defaultOptions

decodeSubmit
  :: DE.Row UserSubmit
decodeSubmit =
  UserSubmit
  <$> (liftA fromIntegral $ DE.value DE.int4)
  <*> DE.value DE.text
  <*> DE.value DE.text
  <*> DE.nullableValue DE.text
  <*> DE.nullableValue DE.text
  <*> (liftA (localTimeToUTC utc) $ DE.value DE.timestamp)
  <*> DE.nullableValue DE.text
  <*> DE.value DE.bool
  <*> DE.nullableValue DE.text
  <*> DE.nullableValue DE.text
  <*> DE.nullableValue DE.inet
  <*> DE.value DE.bool

readSubmit
  :: AppHandler ()
readSubmit = do
  s <- maybe (simpleFail 404 "Required parameter sid missing") (return . snd) =<<
       getParamInt "sid"
  writeLBS . encode =<<
    (runModQueries $ const $
     ExceptT $ run $ query (fromIntegral s) $
     statement sql (EN.value EN.int4) (DE.maybeRow decodeSubmit) False)
  where
    sql = "SELECT sid, title, homepage, first_page, description, submitted_on, \
          \name AS user, want_email, submit.email, banner_url, from_ip, \
          \sid in (SELECT sid FROM submit_banner) AS newbanner \
          \FROM submit LEFT JOIN users USING (uid) WHERE sid=$1"

data UserEdit = UserEdit
  { description' :: Maybe Text
  , oldbanner :: Maybe Text
  , newbanner' :: Bool
  , ok :: Bool
  , tags :: [Int]
  , epedias :: [Epedia]
  , origtags :: [Int]
  } deriving (Generic)

instance ToJSON UserEdit where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = filter (/= '\'') }

data Epedia = Epedia
  { epid :: Int
  , entry :: Maybe Text
  } deriving (Generic)

instance ToJSON Epedia

decodeEdit
  :: DE.Row ([Int] -> [Epedia] -> [Int] -> UserEdit)
decodeEdit =
  UserEdit
  <$> DE.nullableValue DE.text
  <*> DE.nullableValue DE.text
  <*> DE.value DE.bool
  <*> pure True

readUserEdit
  :: AppHandler ()
readUserEdit = do
  s <- maybe (simpleFail 404 "Required parameter sid missing")
    (return . fromIntegral . snd) =<< getParamInt "sid"
  let run' sql d = ExceptT $ run $ query s $ statement sql
                   (EN.value EN.int4) d True
  writeLBS . encode =<<
    (runModQueries $ const $ do
        info <- run' sql1 (DE.maybeRow decodeEdit)
        let fillInfo info' = do
              newtags <- run' sql2 (DE.rowsList (fromIntegral <$> DE.value DE.int4))
              epedias' <- run' sql3 $ DE.rowsList $
                         Epedia
                         <$> (fromIntegral <$> DE.value DE.int4)
                         <*> (DE.nullableValue DE.text)
              oldtags <- run' sql4 (DE.rowsList (fromIntegral <$> DE.value DE.int4))
              return $ info' newtags epedias' oldtags
        maybe (return Nothing) ((Just <$>) . fillInfo) info)
  where
    sql1 = "SELECT description, banner_url, sid IN (SELECT sid FROM submit_banner) \
           \FROM user_edit WHERE sid = $1"
    sql2 = "SELECT tagid FROM submit_tag WHERE sid=$1"
    sql3 = "SELECT epid, entry FROM external_entry_submit WHERE sid=$1"
    sql4 = "SELECT tagid FROM comic_tag JOIN user_edit USING (cid) \
           \WHERE sid=$1"

dropUserEdit
  :: AppHandler ()
dropUserEdit = do
  s <- requiredParam "sid" $ \n -> fmap snd <$> getParamInt n
  runModQueries $ const $ do
    lift $ validateCsrf
    ExceptT $ run $ query (fromIntegral s) $
      statement sql (EN.value EN.int4) DE.unit False
    lift $ writeLBS $ encode $ object [ "ok" .= True]
  where
    sql = "DELETE FROM user_edit WHERE sid=$1"

-- Storing data files in a database is generally a bad idea but I went
-- with it for the little used moderator interface.
viewSubmitBanner
  :: AppHandler ()
viewSubmitBanner = do
  s <- requiredParam "sid" $ \n -> fmap snd <$> getParamInt n
  (mime, b) <- runModQueries $ const $ do
    b <- ExceptT $ run $ query (fromIntegral s) $
         statement sql (EN.value EN.int4)
         (DE.maybeRow $
          ((,) <$> DE.nullableValue DE.bytea <*> DE.value DE.bytea)) True
    maybe (lift $ simpleFail 404 "No banner for that submission") return b
  modifyResponse . setHeader "Content-Type" =<< maybe (magickMime b)
    return mime
  writeBS b
  where
    sql = "SELECT mime, banner FROM submit_banner WHERE sid=$1"

magickMime
  :: ByteString
  -> AppHandler ByteString
magickMime b = do
  (Just hIn, Just hOut, _, h) <- liftIO $ createProcess $
    (System.Process.shell "identify -verbose -"){ std_out = CreatePipe
                                                , std_in = CreatePipe
                                                }
  let getMime = do
        line <- B.hGetLine hOut
        if B.null line then return Nothing else do
          let (x,x') = B.splitAt 13 line
          if x == "  Mime type: " then return $ Just x' else getMime
  (mime, err) <- liftIO $ (forkIO $ finally (B.hPut hIn b) $ hClose hIn) >> (,)
    <$> (finally getMime $ hClose hOut)
    <*> waitForProcess h
  maybe (simpleFail 500 "Error decoding MIME type") return
    (guard (case err of
              ExitFailure _ -> False
              ExitSuccess -> True) >> mime)
