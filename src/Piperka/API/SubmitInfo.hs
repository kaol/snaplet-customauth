{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS -Wno-unused-top-binds #-}

module Piperka.API.SubmitInfo (
    readSubmit
  , dropSubmit
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

data UserEdit = UserEdit {
    sid :: Int
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

instance ToJSON UserEdit where
  toEncoding = genericToEncoding defaultOptions

decodeUserEdit
  :: DE.Row UserEdit
decodeUserEdit =
  UserEdit
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
     statement sql (EN.value EN.int4) (DE.maybeRow decodeUserEdit) False)
  where
    sql = "SELECT sid, title, homepage, first_page, description, submitted_on, \
          \name AS user, want_email, submit.email, banner_url, from_ip, \
          \sid in (SELECT sid FROM submit_banner) AS newbanner \
          \FROM submit LEFT JOIN users USING (uid) WHERE sid=$1"

dropSubmit
  :: AppHandler ()
dropSubmit = do
  s <- requiredParam "sid" $ \n -> fmap snd <$> getParamInt n
  runModQueries $ const $ do
    lift $ validateCsrf
    ExceptT $ run $ query (fromIntegral s) $
      statement sql (EN.value EN.int4) DE.unit False
    lift $ writeLBS $ encode $ object [ "ok" .= True]
  where
    sql = "DELEETE FROM submit WHERE sid=$1"

-- Storing data files in a database is generally a bad idea but I went
-- with it for the little used moderator interface.
viewSubmitBanner
  :: AppHandler ()
viewSubmitBanner = do
  s <- requiredParam "sid" $ \n -> fmap snd <$> getParamInt n
  b <- runModQueries $ const $ do
    b <- ExceptT $ run $ query (fromIntegral s) $
         statement sql (EN.value EN.int4)
         (DE.maybeRow $ DE.value DE.bytea) False
    maybe (lift $ simpleFail 404 "No banner for that submission") return b
  (Just hIn, Just hOut, _, h) <- liftIO $ createProcess $
    (System.Process.shell "identify -verbose -"){ std_out = CreatePipe
                                                , std_in = CreatePipe
                                                }
  liftIO $ forkIO $ finally (B.hPut hIn b) $ hClose hIn
  let getMime = do
        line <- B.hGetLine hOut
        if B.null line then return Nothing else do
          let (x,x') = B.splitAt 13 line
          if x == "  Mime type: " then return $ Just x' else getMime
  mime <- liftIO $ finally getMime $ hClose hOut
  err <- liftIO $ waitForProcess h
  maybe (simpleFail 500 "Error decoding MIME type")
    (modifyResponse . setHeader "Content-Type")
    (guard (case err of
              ExitFailure _ -> False
              ExitSuccess -> True) >> mime)
  writeBS b
  where
    sql = "SELECT banner FROM submit_banner WHERE sid=$1"
