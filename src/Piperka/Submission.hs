{-# LANGUAGE OverloadedStrings #-}

module Piperka.Submission (renderSubmissions) where

import Control.Applicative
import Control.Monad.Trans
import Data.Map.Syntax
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Textual
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Vector (Vector)
import Hasql.Query
import Hasql.Session (query, Error)
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Heist.Compiled
import Heist.Compiled.Extra (eitherDeferMap)
import qualified HTMLEntities.Text as HTML
import Network.IP.Addr
import Snap.Snaplet.Hasql

import Application
import Piperka.Error.Splices

data Submission = Submission
  { sid :: Int
  , title :: Text
  , submittedOn :: UTCTime
  , fromIP :: NetAddr IP
  , name :: Maybe Text
  }
  deriving (Show)

decodeSubmission
  :: DE.Row Submission
decodeSubmission =
  Submission
  <$> (fromIntegral <$> DE.value DE.int4)
  <*> liftA HTML.text (DE.value DE.text)
  <*> liftA (localTimeToUTC utc) (DE.value DE.timestamp)
  <*> DE.value DE.inet
  <*> DE.nullableValue DE.text

readSubmissions
  :: AppHandler (Either Error (Vector Submission))
readSubmissions = run $ query () $ statement sql EN.unit
  (DE.rowsVector decodeSubmission) True
  where
    sql = "SELECT sid, title, submitted_on, from_ip, name \
          \FROM submit LEFT JOIN users USING (uid) ORDER BY sid DESC"

renderSubmissions
  :: Splice AppHandler
renderSubmissions =
  eitherDeferMap return stdSqlErrorSplice
  (manyWithSplices runChildren splices) $ lift readSubmissions
  where
    splices = mapV (pureSplice . textSplice) $ do
      "sid" ## T.pack . show . sid
      "title" ## title
      "submittedOn" ## T.pack . show . submittedOn
      "fromIP" ## Data.Textual.toText . netHost . fromIP
      "name" ## maybe "" id . name
