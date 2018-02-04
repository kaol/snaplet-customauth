module Piperka.ComicInfo.Types where

import Data.Time.Clock

import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Session (Error)

-- Something to consider: This data has both entry specific data in it
-- as well as user specific data (publicReaders, subscribed).
-- Separate?

data ComicInfo = ComicInfo
  { cid :: Int
  , title :: Text
  , homepage :: Text
  , readers :: Int
  , publicReaders :: Bool
  , pageCount :: Int
  , fragmentCount :: Int
  , archivePages :: Maybe (Text, Text, Bool)
  , addedOn :: Maybe UTCTime
  , subscribed :: Maybe Bool
  , mapped :: Bool
  , dead :: Maybe (Text, UTCTime)
  , banner :: Maybe Text
  , description :: Text
  , tags :: [ComicTag]
  , crawlErrors :: Vector CrawlError
  , extern :: [ExternalEntry]
  }

data CrawlError = CrawlError
  { ord :: Int
  , time :: UTCTime
  , archiveUrl :: Text
  , code :: Int
  , msg :: Text
  }

data ExternalEntry = ExternalEntry
  { eDescription :: Text
  , epediaTag :: Text
  , epediaTagName :: Text
  , epid :: Int
  , base :: Text
  , urlPart :: Text
  } deriving (Show)

data ComicTag = ComicTag
  { tagid :: Int
  , tagName :: Text
  , tagDescription :: Maybe Text
  } deriving (Show)

data ComicInfoError = Missing | SqlError Error | FoundDead
