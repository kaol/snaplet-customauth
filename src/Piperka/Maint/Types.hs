module Piperka.Maint.Types where

import Data.Int
import Data.Text (Text)

data Genentry = Genentry
  { sid :: Int
  , cid :: Int
  , title :: Text
  , homepage :: Text
  , fixedHead :: Maybe Text
  , urlBase :: Text
  , urlTail :: Text
  , acceptbanner :: Bool
  , description :: Text
  , email :: Text
  , wantsEmail :: Bool
  , emailSubject :: Text
  , emailMessage :: Text
  , bookmarkRegexp :: Maybe Text
  , parserType :: Int32
  , extraData :: Maybe Text
  , extraUrl :: Maybe Text
  , tags :: [Int16]
  , epedias :: [(Int16, Text)]
  } deriving (Show)
