{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Piperka.ComicInfo.Epedia
  ( eEntries
  , Epedia(..)
  , decodeEpedia
  ) where

import Control.Error.Util
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Int
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics
import qualified Hasql.Decoders as DE
import Snap.Core (Params)

eEntries
  :: Params
  -> [(Int16, Text)]
eEntries params = catMaybes $
  map (\n -> buildEntry n . listToMaybe =<<
             M.lookup ("epedia-entry-"<>n) params) epedias
  where
    buildEntry n e = do
      i <- fromIntegral . fst <$> B.readInt n
      e' <- T.strip <$> (hush . decodeUtf8' =<< e)
      guard $ not $ T.null e'
      return (i,e')
    epedias = maybe [] id $ M.lookup "epedia" params

data Epedia = Epedia
  { epid :: Int
  , entry :: Maybe Text
  } deriving (Generic)

instance ToJSON Epedia

decodeEpedia
  :: DE.Row Epedia
decodeEpedia =
  Epedia
  <$> (fromIntegral <$> DE.value DE.int4)
  <*> DE.nullableValue DE.text
