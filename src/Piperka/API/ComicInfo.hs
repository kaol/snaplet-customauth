{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS -Wno-unused-top-binds #-}

module Piperka.API.ComicInfo (comicInfo) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session hiding (run, sql)
import Snap
import Snap.Snaplet.Hasql

import Application
import Piperka.API.Common
import Piperka.Util (getParamInt)

data ComicInfo = ComicInfo {
    title :: Text
  , readers :: Int
  , page_count :: Int
  , fragment_count :: Int
  , homepage :: Text
  , added_on :: Maybe UTCTime
  , description :: Text
  , banner :: Maybe Text
  , url_base :: Text
  , url_tail :: Text
  , first_page :: Maybe Text
  , last_page :: Maybe Text
  , have_fixed_head :: Maybe Bool
  , tags :: Vector Int
  , epedias :: Vector (Int, Text)
  } deriving (Generic)

instance ToJSON ComicInfo where
  toEncoding = genericToEncoding defaultOptions

type ComicInfoSeed = Vector (Int, Text)
                     -> ComicInfo

decodeComicInfoMain :: DE.Row ComicInfoSeed
decodeComicInfoMain =
  ComicInfo
  <$> DE.value DE.text
  <*> (liftA fromIntegral $ DE.value DE.int4)
  <*> (liftA fromIntegral $ DE.value DE.int4)
  <*> (liftA fromIntegral $ DE.value DE.int4)
  <*> DE.value DE.text
  <*> (liftA (fmap $ localTimeToUTC utc) $ DE.nullableValue DE.timestamp)
  <*> DE.value DE.text
  <*> DE.nullableValue DE.text
  <*> DE.value DE.text
  <*> DE.value DE.text
  <*> DE.nullableValue DE.text
  <*> DE.nullableValue DE.text
  <*> DE.nullableValue DE.bool
  <*> (liftA (V.map fromIntegral) $
       DE.value (DE.array $ DE.arrayDimension V.replicateM $ DE.arrayValue DE.int4))

decodeEpedias :: DE.Row (Int, Text)
decodeEpedias =
  (,)
  <$> (liftA fromIntegral $ DE.value DE.int4)
  <*> DE.value DE.text

comicInfo :: AppHandler ()
comicInfo =
  (fmap snd <$> getParamInt "cid") >>=
  (maybe (simpleFail 404 "Required parameter cid missing") $
   \c -> runQueries $ do
     infoMain' <- ExceptT $ run $ query (fromIntegral c) $
                  statement sql
                  (EN.value EN.int4) (DE.maybeRow decodeComicInfoMain) True
     maybe (lift $ modifyResponse $ setResponseStatus 404 "No such comic")
       (\infoMain ->
           (ExceptT $ run $ query (fromIntegral c) $
            statement sql'
            (EN.value EN.int4) (DE.rowsVector decodeEpedias) True) >>=
           lift . writeLBS . encode . infoMain
       ) infoMain')
  where
    sql = "SELECT title, readers, max_ord_of(cid)+1 AS page_count, \
          \(SELECT COUNT(*) FROM page_fragments WHERE cid=c.cid) AS fragment_count, \
          \homepage, added_on, description, banners.file AS banner, \
          \url_base, url_tail, \
          \(SELECT name FROM updates WHERE cid=c.cid AND ord=0 LIMIT 1) AS first_page, \
          \(SELECT name FROM updates WHERE cid=c.cid AND \
          \ name IS NOT NULL ORDER BY ord DESC LIMIT 1) AS last_page, \
          \(SELECT name IS NULL FROM updates WHERE cid=c.cid \
          \ ORDER BY ord DESC LIMIT 1) AS have_fixed_head, \
          \(SELECT COALESCE(array_agg(tagid), '{}') FROM comic_tag WHERE cid=c.cid) AS tags \
          \FROM comics AS c LEFT JOIN banners USING (cid) WHERE cid=$1"
    sql' = "SELECT epid, entry FROM external_entry WHERE cid=$1"
