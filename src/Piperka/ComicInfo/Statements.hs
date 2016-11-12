{-# LANGUAGE OverloadedStrings #-}

module Piperka.ComicInfo.Statements where

import Contravariant.Extras.Contrazip
import Control.Applicative
import Control.Monad
import Data.Time.LocalTime
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Data.Int
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Query

import Piperka.ComicInfo.Types

type ComicInfoSeed = Vector CrawlError
                     -> [ExternalEntry]
                     -> ComicInfo

decodeComicInfo :: ([Int] -> [ComicTag]) -> DE.Row ComicInfoSeed
decodeComicInfo taglookup =
  ComicInfo
  <$> (liftA fromIntegral $ DE.value DE.int4)
  <*> DE.value DE.text
  <*> DE.value DE.text
  <*> (liftA fromIntegral $ DE.value DE.int4)
  <*> DE.value DE.bool
  <*> (liftA fromIntegral $ DE.value DE.int4)
  <*> (liftA fromIntegral $ DE.value DE.int4)
  <*> liftA (\(a,b,c) -> do
                a' <- a; b' <- b; c' <- c
                return (a',b',c')) ((,,)
                                    <$> DE.nullableValue DE.text
                                    <*> DE.nullableValue DE.text
                                    <*> DE.nullableValue DE.bool)
  <*> (liftA (fmap $ localTimeToUTC utc) $ DE.nullableValue DE.timestamp)
  <*> DE.nullableValue DE.bool
  <*> DE.value DE.bool
  <*> DE.nullableValue (DE.composite ((,)
                                      <$> DE.compositeValue DE.text
                                      <*> (liftA (localTimeToUTC utc) $
                                           DE.compositeValue DE.timestamp)))
  <*> DE.nullableValue DE.text
  <*> (liftA (taglookup . map fromIntegral) $
       DE.value (DE.array $ DE.arrayDimension replicateM $ DE.arrayValue DE.int4))

decodeDeadComicInfo
  :: ([Int] -> [ComicTag])
  -> DE.Row ComicInfo
decodeDeadComicInfo taglookup =
  decodeComicInfo taglookup
  <*> pure mempty
  <*> pure mempty

decodeCrawlError :: DE.Row CrawlError
decodeCrawlError =
  CrawlError
  <$> (liftA fromIntegral $ DE.value DE.int4)
  <*> (liftA (localTimeToUTC utc) $ DE.value DE.timestamp)
  <*> DE.value DE.text
  <*> (liftA fromIntegral $ DE.value DE.int4)
  <*> DE.value DE.text

decodeExternalEntry
  :: (Int -> Text -> Maybe ExternalEntry)
  -> DE.Row (Maybe ExternalEntry)
decodeExternalEntry extlookup =
  extlookup
  <$> (liftA fromIntegral $ DE.value DE.int4)
  <*> DE.value DE.text

comicInfoFetch
  :: ([Int] -> [ComicTag])
  -> Query (Maybe Int32, Int32) (Maybe ComicInfoSeed)
comicInfoFetch taglookup =
  statement sql (contrazip2 (EN.nullableValue EN.int4) (EN.value EN.int4))
  (DE.maybeRow $ decodeComicInfo taglookup) True
  where
    sql = "SELECT cid, title, homepage, readers, \
          \cid IN (SELECT cid FROM subscriptions AS subs \
          \ LEFT JOIN (SELECT cid, interest AS uid, interest \
          \ FROM permitted_interest WHERE uid=u.uid) AS interest USING (cid, uid) \
          \WHERE (countme AND privacy=3) OR interest IS NOT NULL) as public_readers, \
          \max_ord_of(cid)+1, (SELECT COUNT(*) FROM page_fragments WHERE cid=c.cid), \
          \(SELECT url_base||name||url_tail FROM updates WHERE cid=c.cid AND ord=0 LIMIT 1), \
          \(SELECT url_base||name||url_tail FROM updates WHERE cid=c.cid AND name IS NOT NULL ORDER BY ord DESC LIMIT 1), \
          \(SELECT name IS NULL FROM updates WHERE cid=c.cid ORDER BY ord DESC LIMIT 1), \
          \added_on, EXISTS (SELECT uid FROM subscriptions WHERE cid=c.cid AND uid=u.uid) AS subscribed, \
          \mapped, null AS dead, file AS banner, \
          \COALESCE((SELECT array_agg(tagid) FROM comic_tag WHERE cid=c.cid), '{}') AS tags \
          \FROM comics AS c LEFT JOIN banners USING (cid) \
          \LEFT JOIN users AS u ON (u.uid=$1) WHERE cid=$2"

deadInfoFetch
  :: ([Int] -> [ComicTag])
  -> Query (Maybe Int32, Int32) (Maybe ComicInfo)
deadInfoFetch taglookup =
  statement sql (contrazip2 (EN.nullableValue EN.int4) (EN.value EN.int4))
  (DE.maybeRow $ decodeDeadComicInfo taglookup) True
  where
    sql = "SELECT cid, title, homepage, 0 AS readers, false AS public_readers, \
          \max_ord_of(cid)+1, (SELECT count(*) FROM page_fragment WHERE cid=c.cid), \
          \(SELECT url_base||name||url_tail FROM updates WHERE cid=c.cid AND ord=0 LIMIT 1), \
          \(SELECT url_base||name||url_tail FROM updates WHERE cid=c.cid AND name IS NOT NULL ORDER BY ord DESC LIMIT 1), \
          \(SELECT name IS NULL FROM updates WHERE cid=c.cid ORDER BY ord DESC LIMIT 1), \
          \added_on, EXISTS (SELECT true FROM subscriptions WHERE cid=c.cid AND uid=u.uid) AS subscribed, \
          \false, (reason, removed_on), file AS banner, \
          \COALESCE((SELECT array_agg(tagid) FROM comic_tag WHERE cid=c.cid), '{}') AS tags \
          \FROM graveyard AS c LEFT JOIN banners USING (cid) \
          \LEFT JOIN users AS u ON (u.uid=$1) WHERE c.cid=$2"

crawlErrorFetch
  :: Query Int32 (Vector CrawlError)
crawlErrorFetch =
  statement sql (EN.value EN.int4) (DE.rowsVector decodeCrawlError) True
  where
    sql = "SELECT ord, stamp, url, http_code, http_message \
           \FROM crawl_error WHERE cid=$1 AND stamp > date 'now' - integer '30' \
           \ORDER BY errid DESC LIMIT 5"

externalEntryFetch
  :: (Int -> Text -> Maybe ExternalEntry)
  -> Query Int32 [Maybe ExternalEntry]
externalEntryFetch extlookup =
  statement sql (EN.value EN.int4)
  (DE.rowsList $ decodeExternalEntry extlookup) True
  where
    sql = "SELECT epid, entry FROM external_entry WHERE cid=$1 ORDER BY epid"

isComicDeadFetch
  :: Query Int32 Bool
isComicDeadFetch =
  statement sql (EN.value EN.int4) (DE.singleRow $ DE.value DE.bool) True
  where
    sql = "SELECT EXISTS (SELECT true FROM graveyard WHERE cid=$1)"
