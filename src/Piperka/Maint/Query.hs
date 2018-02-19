{-# LANGUAGE OverloadedStrings #-}

module Piperka.Maint.Query where

import Contravariant.Extras.Contrazip
import Control.Monad.Trans.Except
import Data.Functor.Contravariant
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session (Error, query)
import Snap.Snaplet.Hasql

import Application
import Piperka.Maint.Types

nextCid
  :: AppHandler (Either Error Int)
nextCid = run $ query () $ statement sql EN.unit
          (DE.singleRow $ fromIntegral <$> DE.value DE.int4) True
  where
    sql = "SELECT max(cid)+1 FROM comics"

precrawlArchive
  :: AppHandler (Either Error (Vector (Maybe Text)))
precrawlArchive =
  run $ query () $ statement sql EN.unit
  (DE.rowsVector $ DE.nullableValue DE.text) True
  where
    sql = "SELECT name FROM updates \
          \WHERE cid=(SELECT max(cid)+1 FROM comics) ORDER BY ord"

insertComic
  :: Genentry
  -> ExceptT Error AppHandler ()
insertComic e =
  ExceptT $ run $ query e $ statement sql encode DE.unit True
  where
    encodeList = EN.value . EN.array . EN.arrayDimension foldl . EN.arrayValue
    encode = mconcat
      [ fromIntegral . cid >$< EN.value EN.int4
      , title >$< EN.value EN.text
      , homepage >$< EN.value EN.text
      , fixedHead >$< EN.nullableValue EN.text
      , urlBase >$< EN.value EN.text
      , urlTail >$< EN.value EN.text
      , description >$< EN.value EN.text
      , bookmarkRegexp >$< EN.nullableValue EN.text
      , parserType >$< EN.value EN.int4
      , extraData >$< EN.nullableValue EN.text
      , extraUrl >$< EN.nullableValue EN.text
      , tags >$< encodeList EN.int2
      , unzip . epedias >$< contrazip2 (encodeList EN.int2) (encodeList EN.text)
      ]
    sql = "WITH new_comic AS (\
          \INSERT INTO comics (cid, title, homepage, fixed_head, \
          \url_base, url_tail, description, bookmark_regexp) \
          \VALUES ($1, $2, $3, $4, $5, $6, $7, $8) RETURNING cid), \
          \new_crawler AS (\
          \INSERT INTO crawler_config (cid, parser_type, extra_data, extra_url) \
          \SELECT new_comic.cid, $9, $10, $11 FROM new_comic), \
          \new_tags AS (\
          \INSERT INTO comic_tag SELECT new_comic.cid, unnest($12) FROM new_comic), \
          \new_epedia AS (\
          \INSERT INTO external_entry \
          \SELECT new_comic.cid, epid, entry \
          \FROM new_comic, unnest ($13, $14) AS u (epid, entry)) \
          \SELECT new_comic.cid FROM new_comic"

deleteSubmit
  :: Genentry
  -> ExceptT Error AppHandler ()
deleteSubmit e =
  ExceptT $ run $ query e $ statement sql
  (fromIntegral . sid >$< EN.value EN.int4) DE.unit True
  where
    sql = "DELETE FROM submit WHERE sid=$1"
