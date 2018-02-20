{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Piperka.Action.Statements where

import Contravariant.Extras.Contrazip
import Control.Applicative
import Control.Arrow
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.Query
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Network.IP.Addr

import Piperka.Util (monadicTuple3)

comicTitleFetch
  :: Query Int32 (Maybe Text)
comicTitleFetch =
  statement sql (EN.value EN.int4) (DE.maybeRow $ DE.value DE.text) True
  where
    sql = "SELECT title FROM comics WHERE cid=$1"

decodeBookmark
  :: DE.Row (Int, Text, Maybe (Int, Int, Bool))
decodeBookmark =
  (,,)
  <$> (liftA fromIntegral $ DE.value DE.int4)
  <*> DE.value DE.text
  <*> liftA monadicTuple3 ((,,)
                           <$> (liftA (fmap fromIntegral) $
                                DE.nullableValue DE.int4)
                           <*> (liftA (fmap fromIntegral) $
                                DE.nullableValue DE.int4)
                           <*> (DE.nullableValue DE.bool))

decodeUnreadStats
  :: DE.Row (Int32, Int32)
decodeUnreadStats =
  (,)
  <$> DE.value DE.int4
  <*> DE.value DE.int4

getUnreadStats
  :: Query Int32 (Int32, Int32)
getUnreadStats =
  statement sql (EN.value EN.int4) (DE.singleRow decodeUnreadStats) True
  where
    sql = "SELECT COALESCE(SUM(Num), 0), COUNT(*) FROM comic_remain_frag($1) \
          \JOIN comics USING (cid) WHERE num > 0"

bookmarkFetch
  :: Query (Text, Bool) [(Int, Text, Maybe (Int, Int, Bool))]
bookmarkFetch =
  statement sql (contrazip2 (EN.value EN.text) (EN.value EN.bool))
  (DE.rowsList decodeBookmark) True
  where
    sql = "SELECT cid, title, ord, subord, at_max \
          \FROM bookmark($1, $2) AS b JOIN comics USING (cid)"

-- url, want_here, host, uid
bookmarkAndLogFetch
  :: Query (Text, Bool, NetAddr IP, Maybe Int32) [(Int, Text, Maybe (Int, Int, Bool))]
bookmarkAndLogFetch =
  statement sql (contrazip4 (EN.value EN.text) (EN.value EN.bool)
                 (EN.value EN.inet) (EN.nullableValue EN.int4))
  (DE.rowsList decodeBookmark) True
  where
    sql = "SELECT b.v_cid, title, b.v_ord, b.v_subord, b.v_at_max \
          \FROM bookmark_and_log($1, $2, $3, $4) AS b \
          \JOIN comics ON (b.v_cid = cid)"

bookmarkSet
  :: Query (Int32, Int32, Int32, Int32) (Int32, Int32)
bookmarkSet =
  statement sql (contrazip4 (EN.value EN.int4) (EN.value EN.int4)
                 (EN.value EN.int4) (EN.value EN.int4))
  (DE.singleRow decodeUnreadStats) True
-- uid, cid, ord, subord
  where
    sql = "SELECT * FROM set_bookmark($1, $2, $3, $4)"

subscribeSet
  :: Query (Int32, Int32, Bool) (Int32, Int32)
subscribeSet =
  statement sql (contrazip3 (EN.value EN.int4) (EN.value EN.int4)
                 (EN.value EN.bool))
  (DE.singleRow decodeUnreadStats) True
  where
    sql = "SELECT * FROM set_bookmark($1, $2, $3)"

unsubscribeSet
  :: Query (Int32, Int32) (Int32, Int32)
unsubscribeSet =
  statement sql (contrazip2 (EN.value EN.int4) (EN.value EN.int4))
  (DE.singleRow decodeUnreadStats) True
  where
    sql = "SELECT * FROM unset_bookmark($1, $2)"

revertUpdatesSet
  :: Query (Int32, Vector Int32) (Int32, Int32)
revertUpdatesSet = proc params -> do
  applyReverts -< params
  deleteReverts -< params
  uid <- arr fst -< params
  getUnreadStats -< uid
  where
    encoder = (contrazip2 (EN.value EN.int4)
                (EN.value $ EN.array (EN.arrayDimension V.foldl' $
                                      EN.arrayValue EN.int4)))
    applyReverts = statement sql1 encoder DE.unit True
    deleteReverts = statement sql2 encoder DE.unit True
    sql1 = "UPDATE subscriptions SET ord=\
           \COALESCE((SELECT ord FROM recent WHERE \
           \uid=subscriptions.uid and cid=subscriptions.cid ), 0), \
           \subord=COALESCE((SELECT subord FROM recent WHERE \
           \uid=subscriptions.uid and cid=subscriptions.cid), 0) \
           \WHERE subscriptions.uid=$1 AND \
           \subscriptions.cid = ANY ($2 :: int[])"
    sql2 = "DELETE FROM recent WHERE uid = $1 and cid = ANY ($2 :: int[])"

titleFetch
  :: Query Int32 (Maybe Text)
titleFetch =
  statement sql (EN.value EN.int4) (DE.maybeRow (DE.value DE.text)) True
  where
    sql = "SELECT title FROM comics WHERE cid=$1"
