{-# LANGUAGE OverloadedStrings #-}

module Piperka.Listing.Statements where

import Piperka.Listing.Types
import Piperka.Listing.Types.Ordering

import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Contravariant.Extras.Contrazip
import Data.ByteString (ByteString)
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector hiding ((++), map)
import Hasql.Query
import Prelude hiding (Ordering)

parseOrdering :: (IsString a, Eq a) => a -> Ordering
parseOrdering "new" = NewDesc
parseOrdering "update" = UpdateDesc
parseOrdering "top" = TopDesc
parseOrdering _ = TitleAsc

orderingSqlPart :: Ordering -> ByteString
orderingSqlPart NewDesc = "cid DESC"
orderingSqlPart UpdateAsc = "(SELECT last_updated FROM comics AS c LEFT JOIN crawler_config USING (cid) WHERE c.cid=x.cid) ASC NULLS LAST, ordering_form(title)"
orderingSqlPart UpdateDesc = "(SELECT last_updated FROM comics AS c LEFT JOIN crawler_config USING (cid) WHERE c.cid=x.cid) DESC NULLS LAST, ordering_form(title)"
orderingSqlPart TopDesc = "readers DESC, ordering_form(title)"
orderingSqlPart TitleAsc = "ordering_form(title)"
orderingSqlPart UserUpdates = "num ASC, ordering_form(title)"
orderingSqlPart UserUpdatesDesc = "num DESC, ordering_form(title)"

updateListingRow :: DE.Row (Maybe Text) -> DE.Row UpdateListingItem
updateListingRow x =
  UpdateListingItem
  <$> DE.value DE.int4
  <*> DE.value DE.bool
  <*> x
  <*> listingRow

userListingRow :: DE.Row UserListingItem
userListingRow =
  UserListingItem
  <$> DE.value DE.bool
  <*> DE.value DE.bool
  <*> listingRow

listingRow :: DE.Row ListingItem
listingRow =
  ListingItem
  <$> DE.value DE.int4
  <*> DE.value DE.text

decodeUserListing :: DE.Result (Vector UserListingItem)
decodeUserListing = DE.rowsVector userListingRow

decodeListing :: DE.Result (Vector ListingItem)
decodeListing = DE.rowsVector listingRow

encode2 :: EN.Params (Int32, Int32)
encode2 = contrazip2 (EN.value EN.int4) (EN.value EN.int4)

encode3 :: EN.Params (Int32, Int32, Int32)
encode3 = contrazip3 (EN.value EN.int4) (EN.value EN.int4) (EN.value EN.int4)

encode4 :: EN.Params (Int32, Int32, Int32, Int32)
encode4 = contrazip4 (EN.value EN.int4) (EN.value EN.int4) (EN.value EN.int4) (EN.value EN.int4)

comicsFetchSubscribed :: Ordering -> Query (Int32, Int32, Int32) (Vector UserListingItem)
comicsFetchSubscribed = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           encode3 decodeUserListing True)) allOrderings
    sql o = "SELECT COALESCE(subscribed, false), COALESCE(perm_intr, false), cid, title  \
            \FROM comics AS x \
            \CROSS JOIN users \
            \LEFT JOIN (SELECT uid, cid, true AS subscribed \
            \ FROM subscriptions) AS subscribed USING (uid, cid) \
            \LEFT JOIN (SELECT DISTINCT uid, cid, true AS perm_intr \
            \ FROM permitted_interest) AS perm_intr USING (uid, cid) \
            \WHERE title IS NOT NULL AND uid=$1 ORDER BY " <>
            (orderingSqlPart o) <> " LIMIT $2 OFFSET $3"

updatesFetch :: Ordering -> Query (Int32, Int32, Int32) (Vector UpdateListingItem)
updatesFetch = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           encode3 (DE.rowsVector $ updateListingRow $ pure Nothing)
                           True)) allOrderings
    sql o = "SELECT num, cid IN (SELECT cid FROM comic_tag \
            \WHERE tagid IN (59,60,12,13,1)), cid, title FROM comics AS x \
            \JOIN comic_remain_frag($1) USING (cid) WHERE num > 0 \
            \order by " <> (orderingSqlPart o) <> " limit $2 offset $3"

updatesDirectLinkFetch :: Ordering -> Query (Int32, Int32, Int32, Bool) (Vector UpdateListingItem)
updatesDirectLinkFetch = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           (contrazip4 (EN.value EN.int4)
                            (EN.value EN.int4) (EN.value EN.int4) (EN.value EN.bool))
                           (DE.rowsVector $ updateListingRow $ DE.nullableValue DE.text)
                            True)) allOrderings
    sql o = "SELECT num, cid IN (SELECT cid FROM comic_tag \
            \WHERE tagid IN (59,60,12,13,1)), \
            \COALESCE((SELECT url FROM redir_url_and_last($1, cid, \
            \CASE WHEN $4 THEN -1 ELSE 0 END)), fixed_head, homepage), \
            \cid, title FROM comics AS x \
            \JOIN comic_remain_frag($1) USING (cid) \
            \WHERE num > 0 \
            \order by " <> (orderingSqlPart o) <> " limit $2 offset $3"

profileFetchSubscribed :: Ordering -> Query (Int32, Int32, Int32, Int32) (Vector UserListingItem)
profileFetchSubscribed = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           encode4 decodeUserListing True)) allOrderings
    sql o = "SELECT * FROM (SELECT DISTINCT COALESCE(subscribed, false), \
            \COALESCE(perm_intr, false), comics.cid, title, readers \
            \FROM comics JOIN subscriptions USING (cid) \
            \CROSS JOIN users \
            \LEFT JOIN (SELECT uid, cid, true as subscribed FROM subscriptions) \
            \AS subscribed ON subscribed.uid=users.uid \
            \AND subscribed.cid=comics.cid \
            \LEFT JOIN (SELECT uid, cid, interest, \
            \ true AS perm_intr FROM permitted_interest) AS perm_intr \
            \ON perm_intr.uid=users.uid AND perm_intr.cid=comics.cid \
            \AND interest <> subscriptions.uid \
            \WHERE subscriptions.uid=$1 \
            \AND users.uid=$2) AS x ORDER BY " <>
            (orderingSqlPart o) <> " LIMIT $3 OFFSET $4"

comicsFetch :: Ordering -> Query (Int32, Int32) (Vector ListingItem)
comicsFetch = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           encode2 decodeListing True)) allOrderings
    sql o = "SELECT cid, title FROM comics AS x ORDER BY " <>
            (orderingSqlPart o) <> " LIMIT $1 OFFSET $2"

profileFetch :: Ordering -> Query (Int32, Int32, Int32) (Vector ListingItem)
profileFetch = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           encode3 decodeListing True)) allOrderings
    sql o = "SELECT cid, title FROM comics AS x \
            \JOIN subscriptions USING (cid) \
            \WHERE uid=$1 ORDER BY " <>
            (orderingSqlPart o) <> " LIMIT $2 OFFSET $3"

graveyardFetch :: Query (Int32, Int32) (Vector ListingItem)
graveyardFetch = statement sql encode2 decodeListing True
  where
    sql = "SELECT cid, title FROM graveyard ORDER BY ordering_form(title) \
          \LIMIT $1 OFFSET $2"


comicsTotalFetch :: Query () Int32
comicsTotalFetch = statement sql EN.unit (DE.singleRow $ DE.value DE.int4) True
  where
    sql = "SELECT count(*) FROM comics"

graveyardTotalFetch :: Query () Int32
graveyardTotalFetch = statement sql EN.unit (DE.singleRow $ DE.value DE.int4) True
  where
    sql = "SELECT count(*) FROM graveyard"
