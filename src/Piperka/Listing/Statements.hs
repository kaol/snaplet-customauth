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
import Data.Vector hiding ((++), map)
import Hasql.Query
import Prelude hiding (Ordering)

parseOrdering :: String -> Ordering
parseOrdering "new" = NewDesc
parseOrdering "update" = UpdateDesc
parseOrdering "top" = TopDesc
parseOrdering _ = TitleAsc

orderingSqlPart :: Ordering -> ByteString
orderingSqlPart NewDesc = "cid DESC"
orderingSqlPart UpdateAsc = "(SELECT last_update FROM comics AS c LEFT JOINS crawler_config USING (cid) WHERE c.cid=comics.cid) ASC NULLS LAST, ordering_form(title)"
orderingSqlPart UpdateDesc = "(SELECT last_update FROM comics AS c LEFT JOINS crawler_config USING (cid) WHERE c.cid=comics.cid) DESC NULLS LAST, ordering_form(title)"
orderingSqlPart TopDesc = "readers DESC, ordering_form(title)"
orderingSqlPart TitleAsc = "ordering_form(title)"
orderingSqlPart UserUpdates = "num ASC, ordering_form(title)"
orderingSqlPart UserUpdatesDesc = "num DESC, ordering_form(title)"

updateListingRow :: DE.Row UpdateListingItem
updateListingRow =
  UpdateListingItem
  <$> DE.value DE.int4
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

decodeUpdateListing :: DE.Result (Vector UpdateListingItem)
decodeUpdateListing = DE.rowsVector updateListingRow

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
    sql o = "select subscribed, perm_intr, cid, title  \
            \from comics \
            \cross join users \
            \left join (select uid, cid, 1 as subscribed \
            \ from subscriptions) as subscribed using (uid, cid) \
            \left join (select distinct uid, cid, 1 as perm_intr \
            \ from permitted_interest) as perm_intr using (uid, cid) \
            \where title is not null and uid=$1 order by " <>
            (orderingSqlPart o) <> " limit $2 offset $3"

updatesFetch :: Ordering -> Query (Int32, Int32, Int32) (Vector UpdateListingItem)
updatesFetch = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           encode3 decodeUpdateListing True)) allOrderings
    sql o = "SELECT num, cid, title FROM comics \
            \JOIN comic_remain_frag($1) USING (cid) WHERE num > 0 \
            \order by " <> (orderingSqlPart o) <> " limit $2 offset $3"

profileFetchSubscribed :: Ordering -> Query (Int32, Int32, Int32, Int32) (Vector UserListingItem)
profileFetchSubscribed = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           encode4 decodeUserListing True)) allOrderings
    sql o = "SELECT DISTINCT subscribed, perm_intr, comics.cid, title \
            \FROM comics JOIN subscriptions USING (cid) \
            \CROSS JOIN users \
            \LEFT JOIN (SELECT uid, cid, 1 as subscribed FROM subscriptions) \
            \AS subscribed ON subscribed.uid=users.uid \
            \AND subscribed.cid=comics.cid \
            \LEFT JOIN (SELECT uid, cid, interest, \
            \ 1 AS perm_intr FROM permitted_interest) AS perm_intr \
            \ON perm_intr.uid=users.uid AND perm_intr.cid=comics.cid \
            \AND interest <> subscriptions.uid \
            \WHERE subscriptions.uid=$1 \
            \AND users.uid=$2 ORDER BY " <>
            (orderingSqlPart o) <> " LIMIT $3 OFFSET $4"

comicsFetch :: Ordering -> Query (Int32, Int32) (Vector ListingItem)
comicsFetch = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           encode2 decodeListing True)) allOrderings
    sql o = "SELECT cid, title FROM comics ORDER BY " <>
            (orderingSqlPart o) <> " LIMIT $1 OFFSET $2"

profileFetch :: Ordering -> Query (Int32, Int32, Int32) (Vector ListingItem)
profileFetch = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           encode3 decodeListing True)) allOrderings
    sql o = "SELECT cid, title FROM comics \
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
