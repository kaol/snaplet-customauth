{-# LANGUAGE OverloadedStrings #-}

module Piperka.Update.Statements (updateOptionsFetch, updateAndRedirect) where

import Contravariant.Extras.Contrazip
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Int
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query
import Prelude hiding (Ordering)

import Application
import Piperka.Update.Types
import Piperka.Listing.Types.Ordering

updateOrderingDecode :: Int32 -> Ordering
updateOrderingDecode 0 = UserUpdatesDesc
updateOrderingDecode 1 = UserUpdates
updateOrderingDecode 2 = TitleAsc
updateOrderingDecode 3 = UpdateAsc
updateOrderingDecode _ = UpdateDesc

updateOptionsRow :: DE.Row UpdateOptions
updateOptionsRow =
  UpdateOptions
  <$> DE.value DE.int4
  <*> (liftA updateOrderingDecode $ DE.value DE.int4)
  <*> DE.value DE.bool
  <*> DE.value DE.bool

updateOptionsFetch
  :: Query UserID UpdateOptions
updateOptionsFetch = statement sql encode (DE.singleRow updateOptionsRow) True
  where
    sql = "SELECT (SELECT COUNT(*) FROM comic_remain_frag(users.uid) WHERE num > 0), \
          \bookmark_sort, offset_bookmark_by_one, hold_bookmark \
          \FROM users WHERE uid=$1"
    encode = EN.value EN.int4

updateAndRedirect
  :: Query (UserID, Int32, Bool) (Maybe ByteString)
updateAndRedirect = statement sql encode (DE.maybeRow $ DE.value DE.bytea) True
  where
    encode = contrazip3 (EN.value EN.int4) (EN.value EN.int4) (EN.value EN.bool)
    sql = "WITH page AS (\
          \SELECT url, last_ord AS ord, last_subord AS subord FROM \
          \redir_url_and_last($1, $2, CASE WHEN $3 THEN -1 ELSE 0 END)), \
          \deleted_recent AS (\
          \DELETE FROM recent WHERE uid=$1 AND cid=$2), r AS (\
          \INSERT INTO recent (uid, cid, ord, subord, used_on) SELECT \
          \$1, $2, page.ord, page.subord, now() FROM page), upd AS (\
          \UPDATE subscriptions SET ord=max_ord_of, subord=\
          \COALESCE((SELECT MAX(subord)+1 FROM page_fragments WHERE cid=\
          \subscriptions.cid AND ord=max_ord_of), 1) FROM max_ord_of($2) \
          \WHERE uid=$1 AND cid=$2) \
          \SELECT url FROM page"
