{-# LANGUAGE OverloadedStrings #-}

module Piperka.Update.Statements(updateOptionsFetch) where

import Piperka.Update.Types
import Piperka.Listing.Types.Ordering

import Control.Applicative
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Data.Int
import Hasql.Query
import Prelude hiding (Ordering)

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

updateOptionsFetch :: Query Int32 UpdateOptions
updateOptionsFetch = statement sql encode (DE.singleRow updateOptionsRow) True
  where
    sql = "SELECT (SELECT COUNT(*) FROM comic_remain_frag(users.uid) WHERE num > 0), \
          \bookmark_sort, offset_bookmark_by_one, hold_bookmark \
          \FROM users WHERE uid=$1"
    encode = EN.value EN.int4
