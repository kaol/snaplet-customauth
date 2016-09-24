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
updateOrderingDecode 0 = UserUpdates
updateOrderingDecode 1 = UserUpdatesDesc
updateOrderingDecode 2 = UpdateAsc
updateOrderingDecode 3 = UpdateDesc
updateOrderingDecode _ = TitleAsc

updateOptionsRow :: DE.Row UpdateOptions
updateOptionsRow =
  UpdateOptions
  <$> DE.value DE.int4
  <*> (liftA updateOrderingDecode $ DE.value DE.int4)
  <*> DE.value DE.bool
  <*> DE.value DE.bool

--updateOptionsFetch :: Query Int32 (Maybe (Int32, UpdateOptions))
updateOptionsFetch :: Query Int32 UpdateOptions
updateOptionsFetch = statement sql encode (DE.singleRow updateOptionsRow) True
  where
    sql = "SELECT (SELECT COUNT(*) FROM comic_remain_frag(users.uid) WHERE num > 0), \
          \bookmark_sort, offset_bookmark_by_one, hold_bookmark \
          \FROM users WHERE uid=$1"
    encode = EN.value EN.int4
