module Piperka.Update.Types where

import Piperka.Listing.Types.Ordering

import Data.Int
import Prelude hiding (Ordering)

data UpdateOptions = UpdateOptions
  { total :: Int32
  , bookmarkSort :: Ordering
  , offsetMode :: Bool
  , holdBookmark :: Bool
  }
  deriving (Show, Eq)
