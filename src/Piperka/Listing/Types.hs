{-# LANGUAGE OverloadedStrings #-}

module Piperka.Listing.Types where

import Network.HTTP.Types.URI (Query)
import Data.Int
import Data.Text (Text)
import Data.Vector
import Prelude hiding (Ordering)
import Hasql.Session (Error)
import Data.Text.Encoding (encodeUtf8)

import Piperka.Profile.Types
  (Profile, CommonProfile, profile, name)
import Piperka.Update.Types (UpdateOptions)
import qualified Piperka.Update.Types (total)
import qualified Piperka.Profile.Types (total)
import Piperka.Profile.Types (Profile(..))

data ListingItem = ListingItem
  { cid :: Int32
  , title :: Text
  } deriving (Show, Eq)

data UserListingItem = UserListingItem
  { subs :: Bool
  , friends :: Bool
  , listing' :: ListingItem
  } deriving (Show, Eq)

data UpdateListingItem = UpdateListingItem
  { new :: Int32
  , listing'' :: ListingItem
  } deriving (Show, Eq)

data ViewColumns = OneColumn | TwoColumn | ThreeColumn
                 deriving (Show, Eq, Ord, Read)

data ListingError = Missing | SqlError Error
                  deriving (Show, Eq)

columnsToInt :: (Num n) => ViewColumns -> n
columnsToInt OneColumn = 1
columnsToInt TwoColumn = 2
columnsToInt ThreeColumn = 3

intToColumns :: (Num a, Eq a) => a -> ViewColumns
intToColumns 1 = OneColumn
intToColumns 3 = ThreeColumn
intToColumns _ = TwoColumn

data ListingMode = Top | Browse | Profile | Update | Graveyard
                 deriving (Show, Read, Eq)

data ListingItemMode = ListingMode | UserMode | UpdateMode
                     deriving (Show, Read, Eq)

data ListingParam =
  ListingParam Bool Int (Vector ListingItem) |
  UserParam Bool Int (Vector UserListingItem) |
  UpdateParam UpdateOptions (Vector UpdateListingItem) |
  ProfileParam CommonProfile (Vector ListingItem) |
  UserProfileParam Profile (Vector UserListingItem) |
  GraveyardParam Int (Vector ListingItem)
  deriving (Show, Eq)

extractListing :: ListingParam -> Maybe (Vector ListingItem)
extractListing (ListingParam _ _ x) = Just x
extractListing (ProfileParam _ x) = Just x
extractListing (GraveyardParam _ x) = Just x
extractListing _ = Nothing

extractUserListing :: ListingParam -> Maybe (Vector UserListingItem)
extractUserListing (UserParam _ _ x) = Just x
extractUserListing (UserProfileParam _ x) = Just x
extractUserListing _ = Nothing

extractUpdateListing :: ListingParam -> Maybe (Vector UpdateListingItem)
extractUpdateListing (UpdateParam _ x) = Just x
extractUpdateListing _ = Nothing

extractTotal :: (Integral n) => ListingParam -> n
extractTotal (ListingParam _ x _) = fromIntegral x
extractTotal (UserParam _ x _) = fromIntegral x
extractTotal (UpdateParam x _) = fromIntegral $ Piperka.Update.Types.total x
extractTotal (ProfileParam x _) = fromIntegral $ Piperka.Profile.Types.total x
extractTotal (UserProfileParam x _) = fromIntegral $ Piperka.Profile.Types.total $ profile x
extractTotal (GraveyardParam x _) = fromIntegral x

getUpdateParam :: ListingParam -> UpdateOptions
getUpdateParam (UpdateParam o _) = o
getUpdateParam _ = error "not possible"

getProfile :: ListingParam -> Profile
getProfile (ProfileParam x _) = Common x
getProfile (UserProfileParam x _) = x
getProfile _ = undefined

listingModeSubscribes :: ListingMode -> Bool
listingModeSubscribes Top = True
listingModeSubscribes Browse = True
listingModeSubscribes Profile = True
listingModeSubscribes _ = False

getListingPageName :: ListingMode -> Text
getListingPageName Top = "top.html"
getListingPageName Browse = "browse.html"
getListingPageName Profile = "profile.html"
getListingPageName Update = "updates.html"
getListingPageName Graveyard = "graveyard.html"

getListingPathQuery :: ListingMode -> ListingParam -> ([Text], Query)
getListingPathQuery Top _ = (["top.html"], [])
getListingPathQuery Browse _ = (["browse.html"], [])
getListingPathQuery Graveyard _ = (["graveyard.html"], [])
getListingPathQuery Update _ = (["updates.html"], [])
getListingPathQuery Profile (ProfileParam prof _) =
  (["profile.html"], [("name", Just $ encodeUtf8 $ name $ profile prof)])
getListingPathQuery Profile (UserProfileParam prof _) =
  (["profile.html"], [("name", Just $ encodeUtf8 $ name $ profile prof)])
getListingPathQuery Profile _ = error "not possible"

{-
getListingPathQuery Profile (UserProfileParam (OwnProfile _ _ _) _) =
  PathQuery "profile.html" []
-}

class SubListing a where
  listing :: a -> ListingItem

instance SubListing UserListingItem where
  listing = listing'

instance SubListing UpdateListingItem where
  listing = listing''
