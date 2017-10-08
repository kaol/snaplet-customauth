{-# LANGUAGE DeriveGeneric #-}

module Piperka.Profile.Types where

import Data.Binary
import Data.Int
import Data.Text
import GHC.Generics (Generic)

data Profile = Common CommonProfile | Own OwnProfile | Other OtherProfile
             deriving (Show, Eq)

data Privacy = Public | Friends | Private
             deriving (Show, Eq, Ord, Generic)

instance Binary Privacy

data CommonProfile = CommonProfile
  { uid :: Int32
  , name :: Text
  , perm :: Bool
  , privacy :: Privacy
  , writeup :: Maybe Text
  , total :: Int32
  , inComics :: Int32
  } deriving (Show, Eq)

data OwnProfile = OwnProfile
  { followers :: Int32
  , followees :: Int32
  , profile' :: CommonProfile
  } deriving (Show, Eq)

data OtherProfile = OtherProfile
  { myPrivacy :: Privacy
  , myPerm :: Bool
  , titfortat :: Bool
  , interest :: Bool
  , profile'' :: CommonProfile
  } deriving (Show, Eq)

class SubProfile a where
  profile :: a -> CommonProfile

instance SubProfile OwnProfile where
  profile = profile'

instance SubProfile OtherProfile where
  profile = profile''

instance SubProfile CommonProfile where
  profile = id

instance SubProfile Profile where
  profile (Common x) = x
  profile (Own x) = profile x
  profile (Other x) = profile x

intToPrivacy :: (Num a, Eq a) => a -> Privacy
intToPrivacy 3 = Public
intToPrivacy 2 = Friends
intToPrivacy _ = Private

privacyToInt :: (Num a) => Privacy -> a
privacyToInt Public = 3
privacyToInt Friends = 2
privacyToInt Private = 1
