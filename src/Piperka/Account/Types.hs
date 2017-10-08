{-# LANGUAGE DeriveGeneric #-}

module Piperka.Account.Types where

import Data.Binary
import Data.Map.Strict (Map)
import Data.Int
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Hasql.Session as S
import Snap.Snaplet.CustomAuth (Provider)

import Piperka.API.OAuth2.Types
import Piperka.Listing.Types (ViewColumns(..))
import Piperka.Profile.Types (Privacy(..))

data UserAccountSettings = UserAccountSettings
  { privacy :: Privacy
  , email :: Maybe Text
  , writeup :: Maybe Text
  , identification :: Map Provider Text
  } deriving (Show, Eq)

emptySettings :: UserAccountSettings
emptySettings = UserAccountSettings Private Nothing Nothing mempty

data AccountUpdateError = AccountSqlError S.Error
                        | AccountPasswordMissing
                        | AccountPasswordWrong
                        | AccountNewPasswordMismatch
                        | NeedsValidation Provider
                        deriving (Eq)

instance Enum AccountUpdateError where
  fromEnum (AccountSqlError _) = 0
  fromEnum AccountPasswordMissing = 1
  fromEnum AccountPasswordWrong = 2
  fromEnum AccountNewPasswordMismatch = 3
  fromEnum (NeedsValidation p) = 4 + (fromIntegral $ providerOpid p)
  toEnum 0 = AccountSqlError (S.ClientError Nothing)
  toEnum 1 = AccountPasswordMissing
  toEnum 2 = AccountPasswordWrong
  toEnum 3 = AccountNewPasswordMismatch
  toEnum n = NeedsValidation (opidProvider $ fromIntegral n)
--  toEnum _ = error "AccountUpdateError"

instance Bounded AccountUpdateError where
  minBound = AccountSqlError (S.ClientError Nothing)
  maxBound = AccountNewPasswordMismatch

data OAuth2Change = DeleteIdentity Provider
                  | AddIdentity Provider Text
                  deriving (Show, Eq, Generic)

data AccountUpdate = AccountUpdateUnpriv
  { newWindows :: Bool
  , rows' :: Int32
  , columns' :: ViewColumns
  } | AccountUpdatePriv
  { validation :: ValidateMethod
  , newPassword :: Maybe Text
  , newPasswordRetype :: Maybe Text
  , email' :: Maybe Text
  , privacy' :: Privacy
  , writeup' :: Maybe Text
  , oauth2Removes :: [Provider]
  , passwordless :: Bool
  } | AttachProvider Provider
  deriving (Show, Eq, Generic)

instance Binary AccountUpdate
instance Binary OAuth2Change

data ValidateMethod = Password Text | OAuth2 Provider
  deriving (Show, Eq, Generic)

instance Binary ValidateMethod
