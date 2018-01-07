{-# LANGUAGE DeriveGeneric #-}

module Piperka.Account.Types where

import Data.Binary
import Data.Int
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Hasql.Session as S
import Snap.Snaplet.CustomAuth.OAuth2 (Provider)

import Piperka.Listing.Types (ViewColumns(..))
import Piperka.Profile.Types (Privacy(..))

data AccountData = AccountData
  { userAccount :: UserAccountSettings
  , providers :: [ProviderData]
  }

data UserAccountSettings = UserAccountSettings
  { privacy :: Privacy
  , hasPassword :: Bool
  , email :: Maybe Text
  , writeup :: Maybe Text
  } deriving (Show, Eq)

data ProviderData = ProviderData
  { name :: Text
  , label :: Text
  , identification :: Maybe Text
  }

data NeedsValidation = NeedsValidation Provider AccountUpdate

data AccountUpdateError = AccountSqlError S.Error
                        | AccountPasswordMissing
                        | AccountPasswordWrong
                        | AccountNewPasswordMismatch
                        deriving (Eq)

instance Enum AccountUpdateError where
  fromEnum (AccountSqlError _) = 0
  fromEnum AccountPasswordMissing = 1
  fromEnum AccountPasswordWrong = 2
  fromEnum AccountNewPasswordMismatch = 3
  toEnum 0 = AccountSqlError (S.ClientError Nothing)
  toEnum 1 = AccountPasswordMissing
  toEnum 2 = AccountPasswordWrong
  toEnum 3 = AccountNewPasswordMismatch
  toEnum _ = error "AccountUpdateError"

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
  }
  deriving (Show, Eq, Generic)

instance Binary AccountUpdate
instance Binary OAuth2Change

data ValidateMethod = Password Text | OAuth2 Provider | Trusted
  deriving (Show, Eq, Generic)

instance Binary ValidateMethod
