{-# LANGUAGE DeriveGeneric #-}

module Piperka.Account.Types where

import Data.Binary
import Data.Int
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Hasql.Session as S

import Piperka.Listing.Types (ViewColumns(..))
import Piperka.Profile.Types (Privacy(..))
import Piperka.OAuth2.Types

data BookmarkOptions = BookmarkOptions
  { bookmarkSort :: Int32
  , offsetMode :: Bool
  , holdBookmark :: Bool
  }
  deriving (Show, Eq, Generic)

data AccountData = AccountData
  { userAccount :: UserAccountSettings
  , providers :: [ProviderData]
  }

data UserAccountSettings = UserAccountSettings
  { privacy :: Privacy
  , hasPassword :: Bool
  , email :: Maybe Text
  , writeup :: Maybe Text
  , bookmarkSettings :: BookmarkOptions
  } deriving (Show, Eq)

data ProviderData = ProviderData
  { name :: Text
  , label :: Text
  , identification :: Maybe Text
  }

data NeedsValidation = NeedsValidation Provider PrivData

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

data AccountUpdate = AccountUpdateUnpriv
  { newWindows :: Bool
  , rows' :: Int32
  , columns' :: ViewColumns
  , bookmarkSettings' :: BookmarkOptions
  } | AccountUpdatePriv ValidateMethod PrivData
  deriving (Show, Eq, Generic)

data PrivData = UpdateAccount
  { newPassword :: Maybe Text
  , newPasswordRetype :: Maybe Text
  , email' :: Maybe Text
  , privacy' :: Privacy
  , writeup' :: Maybe Text
  , oauth2Removes :: [Provider]
  , passwordless :: Bool
  } | AttachProvider Provider Text
  deriving (Show, Eq, Generic)

data OAuth2Payload = AccountPayload PrivData | AttachPayload Provider
  deriving (Show, Eq, Generic)

instance Binary BookmarkOptions
instance Binary AccountUpdate
instance Binary PrivData
instance Binary OAuth2Payload

data ValidateMethod = Password Text | OAuth2 Provider | Trusted
  deriving (Show, Eq, Generic)

instance Binary ValidateMethod
