module Piperka.Account.Types where

import Data.Int
import Data.Text (Text)
import qualified Hasql.Session as S

import Piperka.Listing.Types (ViewColumns(..))
import Piperka.Profile.Types (Privacy(..))

data UserAccountSettings = UserAccountSettings
  { privacy :: Privacy
  , email :: Maybe Text
  , writeup :: Maybe Text
  } deriving (Show, Eq)

emptySettings :: UserAccountSettings
emptySettings = UserAccountSettings Private Nothing Nothing

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
  } | AccountUpdatePriv
  { password :: Text
  , newPassword :: Maybe Text
  , newPasswordRetype :: Maybe Text
  , email' :: Maybe Text
  , privacy' :: Privacy
  , writeup' :: Maybe Text
  } deriving (Show, Eq)
