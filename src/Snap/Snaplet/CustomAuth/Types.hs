{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Snap.Snaplet.CustomAuth.Types where

import Control.Lens.TH
import Data.Binary
import Data.Binary.Orphans ()
import Data.ByteString
import Data.String (IsString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data AuthFailure =
  AuthFailure | UsernameMissing | PasswordMissing
  deriving (Show, Eq, Read)

data CreateFailure =
    MissingName | NameUsed | InvalidName
  | DuplicateName
  | PasswordFailure PasswordFailure
  | OAuth2Failure OAuth2Failure
  deriving (Eq, Show, Read)

data OAuth2Failure =
    StateNotStored | StateNotReceived | ExpiredState | BadState
  | IdExtractionFailed | NoStoredToken
  | AlreadyUser | AlreadyLoggedIn | OtherUser
  | AttachNotLoggedIn | AlreadyAttached
  | ProviderError (Maybe Text)
  | AccessTokenFetchError
  deriving (Show, Read)

instance Eq OAuth2Failure where
  (ProviderError _) == (ProviderError _) = True
  a == b = (show a) == (show b)

data OAuth2ActionFailure =
  ActionTimeout | ActionDecodeError | ActionUserMismatch
  deriving (Show, Eq)

data PasswordFailure = Missing | Mismatch
  deriving (Show, Eq, Read)

data Provider =
  Google | Reddit
  deriving (Show, Eq, Ord, Generic)

data SavedAction = SavedAction {
    actionProvider :: Provider
  , actionStamp :: UTCTime
  , actionUser :: Maybe ByteString
  , savedAction :: Maybe ByteString
  } deriving (Generic)

instance Binary SavedAction

instance Binary Provider

parseProvider
  :: (Eq s, IsString s)
  => s
  -> Maybe Provider
parseProvider "reddit" = Just Reddit
parseProvider "Reddit" = Just Reddit
parseProvider "google" = Just Google
parseProvider "Google" = Just Google
parseProvider _ = Nothing

data AuthUser = AuthUser
  { name :: Text
  , session :: Text
  , csrfToken :: Text
  } deriving (Show)

data AuthSettings = AuthSettings {
    _authName :: Text
  , _authUserField :: ByteString
  , _authPasswordField :: ByteString
  , _authSessionCookieName :: ByteString
  }

makeLenses ''AuthSettings

defAuthSettings :: AuthSettings
defAuthSettings = AuthSettings "auth" "_login" "_password" "_session"
