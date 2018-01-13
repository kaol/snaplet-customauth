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

data LoginFailure =
  NoSession | SessionRecoverFail | UsernameMissing | PasswordMissing | WrongPasswordOrUsername
  deriving (Show, Eq, Read)

data AuthFailure e =
    UserError e
  | Login LoginFailure
  | Create CreateFailure
  | Action OAuth2ActionFailure
  deriving (Show)

data CreateFailure =
    MissingName | NameUsed | InvalidName
  | DuplicateName
  | PasswordFailure PasswordFailure
  | OAuth2Failure OAuth2Failure
  deriving (Eq, Show, Read)

data OAuth2Failure =
    StateNotStored | StateNotReceived | ExpiredState | BadState
  | ConfigurationError | IdExtractionFailed | NoStoredToken
  | AlreadyUser | AlreadyLoggedIn
  | IdentityInUse
  | ProviderError (Maybe Text)
  | AccessTokenFetchError
  deriving (Show, Read, Eq)

data OAuth2ActionFailure =
  ActionTimeout | ActionDecodeError | ActionUserMismatch
  | AttachNotLoggedIn | AlreadyAttached
  deriving (Show, Eq, Read)

data PasswordFailure = Missing | Mismatch
  deriving (Show, Eq, Read)

data OAuth2Stage = SCallback | SLogin | SCreate | SAction
  deriving (Show, Eq, Read)

data Provider =
  Google | Reddit | Github
  deriving (Show, Eq, Ord, Generic)

data SavedAction = SavedAction {
    actionProvider :: Provider
  , actionStamp :: UTCTime
  , actionUser :: Maybe ByteString
  -- Is the action expected to match with an ID attached to the user.
  -- Use False if using the action to attach a new ID.
  , requireUser :: Bool
  , savedAction :: ByteString
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
parseProvider "github" = Just Github
parseProvider "Github" = Just Github
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
