{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Snap.Snaplet.CustomAuth.Types where

import Control.Lens.TH
import Data.Binary
import Data.Binary.Instances ()
import Data.ByteString
import Data.Text (Text)
import Data.Time.Clock (UTCTime, NominalDiffTime)
import GHC.Generics (Generic)
import Network.OAuth.OAuth2 (OAuth2)
import URI.ByteString (URI)

import Snap.Core (Cookie(..))

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
    MissingName | InvalidName
  | DuplicateName
  | PasswordFailure PasswordFailure
  | OAuth2Failure OAuth2Failure
  deriving (Eq, Show, Read)

data OAuth2Failure =
    StateNotStored | StateNotReceived | ExpiredState | BadState
  | ConfigurationError | IdExtractionFailed (Maybe Text) | NoStoredToken
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

data Provider = Provider
  { providerName :: Text
  , discovery :: Maybe URI
  , scope :: Text
  , identityEndpoint :: URI
  , identityField :: Text
  , oauth :: OAuth2
  }
  deriving (Show)

data SavedAction = SavedAction
  { actionProvider :: Text
  , actionStamp :: UTCTime
  , actionUser :: Maybe ByteString
  -- | Is the action expected to match with an ID attached to the
  -- user.  Use False if using the action to attach a new ID.
  , requireUser :: Bool
  , savedAction :: ByteString
  } deriving (Generic)

instance Binary SavedAction

data AuthUser = AuthUser
  { name :: Text
  , session :: ByteString
  , csrfToken :: ByteString
  } deriving (Show)

data AuthSettings = AuthSettings
  { _authName :: Text
  , _authCookieLifetime :: Maybe NominalDiffTime
  }

makeLenses ''AuthSettings

defAuthSettings :: AuthSettings
defAuthSettings = AuthSettings "auth" Nothing
