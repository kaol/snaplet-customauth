{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FunctionalDependencies #-}

module Snap.Snaplet.CustomAuth.AuthManager
  (
    AuthManager(..)
  , IAuthBackend(..)
  , UserData(..)
  , HasAuth(..)
  , AuthFailure(..)
  , OAuth2Settings(..)
  ) where

import Crypto.JOSE.JWK (JWK)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Network.HTTP.Client (Manager)

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.Session2
import Snap.Snaplet.Session2.SessionManager

class UserData a where
  extractUser :: a -> AuthUser

class UserData u => HasAuth u a where
  extractAuth :: a -> AuthManager u e b

class (UserData u, Binary i, Show e, Eq e) => IAuthBackend u i e b | u -> b, b -> e, e -> i where
  preparePasswordCreate :: Maybe u -> Text -> Handler b (AuthManager u e b) (Either e i)
  cancelPrepare :: i -> Handler b (AuthManager u e b) ()
  create :: Text -> i -> Handler b (AuthManager u e b) (Either (Either e CreateFailure) u)
  attachLoginMethod :: u -> i -> Handler b (AuthManager u e b) (Either e ())
  login :: Text -> Text -> Handler b (AuthManager u e b) (Either e (Maybe u))
  logout :: Text -> Handler b (AuthManager u e b) ()
  recover :: Text -> Handler b (AuthManager u e b) (Either (AuthFailure e) u)
  getUserId :: u -> Handler b (AuthManager u e b) ByteString
  isDuplicateError :: e -> Handler b (AuthManager u e b) Bool

data AuthManager u e b = forall i. IAuthBackend u i e b => AuthManager
  { activeUser :: UserData u => Maybe u
  , cookieLifetime :: Maybe NominalDiffTime
  , sessionCookieName :: ByteString
  , userField :: ByteString
  , passwordField :: ByteString
  , stateStore' :: SnapletLens (Snaplet b) (SessionManager b)
  , oauth2Provider :: Maybe Text
  , authFailData :: Maybe (AuthFailure e)
  , providers :: Map Text Provider

  -- Parameters used by OAuth2
  , httpManager :: Manager
  -- | Used for RFC 7523 JWT Bearer tokens when required
  , jwk :: Maybe JWK
  }

data OAuth2Settings p u i e b = IAuthBackend u i e b => OAuth2Settings
  { oauth2Check :: p -> Text -> Handler b (AuthManager u e b) (Either e (Maybe ByteString))
  , oauth2Login :: p -> Text -> Handler b (AuthManager u e b) (Either e (Maybe u))
  , oauth2Failure :: OAuth2Stage -> Handler b (AuthManager u e b) ()
  , prepareOAuth2Create :: p -> Text -> Handler b (AuthManager u e b) (Either e i)
  , oauth2AccountCreated :: u -> Handler b (AuthManager u e b) ()
  , oauth2LoginDone :: Handler b (AuthManager u e b) ()
  , resumeAction :: Text -> Text -> ByteString -> Handler b (AuthManager u e b) ()
  , stateStore :: SnapletLens (Snaplet b) (SessionManager b)
  , bracket :: Handler b (AuthManager u e b) () -> Handler b (AuthManager u e b) ()
  -- | Enabled provider names along with user defined associated data.
  -- These are passed directly to routes and don't need to be
  -- represented in AuthManager data.
  , enabledProviders :: [(Text, p)]
  }
