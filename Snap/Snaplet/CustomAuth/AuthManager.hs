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

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.HashMap.Lazy (HashMap)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Network.HTTP.Client (Manager)

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.Session

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
  , stateStore' :: SnapletLens (Snaplet b) SessionManager
  , oauth2Provider :: Maybe Text
  , authFailData :: Maybe (AuthFailure e)
  , providers :: HashMap Text Provider
  }

data OAuth2Settings u i e b = IAuthBackend u i e b => OAuth2Settings {
    oauth2Check :: Text -> Text -> Handler b (AuthManager u e b) (Either e (Maybe ByteString))
  , oauth2Login :: Text -> Text -> Handler b (AuthManager u e b) (Either e (Maybe u))
  , oauth2Failure :: OAuth2Stage -> Handler b (AuthManager u e b) ()
  , prepareOAuth2Create :: Text -> Text -> Handler b (AuthManager u e b) (Either e i)
  , oauth2AccountCreated :: u -> Handler b (AuthManager u e b) ()
  , oauth2LoginDone :: Handler b (AuthManager u e b) ()
  , resumeAction :: Text -> Text -> ByteString -> Handler b (AuthManager u e b) ()
  , stateStore :: SnapletLens (Snaplet b) SessionManager
  , httpManager :: Manager
  , bracket :: Handler b (AuthManager u e b) () -> Handler b (AuthManager u e b) ()
  }
