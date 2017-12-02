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
import Data.Text (Text)
import Network.HTTP.Client (Manager)

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
  , sessionCookieName :: ByteString
  , userField :: ByteString
  , passwordField :: ByteString
  , stateStore' :: SnapletLens (Snaplet b) SessionManager
  , oauth2Provider :: Maybe Provider
  , authFailData :: Maybe (AuthFailure e)
  }

data OAuth2Settings u i e b = IAuthBackend u i e b => OAuth2Settings {
    enabledProviders :: [Provider]
  , oauth2Check :: Provider -> Text -> Handler b (AuthManager u e b) (Either e (Maybe ByteString))
  , oauth2Login :: Provider -> Text -> Handler b (AuthManager u e b) (Either e (Maybe u))
  , oauth2Failure :: Handler b (AuthManager u e b) ()
  , oauth2ActionFailure :: Handler b (AuthManager u e b) ()
  , prepareOAuth2Create :: Provider -> Text -> Handler b (AuthManager u e b) (Either e i)
  , oauth2AccountCreated :: Handler b (AuthManager u e b) ()
  , oauth2LoginDone :: Handler b (AuthManager u e b) ()
  , resumeAction :: Provider -> Text -> ByteString -> Handler b (AuthManager u e b) ()
  , stateStore :: SnapletLens (Snaplet b) SessionManager
  , httpManager :: Manager
  }
