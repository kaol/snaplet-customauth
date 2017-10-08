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
  extractAuth :: a -> AuthManager u b

class (UserData u, Binary i, Show e, Eq e) => IAuthBackend u i e b | u -> b, b -> e, e -> i where
  preparePasswordCreate :: Maybe u -> Text -> Handler b (AuthManager u b) (Either e i)
  cancelPrepare :: i -> Handler b (AuthManager u b) ()
  create :: Text -> i -> Handler b (AuthManager u b) (Either (Either e CreateFailure) u)
  attachLoginMethod :: i -> Handler b (AuthManager u b) (Either e u)
  login :: Text -> Text -> Handler b (AuthManager u b) (Either e (Maybe u))
  logout :: Text -> Handler b (AuthManager u b) ()
  recover :: Text -> Handler b (AuthManager u b) (Either (Either e AuthFailure) u)
  getUserId :: u -> Handler b (AuthManager u b) ByteString
  isDuplicateError :: e -> Handler b (AuthManager u b) Bool

data AuthManager u b = forall e i. IAuthBackend u i e b => AuthManager
  { activeUser :: UserData u => Maybe u
  , sessionCookieName :: ByteString
  , userField :: ByteString
  , passwordField :: ByteString
  , stateStore' :: SnapletLens (Snaplet b) SessionManager
  , getKey' :: Provider -> (Text, Text)
  }

data OAuth2Settings u i e b = IAuthBackend u i e b => OAuth2Settings {
    enabledProviders :: [Provider]
  , getKey :: Provider -> (Text, Text)
  , oauth2Check :: Provider -> Text -> Handler b (AuthManager u b) (Either e (Maybe u))
  , oauth2Login :: Provider -> Text -> Handler b (AuthManager u b) (Either e u)
  , oauth2Failure :: Maybe Provider -> Either e CreateFailure -> Handler b (AuthManager u b) ()
  , oauth2ActionFailure :: Provider -> Either e OAuth2ActionFailure -> Handler b (AuthManager u b) ()
  , prepareOAuth2Create :: Provider -> Text -> Handler b (AuthManager u b) (Either e i)
  , oauth2AccountCreated :: Handler b (AuthManager u b) ()
  , oauth2LoginDone :: Handler b (AuthManager u b) ()
  , resumeAction :: Provider -> Text -> ByteString -> Handler b (AuthManager u b) ()
  , stateStore :: SnapletLens (Snaplet b) SessionManager
  , httpManager :: Manager
  }
