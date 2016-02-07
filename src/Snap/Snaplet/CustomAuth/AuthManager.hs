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
  ) where

import Data.ByteString
import Data.Text

import Snap.Snaplet
import Snap.Snaplet.CustomAuth.Types

class UserData a where
  extractUser :: a -> AuthUser

class UserData u => HasAuth u a where
  extractAuth :: a -> AuthManager u b

class UserData u => IAuthBackend u b | b -> u where
  login :: Text -> Text -> Handler b (AuthManager u b) (Either AuthFailure u)
  logout :: Text -> Handler b (AuthManager u b) ()
  recover :: Text -> Handler b (AuthManager u b) (Either AuthFailure u)

data AuthManager u b = IAuthBackend u b => AuthManager
  { activeUser :: UserData u => Maybe u
  , sessionCookieName :: ByteString
  , userField :: ByteString
  , passwordField :: ByteString
  }
