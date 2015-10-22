{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FunctionalDependencies #-}

module Snap.Snaplet.CustomAuth.AuthManager
  (
    AuthManager(..)
  , IAuthBackend(..)
  , UserData(..)
  ) where

import Data.ByteString
import Data.Text

import Snap.Snaplet
import Snap.Snaplet.CustomAuth.Types

class UserData a where
  extractUser :: a -> AuthUser

class UserData u => IAuthBackend u r | r -> u where
  login :: r -> Text -> Text -> IO (Either AuthFailure u)
  logout :: r -> Text -> IO ()
  recover :: r -> Text -> IO (Either AuthFailure u)

data AuthManager u b = forall r. IAuthBackend u r => AuthManager
  { backend :: r
  , activeUser :: UserData u => Maybe u
  , sessionCookieName :: ByteString
  , userField :: ByteString
  , passwordField :: ByteString
  }
