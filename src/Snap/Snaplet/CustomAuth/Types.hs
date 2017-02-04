{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CustomAuth.Types where

import Data.Text
import Data.ByteString

data (Eq e, Show e) => AuthFailure e =
  AuthFailure | UsernameMissing | PasswordMissing | AuthError e
  deriving (Show, Eq)

data (Eq e, Show e) => CreateFailure e =
  MissingName | NameUsed | PasswordMismatch | NoPassword |
  AvailError e | CreateError e
  deriving (Show, Eq)

data AuthUser = AuthUser
 {  name :: Text
  , session :: Text
  , csrfToken :: Text
  }
              deriving (Show)

data AuthSettings = AuthSettings {
    asSessionCookieName :: ByteString
  , asUserField :: ByteString
  , asPasswordField :: ByteString
  }

defAuthSettings :: AuthSettings
defAuthSettings = AuthSettings {
    asSessionCookieName = "_id_token"
  , asUserField = "user"
  , asPasswordField = "password"
  }
