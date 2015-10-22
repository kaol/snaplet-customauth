{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CustomAuth.Types where

import Data.Text
import Data.ByteString

data AuthFailure = AuthFailure | UsernameMissing | PasswordMissing | AuthError String
                 deriving (Show)

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
