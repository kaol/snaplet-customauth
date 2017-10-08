module Snap.Snaplet.CustomAuth
  (
    AuthManager(..)
  , AuthUser(..)
  , AuthFailure(..)
  , CreateFailure(..)
  , IAuthBackend(..)
  , UserData(..)
  , OAuth2Settings(..)
  , Provider
  , parseProvider
  , defAuthSettings
  , authName
  , authUserField
  , authPasswordField
  , authSessionCookieName
  , createAccount
  , loginUser
  , logoutUser
  , recoverSession
  , combinedLoginRecover
  , currentUser
  , authInit
    -- Heist
  , isLoggedIn
  , addAuthSplices
  , compiledAuthSplices
  , ifLoggedIn
  , ifLoggedOut
  , loggedInUser
  , addOAuth2Splices
  )
  where

import Snap.Snaplet.CustomAuth.Handlers
import Snap.Snaplet.CustomAuth.Handlers.OAuth2.Splices
import Snap.Snaplet.CustomAuth.Heist
import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.User
