module Snap.Snaplet.CustomAuth
  (
    AuthManager(..)
  , AuthUser(..)
  , AuthFailure(UserError, Create, Login)
  , LoginFailure(..)
  , CreateFailure(..)
  , PasswordFailure(..)
  , IAuthBackend(..)
  , UserData(..)
  , defAuthSettings
  , authName
  , authCookieLifetime
  , authJWK
  , createAccount
  , loginUser
  , logoutUser
  , recoverSession
  , combinedLoginRecover
  , setUser
  , setUser'
  , currentUser
  , getAuthFailData
  , resetAuthFailData
  , authInit
  , isSessionDefined
    -- Heist
  , isLoggedIn
  , compiledAuthSplices
  , ifLoggedIn
  , ifLoggedOut
  , loggedInUser
  )
  where

import Snap.Snaplet.CustomAuth.Handlers
import Snap.Snaplet.CustomAuth.Heist
import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.User
