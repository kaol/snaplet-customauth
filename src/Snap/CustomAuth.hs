module Snap.CustomAuth
  (
    AuthManager(..)
  , AuthUser(..)
  , AuthFailure(..)
  , IAuthBackend(..)
  , UserData(..)
  , defAuthSettings
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
  )
  where

import Snap.CustomAuth.Handlers
import Snap.CustomAuth.Heist
import Snap.CustomAuth.Types
import Snap.CustomAuth.AuthManager
