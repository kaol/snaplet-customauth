module Snap.Snaplet.CustomAuth
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

import Snap.Snaplet.CustomAuth.Handlers
import Snap.Snaplet.CustomAuth.Heist
import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.CustomAuth.AuthManager
