module Snap.Snaplet.CustomAuth.OAuth2
  (
    OAuth2Settings(..)
  , AuthFailure(Action)
  , OAuth2Failure(..)
  , Provider(..)
  , OAuth2Stage(..)
  , parseProvider
  , addOAuth2Splices
  , oauth2Init
  , saveAction
  , redirectToProvider
  , getOAuth2Provider
  )
  where

import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.OAuth2.Internal
import Snap.Snaplet.CustomAuth.OAuth2.Splices
import Snap.Snaplet.CustomAuth.Types
