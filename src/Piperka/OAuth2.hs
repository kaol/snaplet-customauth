{-# LANGUAGE OverloadedStrings #-}

module Piperka.OAuth2 (piperkaOAuth2) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Hasql.Session (Error)
import Network.HTTP.Client (Manager)
import Snap
import Snap.Snaplet.CustomAuth.OAuth2 (Provider(..), OAuth2Settings(..))
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.CustomAuth

import Application hiding (httpManager)
import qualified Backend
import Piperka.Account.Action (privUpdateConfirmed)
import Piperka.OAuth2.Query

piperkaOAuth2
  :: SnapletLens (Snaplet App) SessionManager
  -> Manager
  -> OAuth2Settings MyData AuthID Error App
piperkaOAuth2 s m = OAuth2Settings {
    enabledProviders = [ Reddit, Google ]
  , oauth2Check = Backend.oauth2Check
  , oauth2Login = Backend.oauth2Login
  , oauth2Failure = handleFailure
  , oauth2ActionFailure = handleFailure
  , prepareOAuth2Create = prepareCreate
  , oauth2AccountCreated = accountCreated
  , oauth2LoginDone = loginDone
  , resumeAction = privUpdateConfirmed
  , stateStore = s
  , httpManager = m
  }

handleFailure
  :: Handler App ApiAuth ()
handleFailure = withTop heist $ cRender "_oauth2Failure"

prepareCreate
  :: Provider
  -> Text
  -> Handler App ApiAuth (Either Error AuthID)
prepareCreate provider text = withTop db $ reserveOAuth2Identity provider text

accountCreated
  :: Handler App ApiAuth ()
accountCreated = withTop heist $ cRender "_oauth2AccountCreated"

loginDone
  :: Handler App ApiAuth ()
loginDone = do
  usr <- withTop apiAuth $ currentUser
  redirect' (if isJust usr then "/updates.html" else "/") 303
