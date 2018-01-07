{-# LANGUAGE OverloadedStrings #-}

module Piperka.OAuth2 (piperkaOAuth2) where

import Control.Lens (set)
import Control.Monad.State
import Data.Monoid
import Data.Text (Text)
import Hasql.Session (Error)
import Network.HTTP.Client (Manager)
import Snap
import Snap.Snaplet.CustomAuth.OAuth2
import Snap.Snaplet.Hasql (bracketDbOpen)
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.CustomAuth
import Snap.Snaplet.CustomAuth.User (setUser)

import Application hiding (httpManager)
import qualified Backend
import Piperka.Auth
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
  , prepareOAuth2Create = prepareCreate
  , oauth2AccountCreated = accountCreated
  , oauth2LoginDone = loginDone
  , resumeAction = privUpdateConfirmed
  , stateStore = s
  , httpManager = m
  , bracket = bracketDbOpen
  }

handleFailure
  :: OAuth2Stage
  -> Handler App ApiAuth ()
handleFailure SCreate = do
  failData <- withTop apiAuth getAuthFailData
  liftIO $ print ("handlefailure SCreate " <> (show failData))
  let retry = do
        withTop' id $ modify $ set suppressError True
        cRender "newUserSelectName_"
  case failData of
    Just (Create DuplicateName) -> retry
    Just (Create InvalidName) -> retry
    _ -> withTop' id $ authHandler False $ cRender "oauth2Failure_"
handleFailure a = withTop' id $ authHandler False $ do
  failData <- withTop apiAuth getAuthFailData
  liftIO $ print ("handlefailure " <> (show a) <> " " <> (show failData))
  cRender "oauth2Failure_"

prepareCreate
  :: Provider
  -> Text
  -> Handler App ApiAuth (Either Error AuthID)
prepareCreate provider text = withTop db $ reserveOAuth2Identity provider text

accountCreated
  :: MyData
  -> Handler App ApiAuth ()
accountCreated usr = do
  withTop auth $ setUser $ defaultUserStats usr
  cRender "oauth2AccountCreated_"

loginDone
  :: Handler App ApiAuth ()
loginDone = do
  usr <- withTop apiAuth $ currentUser
  liftIO $ print ("logindone " <> (show usr))
  maybe (cRender "newUserSelectName_") (const $ redirect' "/updates.html" 303) usr
