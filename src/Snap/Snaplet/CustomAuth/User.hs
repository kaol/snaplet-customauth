{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CustomAuth.User (setUser, currentUser, recoverSession, getParamText) where

import Control.Error.Util
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Text.Encoding
import Snap

import Snap.Snaplet.CustomAuth.Types (AuthUser(..))
import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.Util

setUser
  :: UserData u
  => u
  -> Handler b (AuthManager u e b) ()
setUser usr = do
  sesName <- gets sessionCookieName
  let udata = extractUser usr
  -- TODO
  let wafer = Cookie sesName (encodeUtf8 $ session udata) Nothing Nothing (Just "/") False False
  modifyResponse $ addResponseCookie wafer

currentUser :: UserData u => Handler b (AuthManager u e b) (Maybe u)
currentUser = do
  u <- get
  return $ activeUser u

recoverSession
  :: IAuthBackend u i e b
  => Handler b (AuthManager u e b) ()
recoverSession = do
  sesName <- gets sessionCookieName
  usr <- runMaybeT $ do
    ses <- MaybeT $ getCookie sesName
-- TODO
    hushT $ ExceptT $ recover (decodeUtf8 $ cookieValue ses)
  modify $ \mgr -> mgr { activeUser = usr }
