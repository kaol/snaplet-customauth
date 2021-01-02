{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CustomAuth.User where

import Control.Error.Util
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.Configurator as C
import Data.Maybe
import Data.Text.Encoding
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Snap

import Snap.Snaplet.CustomAuth.Types (AuthUser(..))
import Snap.Snaplet.CustomAuth.AuthManager

setUser
  :: UserData u
  => u
  -> Handler b (AuthManager u e b) ()
setUser usr = do
  cfg <- getSnapletUserConfig
  secure <- fmap (== "https") $ liftIO $ C.lookupDefault ("https" :: ByteString) cfg "protocol"
  let udata = extractUser usr
  (name, lifetime) <- gets ((,) <$> sessionCookieName <*> cookieLifetime)
  cookieTime <- runMaybeT $ do
    diff <- MaybeT $ pure lifetime
    liftIO $ addUTCTime diff <$> getCurrentTime
  let wafer = Cookie name (session udata) cookieTime Nothing (Just "/") secure True
  modifyResponse $ addResponseCookie wafer
  modify $ \mgr -> mgr { activeUser = Just usr }

currentUser :: UserData u => Handler b (AuthManager u e b) (Maybe u)
currentUser = do
  u <- get
  return $ activeUser u

setFailure'
  :: AuthFailure e
  -> Handler b (AuthManager u e b) ()
setFailure' failure = modify $ \mgr -> mgr { authFailData = Just failure }

recoverSession
  :: IAuthBackend u i e b
  => Handler b (AuthManager u e b) ()
recoverSession = do
  sesName <- gets sessionCookieName
  let quit e = do
        ses <- getCookie sesName
        maybe (return ()) expireCookie ses
        setFailure' e
  usr <- runMaybeT $ do
    val <- MaybeT $ ((hush . decodeUtf8' . cookieValue =<<) <$> getCookie sesName)
    lift $ recover val
  modify $ \mgr -> mgr { activeUser = join $ hush <$> usr }
  maybe (return ()) (either quit (const $ return ())) usr

-- Just check if the session cookie is defined
isSessionDefined
  :: Handler b (AuthManager u e b) Bool
isSessionDefined = gets sessionCookieName >>= getCookie >>= return . isJust
