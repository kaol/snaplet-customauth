{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Snap.Snaplet.CustomAuth.Handlers where

--import Control.Applicative
import Control.Error.Util
--import Control.Lens
import Control.Monad.Trans
--import Control.Monad.Trans.Either
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Reader
import Control.Monad.State
import Data.Maybe
import Data.ByteString hiding (all)
import Data.Text.Encoding
import Snap
import Data.Map

import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.CustomAuth.AuthManager

loginUser
  :: (UserData u, IAuthBackend u b)
  => (AuthFailure -> Handler b (AuthManager u b) ())
  -> Handler b (AuthManager u b) ()
  -> Handler b (AuthManager u b) ()
loginUser loginFail loginSucc = do
  sesName <- gets sessionCookieName
  usrName <- gets userField
  pwdName <- gets passwordField
  res :: Either AuthFailure u <- runExceptT $ do
    userName <- noteT UsernameMissing $ MaybeT $ (fmap . fmap) decodeUtf8 $ getParam usrName
    passwd <- noteT PasswordMissing $ MaybeT $ (fmap . fmap) decodeUtf8 $ getParam pwdName
    usr <- ExceptT $ login userName passwd
    let udata = extractUser usr
    let wafer = Cookie sesName (encodeUtf8 $ session udata) Nothing Nothing (Just "/") False True
    lift $ modifyResponse $ addResponseCookie wafer
    return usr
  modify $ \mgr -> mgr { activeUser = hush res }
  either loginFail (const loginSucc) res

logoutUser
  :: (UserData u, IAuthBackend u b)
  => Handler b (AuthManager u b) ()
logoutUser = do
  sesName <- gets sessionCookieName
  runMaybeT $ do
    ses <- MaybeT $ getCookie sesName
    lift $ logout (decodeUtf8 $ cookieValue ses)
  modify $ \mgr -> mgr { activeUser = Nothing }
  expireCookie sesName Nothing

recoverSession
  :: (UserData u, IAuthBackend u b)
  => Handler b (AuthManager u b) ()
recoverSession = do
  sesName <- gets sessionCookieName
  usr <- runMaybeT $ do
    ses <- MaybeT $ getCookie sesName
-- TODO
    hushT $ ExceptT $ recover (decodeUtf8 $ cookieValue ses)
  modify $ \mgr -> mgr { activeUser = usr }

-- Recover if session token is present.  Login if login+password are
-- present.
combinedLoginRecover
  :: (UserData u, IAuthBackend u b)
  => (AuthFailure -> Handler b (AuthManager u b) ())
  -> Handler b (AuthManager u b) (Maybe u)
combinedLoginRecover loginFail = do
  usr <- runMaybeT $ do
    lift recoverSession
    MaybeT currentUser
  maybe combinedLogin (return . Just) usr
    where
      combinedLogin = runMaybeT $ do
        usrName <- gets userField
        pwdName <- gets passwordField
        params <- lift $ fmap rqParams getRequest
        when (all (flip member params) [usrName, pwdName]) $ do
          lift $ loginUser loginFail $ return ()
        MaybeT currentUser

authInit
  :: (UserData u, IAuthBackend u b)
  => ByteString
  -> ByteString
  -> ByteString
  -> SnapletInit b (AuthManager u b)
authInit s u p = makeSnaplet "auth" "Custom auth" Nothing $ do
  return $ AuthManager
    { activeUser = Nothing
    , sessionCookieName = s
    , userField = u
    , passwordField = p
    }

currentUser :: UserData u => Handler b (AuthManager u b) (Maybe u)
currentUser = do
  u <- get
  return $ activeUser u

isLoggedIn :: UserData u => Handler b (AuthManager u b) Bool
isLoggedIn = isJust <$> currentUser
