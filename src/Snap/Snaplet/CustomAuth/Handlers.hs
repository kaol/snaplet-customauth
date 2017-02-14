{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Snap.Snaplet.CustomAuth.Handlers where

import Control.Error.Util
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Data.ByteString hiding (all)
import Data.Text.Encoding
import Snap
import Data.Map

import Snap.Snaplet.CustomAuth.Types hiding (name)
import Snap.Snaplet.CustomAuth.AuthManager

setUser
  :: UserData u
  => u
  -> Handler b (AuthManager u b) ()
setUser usr = do
  sesName <- gets sessionCookieName
  let udata = extractUser usr
  -- TODO
  let wafer = Cookie sesName (encodeUtf8 $ session udata) Nothing (Just "localhost") (Just "/") False False
  modifyResponse $ addResponseCookie wafer

loginUser
  :: IAuthBackend u e b
  => (AuthFailure e -> Handler b (AuthManager u b) ())
  -> Handler b (AuthManager u b) ()
  -> Handler b (AuthManager u b) ()
loginUser loginFail loginSucc = do
  usrName <- gets userField
  pwdName <- gets passwordField
  res :: Either (AuthFailure e) u <- runExceptT $ do
    userName <- noteT UsernameMissing $ MaybeT $ (fmap . fmap) decodeUtf8 $ getParam usrName
    passwd <- noteT PasswordMissing $ MaybeT $ (fmap . fmap) decodeUtf8 $ getParam pwdName
    usr <- ExceptT $ login userName passwd
    lift $ setUser usr
    return usr
  modify $ \mgr -> mgr { activeUser = hush res }
  either loginFail (const loginSucc) res

logoutUser
  :: IAuthBackend u e b
  => Handler b (AuthManager u b) ()
logoutUser = do
  sesName <- gets sessionCookieName
  runMaybeT $ do
    ses <- MaybeT $ getCookie sesName
    lift $ expireCookie ses >> logout (decodeUtf8 $ cookieValue ses)
  modify $ \mgr -> mgr { activeUser = Nothing }

recoverSession
  :: IAuthBackend u e b
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
  :: IAuthBackend u e b
  => (AuthFailure e -> Handler b (AuthManager u b) ())
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

createAccount
  :: (Eq e, Show e, IAuthBackend u e b)
  => Handler b (AuthManager u b) (Either (CreateFailure e) u)
createAccount = do
  usrName <- ("_new" <>) <$> gets userField
  pwdName <- ("_new" <>) <$> gets passwordField
  let pwdAgainName = pwdName <> "_again"
  res :: Either (CreateFailure e) u <- runExceptT $ do
    name <- noteT MissingName $ MaybeT $
            (fmap . fmap) decodeUtf8 $ getParam usrName
    avail <- either (throwE . AvailError) return =<< lift (check name)
    when (not avail) $ throwE NameUsed
    passwd <- noteT NoPassword $ MaybeT $ getParam pwdName
    when (Data.ByteString.null passwd) $ throwE NoPassword
    noteT PasswordMismatch $ guard =<<
      (MaybeT $ (fmap . fmap) (== passwd) (getParam pwdAgainName))
    usr <- ExceptT $ create name (decodeUtf8 passwd)
    lift $ setUser usr
    return usr
  modify $ \mgr -> mgr { activeUser = hush res }
  return res

authInit
  :: IAuthBackend u e b
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
