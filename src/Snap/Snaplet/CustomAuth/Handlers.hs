{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Snap.Snaplet.CustomAuth.Handlers where

import Control.Error.Util
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Snap
import Data.Map

import Snap.Snaplet.CustomAuth.Types hiding (name)
import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.Handlers.OAuth2
import Snap.Snaplet.CustomAuth.User (setUser, recoverSession, currentUser, getParamText)

loginUser
  :: IAuthBackend u i e b
  => (Either e AuthFailure -> Handler b (AuthManager u b) ())
  -> Handler b (AuthManager u b) ()
  -> Handler b (AuthManager u b) ()
loginUser loginFail loginSucc = do
  usrName <- gets userField
  pwdName <- gets passwordField
  res <- runExceptT $ do
    userName <- noteT (Right UsernameMissing) $ MaybeT $
      (fmap . fmap) decodeUtf8 $ getParam usrName
    passwd <- noteT (Right PasswordMissing) $ MaybeT $
      (fmap . fmap) decodeUtf8 $ getParam pwdName
    usr <- withExceptT Left $ ExceptT $ login userName passwd
    lift $ maybe (return ()) setUser usr
    return usr
  modify $ \mgr -> mgr { activeUser = join $ hush res }
  either loginFail (const loginSucc) res

logoutUser
  :: IAuthBackend u i e b
  => Handler b (AuthManager u b) ()
logoutUser = do
  sesName <- gets sessionCookieName
  runMaybeT $ do
    ses <- MaybeT $ getCookie sesName
    lift $ expireCookie ses >> logout (decodeUtf8 $ cookieValue ses)
  modify $ \mgr -> mgr { activeUser = Nothing }

-- Recover if session token is present.  Login if login+password are
-- present.
combinedLoginRecover
  :: IAuthBackend u i e b
  => (Either e AuthFailure -> Handler b (AuthManager u b) ())
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

-- Account with password login
createAccount
  :: IAuthBackend u i e b
  => Handler b (AuthManager u b) (Either (Either e CreateFailure) u)
createAccount = do
  usrName <- ("_new" <>) <$> gets userField
  pwdName <- ("_new" <>) <$> gets passwordField
  let pwdAgainName = pwdName <> "_again"
  usr <- runExceptT $ do
    name <- noteT (Right MissingName) $ MaybeT $
            (fmap . fmap) decodeUtf8 $ getParam usrName
    passwd <- noteT (Right $ PasswordFailure Missing) $ MaybeT $
              (hush . decodeUtf8' =<<) <$> getParam pwdName
    when (T.null passwd) $ throwE (Right $ PasswordFailure Missing)
    noteT (Right $ PasswordFailure Mismatch) $ guard =<<
      (MaybeT $ (fmap . fmap) (== passwd) (getParamText pwdAgainName))
    userId <- either (throwE . Left) return =<< (lift $ preparePasswordCreate Nothing passwd)
    return (name, userId)
  res <- runExceptT $ do
    (name, userId) <- hoistEither usr
    u <- ExceptT $ create name userId
    lift $ setUser u
    return u
  modify $ \mgr -> mgr { activeUser = hush res }
  case (usr, res) of
    (Right i, Left _) -> cancelPrepare $ snd i
    _ -> return ()
  return res

authInit
  :: IAuthBackend u i e b
  => Maybe (OAuth2Settings u i e b)
  -> AuthSettings
  -> SnapletInit b (AuthManager u b)
authInit oa s = makeSnaplet (view authName s) "Custom auth" Nothing $ do
  maybe (return ()) oauth2Init oa
  return $ AuthManager
    { activeUser = Nothing
    , sessionCookieName = s ^. authSessionCookieName
    , userField = s ^. authUserField
    , passwordField = s ^. authPasswordField
    , stateStore' = maybe (error "oauth2 hooks not defined") stateStore oa
    , getKey' = maybe (error "oauth2 hooks not defined") getKey oa
    }

isLoggedIn :: UserData u => Handler b (AuthManager u b) Bool
isLoggedIn = isJust <$> currentUser
