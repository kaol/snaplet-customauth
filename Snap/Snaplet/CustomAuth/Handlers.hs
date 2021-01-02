{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Snap.Snaplet.CustomAuth.Handlers where

import Control.Error.Util hiding (err)
import Control.Lens hiding (un)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.State
import qualified Data.Configurator as C
import qualified Data.HashMap.Lazy as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Snap
import Data.Map

import Snap.Snaplet.CustomAuth.Types hiding (name)
import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.OAuth2.Internal
import Snap.Snaplet.CustomAuth.User (setUser, recoverSession, currentUser, isSessionDefined)
import Snap.Snaplet.CustomAuth.Util (getParamText)

setFailure'
  :: Handler b (AuthManager u e b) ()
  -> AuthFailure e
  -> Handler b (AuthManager u e b) ()
setFailure' action err =
  (modify $ \s -> s { authFailData = Just err }) >> action

loginUser
  :: IAuthBackend u i e b
  => Handler b (AuthManager u e b) ()
  -> Handler b (AuthManager u e b) ()
  -> Handler b (AuthManager u e b) ()
loginUser loginFail loginSucc = do
  usrName <- gets userField
  pwdName <- gets passwordField
  res <- runExceptT $ do
    userName <- noteT (Login UsernameMissing) $ MaybeT $
      (fmap . fmap) decodeUtf8 $ getParam usrName
    passwd <- noteT (Login PasswordMissing) $ MaybeT $
      (fmap . fmap) decodeUtf8 $ getParam pwdName
    usr <- withExceptT UserError $ ExceptT $ login userName passwd
    lift $ maybe (return ()) setUser usr
    hoistEither $ note (Login WrongPasswordOrUsername) usr
  either (setFailure' loginFail) (const loginSucc) res

logoutUser
  :: IAuthBackend u i e b
  => Handler b (AuthManager u e b) ()
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
  => Handler b (AuthManager u e b) ()
  -> Handler b (AuthManager u e b) (Maybe u)
combinedLoginRecover loginFail = do
  sesActive <- isSessionDefined
  usr <- runMaybeT $ do
    guard sesActive
    lift recoverSession
    MaybeT currentUser
  err <- gets authFailData
  maybe (maybe combinedLogin (return . Just) usr)
    (const $ loginFail >> return Nothing) err
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
  => Handler b (AuthManager u e b) (Either (Either e CreateFailure) u)
createAccount = do
  usrName <- ("_new" <>) <$> gets userField
  pwdName <- ("_new" <>) <$> gets passwordField
  let pwdAgainName = pwdName <> "_again"
  usr <- runExceptT $ do
    name <- noteT (Right MissingName) $ MaybeT $
            getParamText usrName
    passwd <- noteT (Right $ PasswordFailure Missing) $ MaybeT $
              getParamText pwdName
    when (T.null passwd) $ throwE (Right $ PasswordFailure Missing)
    noteT (Right $ PasswordFailure Mismatch) $ guard =<<
      (MaybeT $ (fmap . fmap) (== passwd) (getParamText pwdAgainName))
    userId <- either (throwE . Left) return =<<
      (lift $ preparePasswordCreate Nothing passwd)
    return (name, userId)
  res <- runExceptT $ do
    (name, userId) <- hoistEither usr
    u <- ExceptT $ create name userId
    lift $ setUser u
    return u
  case (usr, res) of
    (Right i, Left _) -> cancelPrepare $ snd i
    _ -> return ()
  either (setFailure' (return ()) . either UserError Create) (const $ return ()) res
  return res

authInit
  :: IAuthBackend u i e b
  => Maybe (OAuth2Settings u i e b)
  -> AuthSettings
  -> SnapletInit b (AuthManager u e b)
authInit oa s = makeSnaplet (view authName s) "Custom auth" Nothing $ do
  cfg <- getSnapletUserConfig
  un <- liftIO $ C.lookupDefault "_login" cfg "userField"
  pn <- liftIO $ C.lookupDefault "_password" cfg "passwordField"
  scn <- liftIO $ C.lookupDefault "_session" cfg "sessionCookieName"
  ps <- maybe (return M.empty) oauth2Init oa
  return $ AuthManager
    { activeUser = Nothing
    , cookieLifetime = s ^. authCookieLifetime
    , sessionCookieName = scn
    , userField = un
    , passwordField = pn
    , stateStore' = maybe (error "oauth2 hooks not defined") stateStore oa
    , oauth2Provider = Nothing
    , authFailData = Nothing
    , providers = ps
    }

isLoggedIn :: UserData u => Handler b (AuthManager u e b) Bool
isLoggedIn = isJust <$> currentUser

getAuthFailData
  :: Handler b (AuthManager u e b) (Maybe (AuthFailure e))
getAuthFailData = get >>= return . authFailData

resetAuthFailData
  :: Handler b (AuthManager u e b) ()
resetAuthFailData = modify $ \mgr -> mgr { authFailData = Nothing }
