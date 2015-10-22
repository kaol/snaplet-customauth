{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Snap.Snaplet.CustomAuth.Handlers where

import Control.Applicative
import Control.Error.Util
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.State
import Data.Maybe
import Data.ByteString
import qualified Data.Text as T
import Data.Text.Encoding
import Snap

import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.CustomAuth.AuthManager

loginUser
  :: UserData u
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
    usr <- ExceptT $ withBackend $ \r -> liftIO $ login r userName passwd
    let udata = extractUser usr
    let wafer = Cookie sesName (encodeUtf8 $ session udata) Nothing Nothing (Just "/") False True
    lift $ modifyResponse $ addResponseCookie wafer
    return usr
  modify $ \mgr -> mgr { activeUser = hush res }
  either loginFail (const loginSucc) res

logoutUser :: UserData u => Handler b (AuthManager u b) ()
logoutUser = do
  sesName <- gets sessionCookieName
  runMaybeT $ do
    ses <- MaybeT $ getCookie sesName
    lift $ withBackend $ \r -> liftIO $ logout r (decodeUtf8 $ cookieValue ses)
  modify $ \mgr -> mgr { activeUser = Nothing }
  expireCookie sesName Nothing

recoverSession :: UserData u => Handler b (AuthManager u b) ()
recoverSession = do
  sesName <- gets sessionCookieName
  usr <- runMaybeT $ do
    ses <- MaybeT $ getCookie sesName
-- TODO
    hushT $ ExceptT $ withBackend $ \r -> liftIO $ recover r (decodeUtf8 $ cookieValue ses)
  modify $ \mgr -> mgr { activeUser = usr }

authInit
  :: (UserData u, IAuthBackend u r)
  => r
  -> ByteString
  -> ByteString
  -> ByteString
  -> SnapletInit b (AuthManager u b)
authInit b s u p = makeSnaplet "auth" "Custom auth" Nothing $ do
  return $ AuthManager
    { backend = b
    , activeUser = Nothing
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

withBackend
  :: UserData u
  => (forall r. (IAuthBackend u r) => r -> Handler b (AuthManager u v) a)
  -> Handler b (AuthManager u v) a
withBackend f = join $ do
  (AuthManager backend_ _ _ _ _) <- get
  return $ f backend_
