{-# LANGUAGE OverloadedStrings #-}

module Piperka.Auth (authHandler, currentUserPlain, mayCreateAccount, setCSRFCookie) where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Maybe (isJust)
import Data.IORef
import Data.UUID (UUID, toASCIIBytes)
import Snap
import Snap.Snaplet.CustomAuth
import Snap.Snaplet.CustomAuth.User (setUser)
import Snap.Snaplet.Heist

import Application
import Backend ()
import Piperka.Action

authHandler
  :: Bool
  -> AppHandler ()
  -> AppHandler ()
authHandler alwaysMinimal action = do
  failed <- liftIO $ newIORef False
  let loginFailed = withTop' id $ liftIO $ writeIORef failed True
  useMinimal <- if alwaysMinimal then return True
    else any isJust <$> mapM getParam ["min", "redir"]
  modify $ set minimal useMinimal
  case useMinimal of
    True -> (withTop apiAuth $ combinedLoginRecover loginFailed) >> return ()
    False -> (withTop auth (combinedLoginRecover loginFailed)) >>
      withTop auth currentUser >>= processAction >>=
      (\(act, usr) -> do
          modify $ set actionResult act
          maybe (return ()) (withTop auth . setUser) usr)
  isFailed <- liftIO $ readIORef failed
  maybe (return ()) (setCSRFCookie . ucsrfToken) =<< currentUserPlain
  if isFailed then cRender "loginFailed_" else action

setCSRFCookie
  :: UUID
  -> AppHandler ()
setCSRFCookie token = modifyResponse $ addResponseCookie $
  Cookie "csrf_ham" (toASCIIBytes token)
  Nothing Nothing (Just "/") False False


currentUserPlain
  :: AppHandler (Maybe MyData)
currentUserPlain =
  withTop apiAuth currentUser >>=
  maybe ((fmap user) <$> withTop auth currentUser) (return . Just)

mayCreateAccount
  :: AppHandler ()
  -> AppHandler ()
mayCreateAccount action = maybe action id =<< runMaybeT
  (do
      -- Do nothing if there's a session cookie
      guard =<< (lift $ not <$> withTop auth isSessionDefined)
      guard =<< (lift $ (== (Just "Create account")) <$> getParam "action")
      return $ do
        res <- withTop auth createAccount
        let nextPage = either (const "newuser.html") (const "welcome_") res
        cRender nextPage)
