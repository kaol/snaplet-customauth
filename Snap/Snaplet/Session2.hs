{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Snaplet.Session2 where

import Control.Monad.State
import Data.Text
import Snap.Snaplet
import Snap.Snaplet.Session2.SessionManager

withSession :: SnapletLens b (SessionManager b) -> Handler b v a -> Handler b v a
withSession l h = do
  a <- h
  withTop l commitSession
  return a

commitSession :: Handler b (SessionManager b) ()
commitSession = do
  SessionManager b <- loadSession
  commit b

setInSession :: Text -> Text -> Handler b (SessionManager b) ()
setInSession k v = do
  SessionManager r <- loadSession
  let r' = insert k v r
  put $ SessionManager r'

getFromSession :: Text -> Handler b (SessionManager b) (Maybe Text)
getFromSession k = do
  SessionManager r <- loadSession
  return $ Snap.Snaplet.Session2.SessionManager.lookup k r

deleteFromSession :: Text -> Handler b (SessionManager b) ()
deleteFromSession k = do
  SessionManager r <- loadSession
  let r' = delete k r
  put $ SessionManager r'

csrfToken :: Handler b (SessionManager b) Text
csrfToken = do
  mgr@(SessionManager r) <- loadSession
  put mgr
  return $ csrf r

sessionToList :: Handler b (SessionManager b) [(Text, Text)]
sessionToList = do
  SessionManager r <- loadSession
  return $ toList r

resetSession :: Handler b (SessionManager b) ()
resetSession = do
  SessionManager r <- loadSession
  r' <- reset r
  put $ SessionManager r'

touchSession :: Handler b (SessionManager b) ()
touchSession = do
  SessionManager r <- loadSession
  let r' = touch r
  put $ SessionManager r'

loadSession :: Handler b (SessionManager b) (SessionManager b)
loadSession = do
  SessionManager r <- get
  r' <- load r
  return $ SessionManager r'
