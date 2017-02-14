{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CustomAuth.Heist where

import Control.Lens
import Control.Monad.Trans
import qualified Text.XmlHtml as X
import Heist
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.CustomAuth.Handlers
import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.CustomAuth.AuthManager
import Data.Map.Syntax


addAuthSplices
  :: UserData u
  => Snaplet (Heist b)
  -> SnapletLens b (AuthManager u b)
  -> Initializer b v ()
addAuthSplices h auth = addConfig h sc
  where
    sc = mempty & scInterpretedSplices .~ is
                & scCompiledSplices .~ cs
    is = do
      "ifLoggedIn" ## ifLoggedIn auth
      "ifLoggedOut" ## ifLoggedOut auth
      "loggedInUser" ## loggedInUser auth
    cs = compiledAuthSplices auth

compiledAuthSplices
  :: UserData u
  => SnapletLens b (AuthManager u b)
  -> Splices (SnapletCSplice b)
compiledAuthSplices auth = do
    "ifLoggedIn"   ## cIfLoggedIn auth
    "ifLoggedOut"  ## cIfLoggedOut auth
    "loggedInUser" ## cLoggedInUser auth

ifLoggedIn
  :: UserData u
  => SnapletLens b (AuthManager u b)
  -> SnapletISplice b
ifLoggedIn auth = do
  chk <- lift $ withTop auth isLoggedIn
  case chk of
   True -> getParamNode >>= return . X.childNodes
   False -> return []


cIfLoggedIn
  :: UserData u
  => SnapletLens b (AuthManager u b)
  -> SnapletCSplice b
cIfLoggedIn auth = do
    cs <- C.runChildren
    return $ C.yieldRuntime $ do
        chk <- lift $ withTop auth isLoggedIn
        case chk of
          True -> C.codeGen cs
          False -> mempty

ifLoggedOut
  :: UserData u
  => SnapletLens b (AuthManager u b)
  -> SnapletISplice b
ifLoggedOut auth = do
    chk <- lift $ withTop auth isLoggedIn
    case chk of
      False -> getParamNode >>= return . X.childNodes
      True -> return []

cIfLoggedOut
  :: UserData u
  => SnapletLens b (AuthManager u b)
  -> SnapletCSplice b
cIfLoggedOut auth = do
    cs <- C.runChildren
    return $ C.yieldRuntime $ do
        chk <- lift $ withTop auth isLoggedIn
        case chk of
          False -> C.codeGen cs
          True -> mempty

loggedInUser
  :: UserData u
  => SnapletLens b (AuthManager u b)
  -> SnapletISplice b
loggedInUser auth = do
    u <- lift $ withTop auth currentUser
    maybe (return []) (I.textSplice . name . extractUser) $ u

cLoggedInUser
  :: UserData u
  => SnapletLens b (AuthManager u b)
  -> SnapletCSplice b
cLoggedInUser auth =
    return $ C.yieldRuntimeText $ do
        u <- lift $ withTop auth currentUser
        return $ maybe "" (name . extractUser) u 
