{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CustomAuth.Handlers.OAuth2.Splices (addOAuth2Splices) where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.State
import Data.Map.Syntax
import Data.Maybe
import Data.Monoid
import Heist
import Heist.Compiled
import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Session

import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.Util

addOAuth2Splices
  :: Snaplet (Heist b)
  -> SnapletLens b (AuthManager u b)
  -> Initializer b v ()
addOAuth2Splices h auth = addConfig h sc
  where
    sc = mempty & scCompiledSplices .~ cs
    cs = do
      "ifHaveOAuth2Token" ## spliceOAuth2Token True auth
      "ifNoOAuth2Token" ## spliceOAuth2Token False auth

spliceOAuth2Token
  :: Bool
  -> SnapletLens b (AuthManager u b)
  -> SnapletCSplice b
spliceOAuth2Token t auth = do
  cs <- runChildren
  return $ yieldRuntime $ do
    name <- lift $ (<> "_token") <$> withTop auth getStateName
    store <- lift $ withTop auth $ gets stateStore'
    chk <- lift $ withTop' store $ (fmap isJust $ getFromSession name)
    if chk == t then codeGen cs else mempty
