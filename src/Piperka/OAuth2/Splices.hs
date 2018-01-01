{-# LANGUAGE OverloadedStrings #-}

module Piperka.OAuth2.Splices (renderOAuth2) where

import Control.Monad.State
import Data.Map.Syntax
import Heist
import Heist.Compiled
import Snap.Snaplet
import Snap.Snaplet.CustomAuth

import Application
import Heist.Compiled.Extra (checkedSplice)

data MsgType = Duplicate | Invalid | OldUser
  deriving (Eq)

renderOAuth2
  :: RuntimeAppHandler a
renderOAuth2 _ = (withSplices runChildren oauth2Splices `defer`) $ do
  failData <- lift $ withTop apiAuth getAuthFailData
  return $ case failData of
    Just (Create DuplicateName) -> Duplicate
    Just (Create InvalidName) -> Invalid
    _ -> OldUser

oauth2Splices
  :: Splices (RuntimeAppHandler MsgType)
oauth2Splices = do
  "message" ## const $ runChildren
  "duplicate" ## checkedSplice (== Duplicate)
  "invalid" ## checkedSplice (== Invalid)
  "oldUser" ## checkedSplice (== OldUser)
