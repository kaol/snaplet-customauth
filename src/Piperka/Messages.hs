{-# LANGUAGE OverloadedStrings #-}

module Piperka.Messages ( saveMessage, messagesSplices ) where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Data.Map.Syntax
import Snap.Snaplet.Session
import Snap
import Heist
import Heist.Compiled as C

import Application

saveMessage
  :: Text
  -> Handler App v ()
saveMessage msg = do
  withTop messages $ do
    setInSession "p_message" msg
    commitSession

getMessage
  :: Handler App App (Maybe Text)
getMessage = do
  withTop messages $ runMaybeT $ do
    msg <- MaybeT $ getFromSession "p_message"
    lift $ resetSession >> commitSession
    return msg

messagesSplices :: Splices (C.Splice AppHandler)
messagesSplices =
  "ifHasMessage" ## do
    C.deferMany (C.withSplices C.runChildren messageSplice) $ lift getMessage
  where
    messageSplice = "message" ## return . C.yieldRuntimeText
