{-# LANGUAGE OverloadedStrings #-}

module Piperka.Messages ( saveMessage, messagesSplices ) where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Data.Map.Syntax
import Data.Maybe (isJust)
import Snap.Snaplet.Session
import Snap
import Heist
import Heist.Compiled as C
import Heist.Compiled.Extra (checkedSplice)

import Application

saveMessage
  :: Text
  -> Handler App v ()
saveMessage msg = do
  withTop messages $ do
    setInSession "p_message" msg
    commitSession

getMessage
  :: Text
  -> Handler App App (Maybe Text)
getMessage n = do
  withTop messages $ runMaybeT $ do
    msg <- MaybeT $ getFromSession n
    lift $ deleteFromSession n >> commitSession
    return msg

messagesSplices :: Splices (C.Splice AppHandler)
messagesSplices = do
  "ifHasMessage" ##
    C.deferMany (C.withSplices C.runChildren messageSplice) $ lift $
    getMessage "p_message"
  "ifHasOAuth2Message" ##
    checkedSplice isJust $ lift $ getMessage "p_has_oauth2"
  where
    messageSplice = "message" ## return . C.yieldRuntimeText
