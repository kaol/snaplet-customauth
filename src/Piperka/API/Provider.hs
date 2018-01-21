{-# LANGUAGE OverloadedStrings #-}

module Piperka.API.Provider (attachProvider) where

import Control.Error.Util
import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Data.Maybe (isNothing)
import Snap
import Snap.Snaplet.CustomAuth
import Snap.Snaplet.CustomAuth.OAuth2 (redirectToProvider, saveAction)
import Snap.Snaplet.Heist

import Application
import Piperka.Account.Types
import Piperka.API.Common
import Piperka.OAuth2.Types
import Piperka.Util (getParamText)

data Result = NotLoggedIn | RedirFailed | Ok

attachProvider
  :: AppHandler ()
attachProvider = do
  provider <- maybe (simpleFail 400 "No valid provider parameter") return .
              (parseProvider =<<) =<< getParamText "provider"
  let p' = providerName provider
  success <- withTop apiAuth $ flip runContT return $ callCC $ \exit -> do
    lift recoverSession
    usr <- lift currentUser
    when (isNothing usr) $ exit NotLoggedIn
    lift $ saveAction False p' $ AttachPayload provider
    return . bool RedirFailed Ok =<< (lift $ redirectToProvider p')
  case success of
    NotLoggedIn -> redirect "/"
    RedirFailed -> cRender "configurationError_"
    Ok -> return ()
