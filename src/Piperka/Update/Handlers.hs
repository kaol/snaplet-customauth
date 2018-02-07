{-# LANGUAGE OverloadedStrings #-}

module Piperka.Update.Handlers (mayRedir) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.ByteString.Char8 as B8
import Data.UUID (fromASCIIBytes)
import Hasql.Session (query)
import Snap.Core
import Snap.Snaplet.Hasql

import Application
import Piperka.Auth (currentUserPlain)
import Piperka.Update.Statements (updateAndRedirect)
import Piperka.Util (getParamInt)

mayRedir
  :: AppHandler ()
mayRedir = void $ runMaybeT $ do
  usr <- MaybeT currentUserPlain
  csrf <- MaybeT $ getParam "csrf_ham"
  guard (fromASCIIBytes csrf == (Just $ ucsrfToken usr))
  redir <- MaybeT $ (fmap (fromIntegral . snd)) <$> getParamInt "redir"
  offset <- maybe False (== "1") <$> lift (getParam "offset_back")
  r <- lift $ run $ query (uid usr, redir, offset) updateAndRedirect
  either (\e -> lift $ logError (B8.pack $ show e))
    (\url' -> do
        url <- MaybeT . return $ url'
        lift $ do
          modifyResponse $ setHeader "Referrer-policy" "origin"
          redirect url) r
