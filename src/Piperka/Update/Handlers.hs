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
import Piperka.API.Login (tokenLogin)
import Piperka.Auth (currentUserPlain)
import Piperka.Update.Statements (updateAndRedirect)
import Piperka.Util (getParamInt)

mayRedir
  :: AppHandler ()
mayRedir = maybe unloggedRedir loggedRedir =<< currentUserPlain

-- TODO: Show an error to user.
hushAndReport
  :: Show e
  => AppHandler (Either e (Maybe a))
  -> MaybeT AppHandler a
hushAndReport = (either
                 ((\e -> (lift $ logError $ B8.pack $ show e) >> mzero))
                 (MaybeT . return) =<<) . lift

-- Piperka App can't pass along session cookie, act with only csrf
-- token for them.
unloggedRedir
  :: AppHandler ()
unloggedRedir = void $ runMaybeT $ do
  csrf <- MaybeT $ (fromASCIIBytes =<<) <$> getParam "csrf_ham"
  uid' <- hushAndReport $ tokenLogin csrf
  redir uid'

loggedRedir
  :: MyData
  -> AppHandler ()
loggedRedir usr = void $ runMaybeT $ do
  csrf <- MaybeT $ getParam "csrf_ham"
  guard (fromASCIIBytes csrf == (Just $ ucsrfToken usr))
  redir $ uid usr

redir
  :: UserID
  -> MaybeT AppHandler a
redir usr = do
  cid <- MaybeT $ (fmap (fromIntegral . snd)) <$> getParamInt "redir"
  offset <- maybe False (== "1") <$> lift (getParam "offset_back")
  url <- hushAndReport $ run $ query (usr, cid, offset) updateAndRedirect
  lift $ do
    modifyResponse $ setHeader "Referrer-policy" "origin"
    redirect url
