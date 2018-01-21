{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Snap.Snaplet.CustomAuth.Util where

import Control.Error.Util
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import Snap hiding (path)

import Snap.Snaplet.CustomAuth.AuthManager

getStateName
  :: Handler b (AuthManager u e b) Text
getStateName = do
  path <- maybe "auth" id . hush . decodeUtf8' <$> getSnapletRootURL
  name <- maybe "auth" id <$> getSnapletName
  return $ "__" <> name <> "_" <> path <> "_state"

getParamText
  :: forall (f :: * -> *).
     MonadSnap f
  => ByteString
  -> f (Maybe Text)
getParamText n = (hush . decodeUtf8' =<<) <$> getParam n

setFailure
  :: Handler b (AuthManager u e b) ()
  -> Maybe Text
  -> Either e (AuthFailure e)
  -> Handler b (AuthManager u e b) ()
setFailure action provider failure = do
  let failure' = either UserError id failure
  modify $ \s -> s { oauth2Provider = provider
                   , authFailData = Just failure'
                   }
  action
