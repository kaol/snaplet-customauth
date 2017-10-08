{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Snap.Snaplet.CustomAuth.Util where

import Control.Error.Util
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import Snap hiding (path)

import Snap.Snaplet.CustomAuth.AuthManager

getStateName
  :: Handler b (AuthManager u b) Text
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

