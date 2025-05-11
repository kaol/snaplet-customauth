{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Snap.Snaplet.Session2.SessionManager where

import Data.Text
import Prelude hiding (lookup)
import Snap

data SessionManager b = forall r. ISessionManager r b => SessionManager r

class ISessionManager r b | r -> b where
  load :: r -> Handler b (SessionManager b) r
  commit :: r -> Handler b (SessionManager b) ()
  reset :: r -> Handler b (SessionManager b) r
  touch :: r -> r
  insert :: Text -> Text -> r -> r
  lookup :: Text -> r -> Maybe Text
  delete :: Text -> r -> r
  csrf :: r -> Text
  toList :: r -> [(Text, Text)]
