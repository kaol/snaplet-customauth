{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application
  ( MyData(..)
  , UserPrefs(..)
  , UserPrefsWithStats(..)
  , App(..)
  , AppHandler
  , RuntimeAppHandler
  , auth
  , apiAuth
  , heist
  , db
  , messages
  , taglookup
  , extlookup
  ) where

------------------------------------------------------------------------------

import Piperka.Listing.Types (ViewColumns)

--import Control.Applicative
import Control.Lens
--import Data.Maybe
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.CustomAuth
import Snap.Snaplet.Hasql
import Snap.Snaplet.Session
import Data.Text as T
import Data.Int
import Data.UUID
import Control.Monad.State
import Heist (RuntimeSplice)
import Heist.Compiled (Splice)

import Piperka.ComicInfo.Types
------------------------------------------------------------------------------

data MyData = MyData
  { uid :: Int32
  , uname :: T.Text
  , usession :: UUID
  , ucsrfToken :: UUID
  } deriving (Show)

data UserPrefs = UserPrefs
  { user :: Maybe MyData
  , rows :: Int32
  , columns :: ViewColumns
  , newExternWindows :: Bool
  } deriving (Show)

data UserPrefsWithStats = UserPrefsWithStats
  { newComics :: Int32
  , unreadCount :: (Int32,Int32)
  , prefs :: UserPrefs
  } deriving (Show)

data App = App
  { _heist :: Snaplet (Heist App)
  , _auth :: Snaplet (AuthManager UserPrefsWithStats App)
  , _apiAuth :: Snaplet (AuthManager UserPrefs App)
  , _db :: Snaplet Hasql
  , _messages :: Snaplet SessionManager
  , _extlookup :: Int -> Text -> Maybe ExternalEntry
  , _taglookup :: [Int] -> [ComicTag]
  }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasHasql (Handler b App) where
  getHasqlState = with db get
  setHasqlState = with db . put

instance HasHasql (Handler App Hasql) where
  getHasqlState = withTop db get
  setHasqlState s = withTop db $ put s

------------------------------------------------------------------------------
type AppHandler = Handler App App
type RuntimeAppHandler a = RuntimeSplice AppHandler a -> Splice AppHandler
