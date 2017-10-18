{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application
  ( MyData(..)
  , UserPrefs(..)
  , UserWithStats(..)
  , App(..)
  , AppHandler
  , RuntimeAppHandler
  , UserID
  , AuthID
  , AppInit(..)
  , auth
  , apiAuth
  , heist
  , db
  , messages
  , taglookup
  , extlookup
  , httpManager
  , defaultUserPrefs
  , getPrefs
  ) where

------------------------------------------------------------------------------

import Piperka.Listing.Types (ViewColumns)

import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.CustomAuth hiding (httpManager)
import Snap.Snaplet.Hasql
import Snap.Snaplet.Session
import Data.Text as T
import Data.Int
import Data.UUID
import Control.Monad.State
import Heist (RuntimeSplice)
import Heist.Compiled (Splice)
import Network.HTTP.Client (Manager)

import Piperka.ComicInfo.Types
import Piperka.Listing.Types (ViewColumns(..))
------------------------------------------------------------------------------

type UserID = Int32
type AuthID = Int32

data MyData = MyData
  { uid :: UserID
  , uname :: T.Text
  , usession :: UUID
  , ucsrfToken :: UUID
  , moderator :: Bool
  , prefs :: UserPrefs
  } deriving (Show)

data UserPrefs = UserPrefs
  { rows :: Int32
  , columns :: ViewColumns
  , newExternWindows :: Bool
  } deriving (Show)

data UserWithStats = UserWithStats
  { newComics :: Int32
  , unreadCount :: (Int32,Int32)
  , modStats :: Maybe (Int32, Int32)
  , user :: MyData
  } deriving (Show)

class HasPrefs u where
  getPrefs :: u -> UserPrefs

instance HasPrefs MyData where
  getPrefs = prefs

instance HasPrefs UserPrefs where
  getPrefs = id

instance HasPrefs UserWithStats where
  getPrefs = prefs . user

instance HasPrefs (Maybe MyData) where
  getPrefs (Just u) = prefs u
  getPrefs Nothing = defaultUserPrefs

data App = App
  { _heist :: Snaplet (Heist App)
  , _auth :: Snaplet (AuthManager UserWithStats App)
  , _apiAuth :: Snaplet (AuthManager MyData App)
  , _db :: Snaplet Hasql
  , _messages :: Snaplet SessionManager
  , _extlookup :: Int -> Text -> Maybe ExternalEntry
  , _taglookup :: [Int] -> [ComicTag]
  , _httpManager :: Manager
  }

data AppInit = AppInit
  { extFormPart :: Text
  , tagFormPart :: Text
  }

defaultUserPrefs :: UserPrefs
defaultUserPrefs = UserPrefs
  { rows = 40
  , columns = TwoColumn
  , newExternWindows = False
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
