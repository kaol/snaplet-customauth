{-# LANGUAGE OverloadedStrings #-}
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
  , WebAuth
  , ApiAuth
  , AnyAuth
  , AppInit(..)
  , auth
  , apiAuth
  , heist
  , db
  , messages
  , taglookup
  , extlookup
  , httpManager
  , suppressError
  , minimal
  , accountUpdateError
  , actionResult
  , submitResult
  , adsEnabled
  , defaultUserPrefs
  , defaultUserStats
  , getPrefs
  , sessionCookieName
  ) where

------------------------------------------------------------------------------

import Piperka.Listing.Types (ViewColumns)

import Control.Lens
import Data.ByteString (ByteString)
import Data.Text as T
import Data.Int
import Data.UUID
import Control.Monad.State
import qualified Hasql.Session
import Heist (RuntimeSplice)
import Heist.Compiled (Splice)
import Network.HTTP.Client (Manager)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.CustomAuth hiding (sessionCookieName)
import Snap.Snaplet.Hasql
import Snap.Snaplet.Session

import Piperka.Account.Types (AccountUpdateError)
import Piperka.Action.Types
import Piperka.API.Submit.Types (SubmitResult)
import Piperka.ComicInfo.Types
import Piperka.Listing.Types (ViewColumns(..))
------------------------------------------------------------------------------

type UserID = Int32

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

type WebAuth = AuthManager UserWithStats Hasql.Session.Error App
type ApiAuth = AuthManager MyData Hasql.Session.Error App
type AnyAuth u = AuthManager u Hasql.Session.Error App

data App = App
  { _heist :: Snaplet (Heist App)
  , _auth :: Snaplet WebAuth
  , _apiAuth :: Snaplet ApiAuth
  , _db :: Snaplet Hasql
  , _messages :: Snaplet SessionManager
  , _extlookup :: Int -> Text -> Maybe ExternalEntry
  , _taglookup :: [Int] -> [ComicTag]
  , _httpManager :: Manager
  -- Used when there's a template specific way of presenting an error.
  , _suppressError :: Bool
  , _minimal :: Bool
  , _accountUpdateError :: Maybe AccountUpdateError
  , _actionResult :: Maybe (Maybe ActionError, Maybe Action)
  , _submitResult :: Maybe SubmitResult
  , _adsEnabled :: Bool
  }

data AppInit = AppInit
  { extFormPart :: Text
  , tagFormPart :: Text
  , scriptHash :: [(Text, Text)]
  }
  deriving (Show)

defaultUserPrefs :: UserPrefs
defaultUserPrefs = UserPrefs
  { rows = 40
  , columns = TwoColumn
  , newExternWindows = False
  }

defaultUserStats :: MyData -> UserWithStats
defaultUserStats = UserWithStats 0 (0, 0) Nothing

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasHasql (Handler App v) where
  getHasqlState = withTop db get
  setHasqlState s = withTop db $ put s

sessionCookieName :: ByteString
sessionCookieName = "p_session"

------------------------------------------------------------------------------
type AppHandler = Handler App App
type RuntimeAppHandler a = RuntimeSplice AppHandler a -> Splice AppHandler
