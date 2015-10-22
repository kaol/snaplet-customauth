{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application
  ( MyData(..)
  , App(..)
  , AppHandler
  , auth
  , heist
  ) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Lens
import Data.Maybe
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.CustomAuth
import Control.Monad.Trans.Maybe
import Data.Text as T
import Data.Int
import Data.UUID

------------------------------------------------------------------------------

data MyData = MyData
  { uid :: Int32
  , uname :: T.Text
  , usession :: UUID
  , ucsrfToken :: UUID
  } deriving (Show)

data App = App
 { _heist :: Snaplet (Heist App)
 , _auth :: Snaplet (AuthManager MyData App)
 }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

------------------------------------------------------------------------------
type AppHandler = Handler App App
