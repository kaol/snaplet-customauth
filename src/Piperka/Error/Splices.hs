{-# LANGUAGE OverloadedStrings #-}

module Piperka.Error.Splices where

import Data.Text (pack)
import Hasql.Session
import Heist.Compiled
import Heist
import Data.Map.Syntax

import Application(AppHandler)

-- TODO: log
sqlErrorSplices
  :: Splices (RuntimeSplice AppHandler Error -> Splice AppHandler)
sqlErrorSplices = do
  "sqlError" ## pureSplice . textSplice $ pack . show
