{-# LANGUAGE OverloadedStrings #-}

module Piperka.Error.Splices where

import Data.Text (pack)
import Hasql.Session
import Heist.Compiled
import Heist
import Data.Map.Syntax

import Application(RuntimeAppHandler)

-- TODO: log
sqlErrorSplices
  :: Splices (RuntimeAppHandler Error)
sqlErrorSplices = do
  "sqlError" ## pureSplice . textSplice $ pack . show

stdSqlErrorSplice
  :: RuntimeAppHandler Error
stdSqlErrorSplice = withSplices (callTemplate "_sqlErr") sqlErrorSplices
