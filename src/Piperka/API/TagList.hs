{-# LANGUAGE OverloadedStrings #-}

module Piperka.API.TagList (tagList) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Hasql.Query
import Hasql.Session (query)
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Snap
import Snap.Snaplet.Hasql

import Application
import Piperka.API.Common
import Piperka.Util (getParamInt)

tagList
  :: AppHandler ()
tagList = do
  t <- maybe (simpleFail 404 "Required parameter tagid missing") return =<<
       fmap snd <$> getParamInt "tagid"
  runQueries $ do
    cids <- ExceptT $ run $ query (fromIntegral t) $
            statement sql (EN.value EN.int4)
            (DE.rowsVector $ DE.value DE.int4) False
    lift $ writeLBS $ encode $ object [ "cids" .= cids ]
  where
    sql = "SELECT cid FROM comic_tag WHERE tagid=$1"
