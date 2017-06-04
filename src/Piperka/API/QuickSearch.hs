{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -Wno-unused-top-binds #-}

module Piperka.API.QuickSearch (quickSearch) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Int
import Data.Maybe
import Data.Monoid
import GHC.Generics
import Hasql.Query
import qualified Hasql.Encoders as EN
import qualified Hasql.Decoders as DE
import Hasql.Session (query)
import Prelude hiding (Ordering)
import Snap
import Snap.Snaplet.Hasql

import Application
import Piperka.API.Common
import Piperka.Listing.Statements (orderingSqlPart, parseOrdering)
import Piperka.Listing.Types.Ordering
import Piperka.Util

fetch
  :: Ordering
  -> Query Int32 (Maybe Int32)
fetch = fromJust . flip lookup table
  where
    table = map (\o -> (o, statement (sql o)
                           (EN.value EN.int4) (DE.maybeRow $ DE.value DE.int4) True))
            allOrderings
    sql o = "SELECT rownum FROM (SELECT cid, row_number() OVER (ORDER BY " <>
            (orderingSqlPart o) <> ") AS rownum FROM comics) AS r WHERE cid=$1"

data Offset = Offset { offset :: Maybe Int32 } deriving (Generic)
instance ToJSON Offset where
  toEncoding = genericToEncoding defaultOptions

quickSearch :: AppHandler ()
quickSearch = do
  cid' <- fmap snd <$> getCid
  sorttype' <- fmap parseOrdering <$> getParamText "sorttype"
  case (cid', sorttype') of
    (Just cid, Just sorttype) -> runQueries $ do
      num <- ExceptT $ run $ query (fromIntegral cid) $ fetch sorttype
      lift $ writeLBS $ encode $ Offset num
    _ -> modifyResponse $ setResponseStatus 400 "Required parameter missing"
