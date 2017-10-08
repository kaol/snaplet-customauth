{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -Wno-unused-top-binds #-}

module Piperka.API.Archive (dumpArchive) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Int
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import Hasql.Query
import qualified Hasql.Encoders as EN
import qualified Hasql.Decoders as DE
import Hasql.Session (query)
import Snap
import Snap.Snaplet.Hasql

import Application
import Piperka.API.Common
import Piperka.Util (getCid)

data Archive = Archive {
    url_base :: Text
  , url_tail :: Text
  , fixed_head :: Text
  , homepage :: Text
  , pages :: Vector (Text, Int32)
  } deriving (Generic)

instance ToJSON Archive where
  toEncoding = genericToEncoding defaultOptions

decodeArchiveInfo :: DE.Row (Vector (Text, Int32) -> Archive)
decodeArchiveInfo =
  Archive
  <$> DE.value DE.text
  <*> DE.value DE.text
  <*> DE.value DE.text
  <*> DE.value DE.text

decodePageInfo :: DE.Row (Text, Int32)
decodePageInfo =
  (,)
  <$> DE.value DE.text
  <*> DE.value DE.int4

dumpArchive :: AppHandler ()
dumpArchive = do
  c <- maybe (simpleFail 404 "Required parameter tagid missing") return =<<
       fmap (fromIntegral . snd) <$> getCid
  runQueries $ do
    aMain' <- ExceptT $ run $ query c $ statement sql
              (EN.value EN.int4) (DE.maybeRow decodeArchiveInfo) True
    maybe (lift $ simpleFail 404 "No such comic")
      (\aMain ->
         (ExceptT $ run $ query c $ statement sql'
          (EN.value EN.int4) (DE.rowsVector decodePageInfo) True) >>=
         lift . writeLBS . encode . aMain
      ) aMain'
  where
    sql = "SELECT url_base, url_tail, fixed_head, homepage \
          \FROM comics WHERE cid=$1"
    sql' = "SELECT name, COALESCE(maxsubord, 0) \
           \FROM updates LEFT JOIN (SELECT cid, ord, MAX(subord) AS maxsubord \
           \FROM page_fragments GROUP BY cid, ord) AS x USING (cid, ord) \
           \WHERE cid=$1 ORDER BY ord"
