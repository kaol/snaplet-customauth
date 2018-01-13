{-# LANGUAGE OverloadedStrings #-}

module Piperka.ComicInfo.Query where

import Control.Error.Util (bool)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Text (Text)
import Data.Maybe
import Hasql.Session hiding (run)
import Snap
import Snap.Snaplet.Hasql
import Heist

import Application hiding (uid, taglookup, extlookup, prefs)
import qualified Application (uid)
import Piperka.ComicInfo.Statements
import Piperka.ComicInfo.Types hiding (cid)

getComicInfo
  :: (Integral n)
  => ([Int] -> [ComicTag])
  -> (Int -> Text -> Maybe ExternalEntry)
  -> n
  -> Maybe MyData
  -> RuntimeSplice AppHandler (Either ComicInfoError ComicInfo)
getComicInfo taglookup extlookup cid usr = do
  let uid = Application.uid <$> usr
  runExceptT $ do
    info' <- ExceptT $ (either (Left . SqlError) return) <$>
             (lift $ withTop db $ run $ query (uid, fromIntegral cid) $
              comicInfoFetch taglookup)
    info <- ExceptT $ maybe (Left <$> checkDead (fromIntegral cid))
            (return . return) info'
    err <- ExceptT $ (either (Left . SqlError) return) <$>
           (lift $ withTop db $ run $ query (fromIntegral cid) crawlErrorFetch)
    ext <- ExceptT $ (either (Left . SqlError) (return . catMaybes)) <$>
           (lift $ withTop db $ run $ query (fromIntegral cid) $
            externalEntryFetch extlookup)
    return $ info err ext

checkDead :: Int -> RuntimeSplice AppHandler ComicInfoError
checkDead cid =
  either SqlError (bool Missing FoundDead) <$>
  (lift $ withTop db $ run $ query (fromIntegral cid) isComicDeadFetch)


getDeadComicInfo
  :: (Integral n)
  => ([Int] -> [ComicTag])
  -> n
  -> Maybe MyData
  -> RuntimeSplice AppHandler (Either ComicInfoError ComicInfo)
getDeadComicInfo taglookup cid usr = do
  let uid = Application.uid <$> usr
  (either (Left . SqlError) (maybe (Left Missing) return)) <$>
    (lift $ withTop db $ run $ query (uid, fromIntegral cid) $
     deadInfoFetch taglookup)
