{-# LANGUAGE OverloadedStrings #-}

module Piperka.ComicInfo.Query where

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
import Piperka.Util (if')

getComicInfo
  :: (Integral n)
  => ([Int] -> [ComicTag])
  -> (Int -> Text -> Maybe ExternalEntry)
  -> n
  -> UserPrefs
  -> RuntimeSplice AppHandler (Either ComicInfoError ComicInfo)
getComicInfo taglookup extlookup cid prefs = do
  let uid = Application.uid <$> user prefs
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
  (either SqlError (if' FoundDead Missing)) <$>
  (lift $ withTop db $ run $ query (fromIntegral cid) isComicDeadFetch)


getDeadComicInfo
  :: (Integral n)
  => ([Int] -> [ComicTag])
  -> n
  -> UserPrefs
  -> RuntimeSplice AppHandler (Either ComicInfoError ComicInfo)
getDeadComicInfo taglookup cid prefs = do
  let uid = Application.uid <$> user prefs
  (either (Left . SqlError) (maybe (Left Missing) return)) <$>
    (lift $ withTop db $ run $ query (uid, fromIntegral cid) $
     deadInfoFetch taglookup)
