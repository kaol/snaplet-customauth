{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.Hasql where

import qualified Hasql.Connection as C
import qualified Hasql.Query as Q
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import qualified Hasql.Session as S
import Snap.Snaplet
import Control.Monad.IO.Class
import Control.Lens
import Data.ByteString

class HasHasql m where
  getHasqlState :: m Hasql
  setHasqlState :: Hasql -> m ()

data Hasql = HasqlSettings C.Settings | HasqlConnection C.Connection C.Settings

hasqlInit :: C.Settings -> SnapletInit b Hasql
hasqlInit s = makeSnaplet "hasql" "Hasql Snaplet" Nothing $ do
  return $ HasqlSettings s

commit :: S.Session ()
commit = S.query () $ Q.statement "commit" E.unit D.unit True

begin :: S.Session ()
begin = S.query () $ Q.statement "begin" E.unit D.unit True

releaseHasql :: (HasHasql m, MonadIO m) => m ()
releaseHasql = do
  s <- getHasqlState
  case s of
   (HasqlSettings s) -> return ()
   (HasqlConnection c s') -> do
     liftIO $ do
       S.run commit c
       C.release c
     setHasqlState (HasqlSettings s')

run :: (HasHasql m, MonadIO m) => S.Session a -> m (Either S.Error a)
run s = do
  c' <- getHasqlState
  case c' of
   HasqlConnection c _ -> liftIO $ S.run s c

wrapDbOpen :: (HasHasql (Handler b v)) => Initializer b v ()
wrapDbOpen = do
  wrapSite bracketDbOpen

addRoutesDbOpen :: (HasHasql (Handler b v)) =>
                   [(ByteString, Handler b v ())] -> Initializer b v ()
addRoutesDbOpen = addRoutes . over (mapped._2) bracketDbOpen

bracketDbOpen :: (HasHasql (Handler b v)) => Handler b v () -> Handler b v ()
bracketDbOpen site = do
  s' <- getHasqlState
  case s' of
   (HasqlSettings s) -> do
     bracketHandler (C.acquire s) release (workSite s)
   (HasqlConnection _ _) -> return ()
  where
    release (Right c) = S.run commit c >> C.release c
    release (Left _) = return ()
    workSite s (Right c) = do
      setHasqlState (HasqlConnection c s)
      liftIO $ S.run begin c
      site
      setHasqlState (HasqlSettings s)
-- TODO
    workSite _ (Left err) = do error $ show err
