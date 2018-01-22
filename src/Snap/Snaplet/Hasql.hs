{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.Hasql where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.ByteString
import qualified Data.Configurator
import Data.IORef
import qualified Hasql.Connection as C
import qualified Hasql.Query as Q
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import qualified Hasql.Session as S
import Snap.Snaplet

class HasHasql m where
  getHasqlState :: m Hasql
  setHasqlState :: Hasql -> m ()

data Hasql = HasqlSettings (IO (Either C.ConnectionError C.Connection))
           | HasqlConnection (IORef (Maybe C.Connection))
             (IO (Either C.ConnectionError C.Connection))

hasqlInit :: C.Settings -> SnapletInit b Hasql
hasqlInit s = makeSnaplet "hasql" "Hasql Snaplet" Nothing $ do
  cfg <- getSnapletUserConfig
  poolVar <- liftIO $ do
    poolSize <- Data.Configurator.lookupDefault 5 cfg "pool"
    poolVar <- newEmptyMVar
    replicateM_ poolSize $ forkIO $ forever $
      putMVar poolVar =<< C.acquire s
    return poolVar
  return $ HasqlSettings $ takeMVar poolVar

commit :: S.Session ()
commit = S.query () $ Q.statement "commit" E.unit D.unit True

begin :: S.Session ()
begin = S.query () $ Q.statement "begin" E.unit D.unit True

run :: (HasHasql m, MonadIO m) => S.Session a -> m (Either S.Error a)
run session = do
  state <- getHasqlState
  case state of
   HasqlConnection ref acquire -> liftIO $ runExceptT $ do
     let initSession = do
           c <- catchE (ExceptT $ liftIO acquire)
                (throwE . S.ClientError)
           liftIO $ writeIORef ref $ Just c
           ExceptT $ liftIO $ S.run begin c
           return c
     c <- maybe initSession return =<< liftIO (readIORef ref)
     ExceptT $ liftIO (S.run session c)
   HasqlSettings _ -> error "connection IORef not initialized"

wrapDbOpen :: (HasHasql (Handler b v)) => Initializer b v ()
wrapDbOpen = wrapSite bracketDbOpen

addRoutesDbOpen :: (HasHasql (Handler b v)) =>
                   [(ByteString, Handler b v ())] -> Initializer b v ()
addRoutesDbOpen = addRoutes . over (mapped._2) bracketDbOpen

bracketDbOpen :: (HasHasql (Handler b v)) => Handler b v a -> Handler b v a
bracketDbOpen site = do
  s' <- getHasqlState
  case s' of
    (HasqlSettings s) -> do
      bracketHandler (newIORef Nothing) maybeRelease (workSite s)
    (HasqlConnection _ _) -> error "should not happen"
  where
    -- TODO: What to do if commit fails?
    maybeRelease ref = readIORef ref >>=
      maybe (return ()) (\c -> S.run commit c >> C.release c)
    workSite s ref = do
      setHasqlState (HasqlConnection ref s)
      x <- site
      setHasqlState (HasqlSettings s)
      return x
