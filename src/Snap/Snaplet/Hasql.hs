{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.Hasql where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
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

type MaybeConnection = Either C.ConnectionError C.Connection

data Hasql = HasqlSettings (IO MaybeConnection) (MaybeConnection -> IO ()) Int
           | HasqlConnection (IORef (Maybe C.Connection))
             (IO MaybeConnection) (MaybeConnection -> IO ()) Int

hasqlInit :: C.Settings -> SnapletInit b Hasql
hasqlInit s = makeSnaplet "hasql" "Hasql Snaplet" Nothing $ do
  cfg <- getSnapletUserConfig
  (poolSize, poolVar, syncVar) <- liftIO $ do
    poolSize <- Data.Configurator.lookupDefault 5 cfg "pool"
    poolVar <- newEmptyMVar
    syncVar <- newEmptyMVar
    replicateM_ poolSize $ forkIO $ forever $
      takeMVar syncVar >> (putMVar poolVar =<< C.acquire s)
    replicateM_ poolSize $ putMVar syncVar ()
    return (poolSize, poolVar, syncVar)
  return $ HasqlSettings (takeMVar poolVar)
    (\c -> either (const $ return ()) C.release c >> putMVar syncVar ()) poolSize

commit :: S.Session ()
commit = S.query () $ Q.statement "commit" E.unit D.unit True

begin :: S.Session ()
begin = S.query () $ Q.statement "begin" E.unit D.unit True

ping :: S.Session Bool
ping = S.query () $ Q.statement "select true" E.unit (D.singleRow (D.value D.bool)) True

run :: (HasHasql m, MonadIO m) => S.Session a -> m (Either S.Error a)
run session = do
  state <- getHasqlState
  case state of
   HasqlConnection ref acquire release poolSize -> liftIO $ runExceptT $ do
     let tryPool :: Int -> S.Error -> ExceptT S.Error IO C.Connection
         tryPool 0 = ExceptT . return . Left
         tryPool n = const $ do
           c <- liftIO acquire
           either (\e -> (liftIO $ release c) >> tryPool (n-1) e) (lift . return) =<<
             (liftIO $ runExceptT $ do
                 c' <- catchE (ExceptT $ return c) (throwE . S.ClientError)
                 _ <- ExceptT $ S.run ping c'
                 return c')
     let initSession = do
           c <- tryPool (poolSize+1) (S.ClientError (Just ""))
           liftIO $ writeIORef ref $ Just c
           ExceptT $ liftIO $ S.run begin c
           return c
     c <- maybe initSession return =<< liftIO (readIORef ref)
     ExceptT $ liftIO (S.run session c)
   HasqlSettings _ _ _ -> error "connection IORef not initialized"

wrapDbOpen :: (HasHasql (Handler b v)) => Initializer b v ()
wrapDbOpen = wrapSite bracketDbOpen

addRoutesDbOpen :: (HasHasql (Handler b v)) =>
                   [(ByteString, Handler b v ())] -> Initializer b v ()
addRoutesDbOpen = addRoutes . over (mapped._2) bracketDbOpen

bracketDbOpen :: (HasHasql (Handler b v)) => Handler b v a -> Handler b v a
bracketDbOpen site = do
  s' <- getHasqlState
  case s' of
    HasqlSettings s r poolSize ->
      bracketHandler (newIORef Nothing)
      (maybeRelease r)
      (workSite s r poolSize)
    HasqlConnection _ _ _ _ -> error "should not happen"
  where
    -- TODO: What to do if commit fails?
    maybeRelease release ref = readIORef ref >>=
      maybe (return ()) (\c -> S.run commit c >> (release $ Right c))
    workSite s r poolSize ref = do
      setHasqlState (HasqlConnection ref s r poolSize)
      x <- site
      setHasqlState (HasqlSettings s r poolSize)
      return x
