{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend ( ) where

import Data.Tuple.Curry
import qualified Data.Text as T
import qualified Control.Exception as E
import qualified Hasql as H
import qualified Hasql.Postgres as HP
import Hasql.Backend
import Data.Int
import Data.UUID
import Data.Text as T
import Snap.Snaplet.CustomAuth
import Application

instance IAuthBackend MyData (H.Pool HP.Postgres) where

  login pool u pwd = flip E.catch onFailure $ do
    res <- H.session pool $ do
      H.tx (Just (H.Serializable, Just True)) $ loginQuery u pwd
    return $ either (Left . AuthError . show) (maybe (Left AuthFailure) Right) res

  logout pool t = do
    H.session pool $ do
      H.tx (Just (H.Serializable, Just True)) $ H.unitEx $ [H.stmt|delete from p_session where ses=?|] t
    return ()

  recover pool t = flip E.catch onFailure $ do
    res <- H.session pool $ do
      let tokenUuid = fromText t
      H.tx (Just (H.Serializable, Just True)) $ do
        cred :: Maybe (Int32, Text, UUID) <- H.maybeEx $ [H.stmt|select uid, name, csrf_ham from recover_session(?) join users on (o_uid=uid)|] t
        return $ do
          ses <- tokenUuid
          cred' <- cred
          return $ (uncurryN MyData . \(a,b,c) -> (a,b,ses,c)) cred'
    return $ either (Left . AuthError . show) (maybe (Left AuthFailure) Right) res

instance UserData MyData where
  extractUser MyData{..} = AuthUser
    { name = uname
    , session = toText usession
    , csrfToken = toText ucsrfToken
    }

onFailure :: Monad m => E.SomeException -> m (Either AuthFailure a)
onFailure e = return $ Left $ AuthError $ show e

loginQuery :: (CxValue c Text, CxValue c Int32, CxValue c UUID) => Text -> Text -> H.Tx c s (Maybe MyData)
loginQuery u pwd = do
  cred :: Maybe (Int32, Text, UUID, UUID) <- H.maybeEx $ [H.stmt|select o_uid, name, p_session, csrf_ham from auth_login(?, ?) join users on o_uid=uid|] u pwd
  return $ fmap (uncurryN MyData) cred

