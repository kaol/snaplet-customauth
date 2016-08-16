{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend ( ) where

import Piperka.Listing.Types

import qualified Data.Text as T
--import Data.Int
import Data.UUID
--import Data.Text as T
import Snap.Snaplet.CustomAuth
import Snap.Snaplet.Hasql
import Application
import Hasql.Query
import qualified Hasql.Encoders as EN
import qualified Hasql.Decoders as DE
import Hasql.Session (query)
--import Data.Functor
import Snap.Snaplet
import Data.Functor.Contravariant
import Data.Monoid
import Control.Applicative
import Data.Maybe

encodeLoginPasswd :: EN.Params (T.Text, T.Text)
encodeLoginPasswd =
  contramap fst (EN.value EN.text) <>
  contramap snd (EN.value EN.text)

encodeSessionToken :: EN.Params T.Text
encodeSessionToken = contramap id (EN.value EN.text)

userRow :: DE.Row UUID -> DE.Row MyData
userRow x =
  MyData
  <$> DE.value DE.int4
  <*> DE.value DE.text
  <*> x
  <*> DE.value DE.uuid

prefsRow :: DE.Row UUID -> DE.Row UserPrefs
prefsRow x =
  UserPrefs
  <$> (liftA Just $ userRow x)
  <*> DE.value DE.int4
  <*> (liftA2 (,) (DE.value DE.int4) (DE.value DE.int4))
  <*> DE.value DE.int4
  <*> (liftA intToColumns (DE.value DE.int4))
  <*> DE.value DE.bool

instance IAuthBackend UserPrefs App where

  login u pwd = withTop db $ do
    usr <- run $ query (u, pwd) loginUserPasswd
    return $ either (Left . AuthError . show) (maybe (Left AuthFailure) Right) usr
      where
        loginUserPasswd = statement sql encode decode True
        encode = encodeLoginPasswd
        decode = DE.maybeRow $ prefsRow (DE.value DE.uuid)
        sql = "select o_uid, name, p_session, csrf_ham, new_comics, total_new, new_in, display_rows, display_columns, new_windows from auth_login($1, $2) join users on o_uid=uid"

  logout t = withTop db $ do
    run $ query t deleteSession
    return ()
      where
        deleteSession = statement sql encode DE.unit True
        encode = encodeSessionToken
        sql = "delete from p_session where ses=$1"

  recover t = withTop db $ do
    let tokenUuid = fromText t
    case tokenUuid of
     Nothing -> return $ Left AuthFailure
     Just token -> do
       usr <- run $ query token recoverSess
       return $ either (Left . AuthError . show) (maybe (Left AuthFailure) Right) usr
         where
           recoverSess = statement sql encode decode True
           encode = contramap id (EN.value EN.uuid)
           decode = DE.maybeRow $ prefsRow (pure token)
           sql = "select uid, name, csrf_ham, new_comics, total_new, new_in, display_rows, display_columns, new_windows from recover_session($1) join users on (o_uid=uid)"

instance UserData MyData where
  extractUser MyData{..} = AuthUser
    { name = uname
    , session = toText usession
    , csrfToken = toText ucsrfToken
    }

instance UserData UserPrefs where
  extractUser UserPrefs{..} = AuthUser
    { name = uname $ fromJust user
    , session = toText $ usession $ fromJust user
    , csrfToken = toText $ ucsrfToken $ fromJust user
    }
