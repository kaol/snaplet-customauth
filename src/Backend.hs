{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend ( ) where

import Piperka.Listing.Types

import Contravariant.Extras.Contrazip
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding
import Data.UUID
import Snap
import Snap.Snaplet.CustomAuth
import Snap.Snaplet.Hasql
import Application
import Hasql.Query
import Hasql.Session (Error)
import qualified Hasql.Encoders as EN
import qualified Hasql.Decoders as DE
import Hasql.Session (query)
import Data.Functor.Contravariant
import Data.Monoid
import Control.Applicative
import Data.Maybe

encodeLoginPasswd :: EN.Params (T.Text, T.Text)
encodeLoginPasswd =
  contramap fst (EN.value EN.text) <>
  contramap snd (EN.value EN.text)

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
  <*> (liftA intToColumns (DE.value DE.int4))
  <*> DE.value DE.bool

-- I get the feeling that there should be a better way to express this.
prefsStatsRow :: DE.Row UUID -> DE.Row UserPrefsWithStats
prefsStatsRow x =
  (\(a,b,c) -> UserPrefsWithStats a (b,c))
  <$> (DE.value $ DE.composite ((,,)
                                <$> DE.compositeValue DE.int4
                                <*> DE.compositeValue DE.int4
                                <*> DE.compositeValue DE.int4))
  <*> prefsRow x

prefsDefaultRow :: Text -> DE.Row UserPrefs
prefsDefaultRow n =
  UserPrefs
  <$> (liftA Just $
       MyData
       <$> DE.value DE.int4
       <*> pure n
       <*> DE.value DE.uuid
       <*> DE.value DE.uuid
      )
  <*> pure (rows defaultUserPrefs)
  <*> pure (columns defaultUserPrefs)
  <*> pure (newExternWindows defaultUserPrefs)

prefsStatsDefaultRow :: Text -> DE.Row UserPrefsWithStats
prefsStatsDefaultRow n =
  UserPrefsWithStats 0 (0,0)
  <$> prefsDefaultRow n

doLogin
  :: Text
  -> Text
  -> (DE.Row UUID -> DE.Row u)
  -> ByteString
  -> Handler App (AuthManager u App) (Either (AuthFailure Error) u)
doLogin u pwd decodeRow sql = withTop db $ do
  usr <- run $ query (u, pwd) loginUserPasswd
  return $ either (Left . AuthError) (maybe (Left AuthFailure) Right) usr
    where
      loginUserPasswd = statement sql encode decode True
      encode = encodeLoginPasswd
      decode = DE.maybeRow $ decodeRow (DE.value DE.uuid)

doLogout
  :: Text
  -> Handler App (AuthManager u App) ()
doLogout t = withTop db $ do
  let tokenUuid = fromText t
  case tokenUuid of
   Nothing -> return ()  -- Don't know what to do in this case
   Just t' -> do
     run $ query t' deleteSession
     return ()
  where
    deleteSession = statement sql (EN.value EN.uuid) DE.unit True
    sql = "delete from p_session where ses=$1"

doRecover
  :: Text
  -> (DE.Row UUID -> DE.Row u)
  -> ByteString
  -> Handler App (AuthManager u App) (Either (AuthFailure Error) u)
doRecover t decodeRow sql = withTop db $ do
    let tokenUuid = fromText t
    case tokenUuid of
     Nothing -> return $ Left AuthFailure
     Just token -> do
       usr <- run $ query token recoverSess
       return $ either (Left . AuthError) (maybe (Left AuthFailure) Right) usr
         where
           recoverSess = statement sql encode decode True
           encode = contramap id (EN.value EN.uuid)
           decode = DE.maybeRow $ decodeRow (pure token)

doCheck
  :: Text
  -> Handler App (AuthManager u App) (Either Error Bool)
doCheck u = withTop db $ do
  run $ query u checkUserName
    where
      checkUserName = statement sql encode decode True
      encode = contramap id $ EN.value EN.text
      decode = DE.singleRow $ DE.value DE.bool
      sql = "select not exists (select true from users where lower($1) = lower(name))"

doCreate
  :: Text
  -> Text
  -> DE.Result b
  -> Handler App (AuthManager u App) (Either (CreateFailure Error) b)
doCreate u pwd decode = withTop db $ do
  email <- (fmap . fmap) decodeUtf8 $ getParam "email"
  usr <- runExceptT $ do
    loginId <- ExceptT $ run $ query pwd createPassword
    ExceptT $ run $ query (u, email, loginId) createUser
  return $ either (Left . CreateError) Right usr
    where
      createPassword = statement sql1 (EN.value EN.text) (DE.singleRow $ DE.value DE.int4) True
      createUser = statement sql2 encode decode True
      encode = contrazip3 (EN.value EN.text) (EN.nullableValue EN.text) (EN.value EN.int4)
      sql1 = "select lmid from auth_create_password($1)"
      sql2 = "select uid, p_session, csrf_ham from auth_create($1, $2, $3)"

-- For use with partial HTML render and AJAX calls
instance IAuthBackend UserPrefs Error App where
  check = doCheck
  create u pwd = doCreate u pwd $ DE.singleRow $ prefsDefaultRow u
  login u pwd = doLogin u pwd prefsRow
                "select uid, name, p_session, csrf_ham, display_rows, \
                \display_columns, new_windows \
                \from auth_login($1, $2) join users using (uid)"
  logout = doLogout
  recover t = doRecover t prefsRow
              "select uid, name, csrf_ham, display_rows, display_columns, \
              \new_windows from recover_session($1) join users using (uid)"

-- Side effect: updates last read stats on login/recover
instance IAuthBackend UserPrefsWithStats Error App where
  check = doCheck
  create u pwd = doCreate u pwd $ DE.singleRow $ prefsStatsDefaultRow u
  login u pwd = doLogin u pwd prefsStatsRow
                "select (select (new_comics, total_new, new_in) from \
                \get_and_update_stats(uid, true)), uid, name, p_session, \
                \csrf_ham, display_rows, display_columns, new_windows \
                \from auth_login($1, $2) join users using (uid)"
  logout = doLogout
  recover t = doRecover t prefsStatsRow
              "select (select (new_comics, total_new, new_in) from \
              \get_and_update_stats(uid, false)), uid, name, csrf_ham, \
              \display_rows, display_columns, new_windows \
              \from recover_session($1) join users using (uid)"

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

instance UserData UserPrefsWithStats where
  extractUser UserPrefsWithStats{..} = AuthUser
    { name = uname $ fromJust $ user prefs
    , session = toText $ usession $ fromJust $ user prefs
    , csrfToken = toText $ ucsrfToken $ fromJust $ user prefs
    }
