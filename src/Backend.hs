{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend ( oauth2Login, oauth2Check ) where

import Piperka.Listing.Types

import Contravariant.Extras.Contrazip
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import qualified Data.Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isSpace)
import Data.Functor.Contravariant
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Data.UUID
import Snap
import Snap.Snaplet.CustomAuth
import Snap.Snaplet.Hasql
import Application
import Hasql.Query
import Hasql.Session (Error(..), ResultError(..), query)
import qualified Hasql.Encoders as EN
import qualified Hasql.Decoders as DE
import PostgreSQL.ErrorCodes (unique_violation)

import Piperka.OAuth2.Types
import Piperka.Util (monadicTuple2, getParamText)

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
  <*> DE.value DE.bool
  <*> prefsRow

prefsRow :: DE.Row UserPrefs
prefsRow =
  UserPrefs
  <$> DE.value DE.int4
  <*> (liftA intToColumns (DE.value DE.int4))
  <*> DE.value DE.bool

-- I get the feeling that there should be a better way to express this.
userStatsRow :: DE.Row UUID -> DE.Row UserWithStats
userStatsRow x =
  (\(a,b,c) -> UserWithStats a (b,c))
  <$> (DE.value $ DE.composite ((,,)
                                <$> DE.compositeValue DE.int4
                                <*> DE.compositeValue DE.int4
                                <*> DE.compositeValue DE.int4))
  <*> liftA monadicTuple2 ((,)
                           <$> DE.nullableValue DE.int4
                           <*> DE.nullableValue DE.int4)
  <*> userRow x

userDefaultRow :: Text -> DE.Row MyData
userDefaultRow n =
  MyData
  <$> DE.value DE.int4
  <*> pure n
  <*> DE.value DE.uuid
  <*> DE.value DE.uuid
  <*> pure False
  <*> pure defaultUserPrefs

statsDefaultRow :: Text -> DE.Row UserWithStats
statsDefaultRow n =
  defaultUserStats
  <$> userDefaultRow n

doLogin
  :: Text
  -> Text
  -> (DE.Row UUID -> DE.Row u)
  -> ByteString
  -> Handler App (AnyAuth u) (Either Error (Maybe u))
doLogin u pwd decodeRow sql = withTop db $ run $ query (u, pwd) loginUserPasswd
    where
      loginUserPasswd = statement sql encode decode True
      encode = encodeLoginPasswd
      decode = DE.maybeRow $ decodeRow (DE.value DE.uuid)

doLogout
  :: Text
  -> Handler App (AnyAuth u) ()
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
  -> Handler App (AnyAuth u) (Either (AuthFailure Error) u)
doRecover t decodeRow sql = withTop db $ do
  let tokenUuid = fromText t
  case tokenUuid of
    Nothing -> return $ Left $ Login NoSession
    Just token -> do
      usr <- run $ query token recoverSess
      return $ either (Left . UserError)
        (maybe (Left $ Login SessionRecoverFail) Right) usr
        where
          recoverSess = statement sql encode decode True
          encode = contramap id (EN.value EN.uuid)
          decode = DE.maybeRow $ decodeRow (pure token)

doPrepare
  :: HasUserID u
  => Maybe u
  -> Text
  -> Handler App (AnyAuth u) (Either Error AuthID)
doPrepare u p = withTop db $ run $ query (p, extractUid <$> u) stmt
  where
    stmt = statement sql encode decode True
    encode = contrazip2 (EN.value EN.text) (EN.nullableValue EN.int4)
    decode = DE.singleRow $ DE.value DE.int4
    sql = "select auth_create_password$1, $2)"

doCancelPrepare
  :: AuthID
  -> Handler App (AnyAuth u) ()
doCancelPrepare u = (withTop db $ run $ query u stmt) >> return ()
  where
    stmt = statement sql encode DE.unit True
    encode = EN.value EN.int4
    sql = "delete from login_method where lmid=$1"

validName
  :: Text
  -> Bool
validName name = len > 1 && len < 40 && isTrimmed
  where
    len = T.length name
    isTrimmed = not (isSpace (T.head name) || isSpace (T.last name))

doCreate
  :: Text
  -> AuthID
  -> DE.Result b
  -> Handler App (AnyAuth u) (Either (Either Error CreateFailure) b)
doCreate u loginId decode = withTop db $ runExceptT $ do
  email <- lift $ getParamText "email"
  when (not $ validName u) $ throwE $ Right InvalidName
  usr <- lift $ run $ query (u, email, loginId) stmt
  either (throwE . maybeDuplicate) return usr
    where
      stmt = statement sql encode decode True
      encode = contrazip3 (EN.value EN.text) (EN.nullableValue EN.text) (EN.value EN.int4)
      sql = "select uid, p_session, csrf_ham from auth_create($1, $2, $3)"
      maybeDuplicate e =
        if isDuplicateSqlError e then Right DuplicateName else Left e

attach
  :: HasUserID u
  => u
  -> AuthID
  -> Handler App (AnyAuth u) (Either Error ())
attach u i = withTop db $ run $ query (extractUid u, i) stmt
  where
    stmt = statement sql encode DE.unit True
    encode = contrazip2 (EN.value EN.int4) (EN.value EN.int4)
    sql = "insert into login_method (uid, lmid) values ($1, $2)"

isDuplicateSqlError
  :: Error
  -> Bool
isDuplicateSqlError (ResultError (ServerError c _ _ _)) = c == unique_violation
isDuplicateSqlError _ = False

-- For use with partial HTML render and AJAX calls
instance IAuthBackend MyData AuthID Error App where
  preparePasswordCreate = doPrepare
  cancelPrepare = doCancelPrepare
  create u i = doCreate u i $ DE.singleRow $ userDefaultRow u
  attachLoginMethod = attach
  login u pwd = doLogin u pwd userRow
                "select uid, name, p_session, csrf_ham, \
                \uid in (select uid from moderator) as moderator, \
                \display_rows, display_columns, new_windows \
                \from auth_login($1, $2) join users using (uid)"
  logout = doLogout
  recover t = doRecover t userRow
              "select uid, name, csrf_ham, \
              \uid in (select uid from moderator) as moderator, \
              \display_rows, display_columns, new_windows \
              \from recover_session($1) join users using (uid)"
  getUserId = return . toStrict . Data.Binary.encode . uid
  isDuplicateError = return . isDuplicateSqlError

-- Side effect: updates last read stats on login/recover
instance IAuthBackend UserWithStats AuthID Error App where
  preparePasswordCreate = doPrepare
  cancelPrepare = doCancelPrepare
  create u i = doCreate u i $ DE.singleRow $ statsDefaultRow u
  attachLoginMethod = attach
  login u pwd = doLogin u pwd userStatsRow
                "select (select (new_comics, total_new, new_in) from \
                \get_and_update_stats(uid, true)), \
                \mod_queue, cast(mod_days as int), \
                \uid, name, p_session, csrf_ham, coalesce(moderator, false), \
                \display_rows, display_columns, new_windows \
                \from auth_login($1, $2) join users using (uid) left join \
                \(select uid, true as moderator, (select count(*) from user_edit \
                \where cid in (select cid from comics)) as mod_queue, \
                \extract(days from now() - last_moderate) as mod_days \
                \from moderator) as m using (uid)"
  logout = doLogout
  recover t = doRecover t userStatsRow
              "select (select (new_comics, total_new, new_in) from \
              \get_and_update_stats(uid, false)), \
              \mod_queue, cast(mod_days as int), \
              \uid, name, csrf_ham, coalesce(moderator, false), \
              \display_rows, display_columns, new_windows \
              \from recover_session($1) join users using (uid) left join \
              \(select uid, true as moderator, (select count(*) from user_edit \
              \where cid in (select cid from comics)) as mod_queue, \
              \extract(days from now() - last_moderate) as mod_days \
              \from moderator) as m using (uid)"
  getUserId = return . toStrict . Data.Binary.encode . uid . user
  isDuplicateError = return . isDuplicateSqlError

oauth2Login
  :: Provider
  -> Text
  -> Handler App ApiAuth (Either Error (Maybe MyData))
oauth2Login provider token = do
  withTop db $ run $ query (fromIntegral $ providerOpid provider, token) stmt
  where
    stmt = statement sql encode decode True
    encode = contrazip2 (EN.value EN.int4) (EN.value EN.text)
    decode = DE.maybeRow $ userRow (DE.value DE.uuid)
    sql = "select uid, name, p_session, csrf_ham, \
          \uid in (select uid from moderator) as moderator, \
          \display_rows, display_columns, new_windows \
          \from auth_oauth2($1, $2) join users using (uid)"

oauth2Check
  :: Provider
  -> Text
  -> Handler App ApiAuth (Either Error (Maybe ByteString))
oauth2Check provider token = do
  withTop db $ run $ query (fromIntegral $ providerOpid provider, token) stmt
  where
    stmt = statement sql encode decode True
    encode = contrazip2 (EN.value EN.int4) (EN.value EN.text)
    decode = DE.maybeRow $ (toStrict . Data.Binary.encode <$> DE.value DE.int4)
    sql = "select uid from login_method_oauth2 where \
          \uid is not null and opid = $1 and identification = $2"

instance UserData MyData where
  extractUser MyData{..} = AuthUser
    { name = uname
    , session = toASCIIBytes usession
    , csrfToken = toASCIIBytes ucsrfToken
    }

instance UserData UserWithStats where
  extractUser UserWithStats{..} = AuthUser
    { name = uname user
    , session = toASCIIBytes $ usession user
    , csrfToken = toASCIIBytes $ ucsrfToken user
    }

class HasUserID u where
  extractUid :: u -> UserID

instance HasUserID MyData where
  extractUid u = uid u

instance HasUserID UserWithStats where
  extractUid u = uid $ user u
