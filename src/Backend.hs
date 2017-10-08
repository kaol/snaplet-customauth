{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend ( oauth2Login ) where

import Piperka.Listing.Types

import Contravariant.Extras.Contrazip
import Control.Applicative
import qualified Data.Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Functor.Contravariant
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding
import Data.UUID
import Snap
import Snap.Snaplet.CustomAuth hiding (oauth2Login)
import Snap.Snaplet.Hasql
import Application
import Hasql.Query
import Hasql.Session (Error(..))
import qualified Hasql.Encoders as EN
import qualified Hasql.Decoders as DE
import Hasql.Session (ResultError(..), query)
import PostgreSQL.ErrorCodes (unique_violation)

import Piperka.API.OAuth2.Types
import Piperka.Util (monadicTuple2)

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
  <*> liftA monadicTuple2 ((,)
                           <$> DE.nullableValue DE.int4
                           <*> DE.nullableValue DE.int4)
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
       <*> DE.value DE.bool
      )
  <*> pure (rows defaultUserPrefs)
  <*> pure (columns defaultUserPrefs)
  <*> pure (newExternWindows defaultUserPrefs)

prefsStatsDefaultRow :: Text -> DE.Row UserPrefsWithStats
prefsStatsDefaultRow n =
  UserPrefsWithStats 0 (0,0) Nothing
  <$> prefsDefaultRow n

doLogin
  :: Text
  -> Text
  -> (DE.Row UUID -> DE.Row u)
  -> ByteString
  -> Handler App (AuthManager u App) (Either Error (Maybe u))
doLogin u pwd decodeRow sql = withTop db $ run $ query (u, pwd) loginUserPasswd
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
  -> Handler App (AuthManager u App) (Either (Either Error AuthFailure) u)
doRecover t decodeRow sql = withTop db $ do
    let tokenUuid = fromText t
    case tokenUuid of
     Nothing -> return $ Left $ Right AuthFailure
     Just token -> do
       usr <- run $ query token recoverSess
       return $ either (Left . Left) (maybe (Left $ Right AuthFailure) Right) usr
         where
           recoverSess = statement sql encode decode True
           encode = contramap id (EN.value EN.uuid)
           decode = DE.maybeRow $ decodeRow (pure token)

doPrepare
  :: HasUserID u
  => Maybe u
  -> Text
  -> Handler App (AuthManager u App) (Either Error AuthID)
doPrepare u p = withTop db $ run $ query (p, extractUid =<< u) stmt
  where
    stmt = statement sql encode decode True
    encode = contrazip2 (EN.value EN.text) (EN.nullableValue EN.int4)
    decode = DE.singleRow $ DE.value DE.int4
    sql = "select auth_create_password$1, $2)"

doCancelPrepare
  :: AuthID
  -> Handler App (AuthManager u App) ()
doCancelPrepare u = (withTop db $ run $ query u stmt) >> return ()
  where
    stmt = statement sql encode DE.unit True
    encode = EN.value EN.int4
    sql = "delete from login_method where lmid=$1"

doCreate
  :: Text
  -> AuthID
  -> DE.Result b
  -> Handler App (AuthManager u App) (Either (Either Error CreateFailure) b)
doCreate u loginId decode = withTop db $ do
  email <- (fmap . fmap) decodeUtf8 $ getParam "email"
  usr <- run $ query (u, email, loginId) stmt
  return $ either (Left . maybeDuplicate) Right usr
    where
      stmt = statement sql encode decode True
      encode = contrazip3 (EN.value EN.text) (EN.nullableValue EN.text) (EN.value EN.int4)
      sql = "select uid, p_session, csrf_ham from auth_create($1, $2, $3)"
      maybeDuplicate e =
        if isDuplicateSqlError e then Right DuplicateName else Left e

isDuplicateSqlError
  :: Error
  -> Bool
isDuplicateSqlError (ResultError (ServerError c _ _ _)) = c == unique_violation
isDuplicateSqlError _ = False

-- For use with partial HTML render and AJAX calls
instance IAuthBackend UserPrefs AuthID Error App where
  preparePasswordCreate = doPrepare
  cancelPrepare = doCancelPrepare
  create u i = doCreate u i $ DE.singleRow $ prefsDefaultRow u
  login u pwd = doLogin u pwd prefsRow
                "select uid, name, p_session, csrf_ham, \
                \uid in (select uid from moderator) as moderator, \
                \display_rows, display_columns, new_windows \
                \from auth_login($1, $2) join users using (uid)"
  logout = doLogout
  recover t = doRecover t prefsRow
              "select uid, name, csrf_ham, \
              \uid in (select uid from moderator) as moderator, \
              \display_rows, display_columns, new_windows \
              \from recover_session($1) join users using (uid)"
  getUserId = return . toStrict . Data.Binary.encode . uid . fromJust . user
  isDuplicateError = return . isDuplicateSqlError

-- Side effect: updates last read stats on login/recover
instance IAuthBackend UserPrefsWithStats AuthID Error App where
  preparePasswordCreate = doPrepare
  cancelPrepare = doCancelPrepare
  create u i = doCreate u i $ DE.singleRow $ prefsStatsDefaultRow u
  login u pwd = doLogin u pwd prefsStatsRow
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
  recover t = doRecover t prefsStatsRow
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
  getUserId = return . toStrict . Data.Binary.encode . uid . fromJust . user . prefs
  isDuplicateError = return . isDuplicateSqlError

oauth2Login
  :: Provider
  -> Text
  -> Handler App (AuthManager UserPrefs App) (Either Error (Maybe UserPrefs))
oauth2Login provider token = do
  withTop db $ run $ query (fromIntegral $ providerOpid provider, token) stmt
  where
    stmt = statement sql encode decode True
    encode = contrazip2 (EN.value EN.int4) (EN.value EN.text)
    decode = DE.maybeRow $ prefsRow (DE.value DE.uuid)
    sql = "select uid, name, p_session, csrf_ham, \
          \uid in (select uid from moderator) as moderator, \
          \display_rows, display_columns, new_windows \
          \from auth_oauth2($1, $2) join users using (uid)"

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

class HasUserID u where
  extractUid :: u -> Maybe UserID

instance HasUserID UserPrefs where
  extractUid u = uid <$> user u

instance HasUserID UserPrefsWithStats where
  extractUid u = uid <$> (user $ prefs u)
