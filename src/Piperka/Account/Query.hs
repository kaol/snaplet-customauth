{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Piperka.Account.Query
  ( getAccountSettings
  , updateUnpriv
  , tryUpdatePriv
  , validateToken
  , validatePriv
  , setOAuth2Login
  , getUserEmail
  ) where

import Control.Applicative
import Control.Arrow
import Control.Error.Util (hush)
import Control.Monad.Trans.Except
import Contravariant.Extras.Contrazip
import Data.Functor.Contravariant
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Hasql.Decoders as DE
import Hasql.Encoders as EN
import Hasql.Session hiding (run, sql)
import Hasql.Query
import Snap.Snaplet.Hasql (run)

import Application
import Piperka.OAuth2.Types
import Piperka.Account.Types
import Piperka.Listing.Types (columnsToInt)
import Piperka.Profile.Types (intToPrivacy, privacyToInt)

getAccountSettings
  :: UserID
  -> AppHandler (Either Error AccountData)
getAccountSettings u = runExceptT $ do
  res1 <- AccountData <$> (ExceptT $ run $ query u stmt1)
  res1 <$> (ExceptT $ run $ query u stmt2)
  where
    stmt1 = statement sql1 encode (DE.singleRow decode1) True
    stmt2 = statement sql2 encode (DE.rowsList decode2) True
    encode = EN.value EN.int4
    decode1 = UserAccountSettings
              <$> (liftA intToPrivacy $ DE.value DE.int4)
              <*> DE.value DE.bool
              <*> DE.nullableValue DE.text
              <*> DE.nullableValue DE.text
              <*> (BookmarkOptions
                   <$> DE.value DE.int4
                   <*> DE.value DE.bool
                   <*> DE.value DE.bool)
    decode2 = ProviderData
              <$> DE.value DE.text
              <*> DE.value DE.text
              <*> DE.nullableValue DE.text
    sql1 = "SELECT privacy, \
           \uid IN (SELECT uid FROM login_method_passwd WHERE uid IS NOT NULL), \
           \email, writeup, bookmark_sort, offset_bookmark_by_one, bookmark_sort \
           \FROM users WHERE uid=$1"
    sql2 = "SELECT name, display_name, identification FROM oauth2_provider LEFT JOIN \
           \(SELECT identification, opid FROM login_method_oauth2 WHERE uid=$1) AS x \
           \USING (opid) ORDER BY display_name"

checkPassword
  :: Query (UserID, Text) Bool
checkPassword = statement sql encode (DE.singleRow $ DE.value DE.bool) True
  where
    encode = contrazip2 (EN.value EN.int4) (EN.value EN.text)
    sql = "SELECT (hash = crypt($2, hash)) AS pwmatch \
          \FROM users JOIN login_method_passwd USING (uid) WHERE uid=$1"

updateUnpriv
  :: UserID
  -> AccountUpdate
  -> AppHandler (Either Error ())
updateUnpriv u a = run $ query (u, a) stmt
  where
    stmt = statement sql encode DE.unit True
    encode = contrazip2 (EN.value EN.int4) $ mconcat
             [ newWindows >$< EN.value EN.bool
             , rows' >$< EN.value EN.int4
             , columnsToInt . columns' >$< EN.value EN.int4
             , holdBookmark . bookmarkSettings' >$< EN.value EN.bool
             , bookmarkSort . bookmarkSettings' >$< EN.value EN.int4
             , offsetMode . bookmarkSettings' >$< EN.value EN.bool
             ]
    sql = "UPDATE users SET new_windows=$2, display_rows=$3, \
          \display_columns=$4, hold_bookmark=$5, bookmark_sort=$6, \
          \offset_bookmark_by_one=$7 WHERE uid=$1"


updatePriv
  :: Query (UserID, PrivData) ()
updatePriv = statement sql encode DE.unit True
  where
    scrub = (=<<) (\x -> if T.null x then Nothing else Just x)
    encode = contrazip2 (EN.value EN.int4)
             ((contramap (scrub . email') (EN.nullableValue EN.text)) <>
              (contramap (privacyToInt . privacy') (EN.value EN.int4)) <>
              (contramap (scrub . writeup') (EN.nullableValue EN.text)))
    sql = "UPDATE users SET email=$2, privacy=$3, writeup=$4 WHERE uid=$1"

updatePassword
  :: Query (UserID, PrivData) ()
updatePassword = statement sql encode DE.unit True
  where
    encode = contrazip2 (EN.value EN.int4)
      (contramap (fromJust . newPassword) (EN.value EN.text))
    sql = "SELECT auth_create_password($2, $1)"

deletePassword
  :: Query UserID ()
deletePassword = statement sql (EN.value EN.int4) DE.unit True
  where
    sql = "DELETE FROM login_method_passwd WHERE uid=$1"

setOAuth2Login
  :: Query (UserID, Provider, Text) ()
setOAuth2Login = statement sql encode DE.unit True
  where
    encode = contrazip3 (EN.value EN.int4)
      (fromIntegral . providerOpid >$< (EN.value EN.int4))
      (EN.value EN.text)
    sql = "INSERT INTO login_method_oauth2 (uid, opid, identification) VALUES \
          \($1, $2, $3) ON CONFLICT (uid, opid) DO NOTHING"

deleteOAuth2Login
  :: Query (UserID, [Provider]) ()
deleteOAuth2Login = statement sql encode DE.unit True
  where
    encode = contrazip2 (EN.value EN.int4)
      (EN.value (map (fromIntegral . providerOpid) >$<
                 (EN.array $ EN.arrayDimension foldl $ EN.arrayValue EN.int4)))
    sql = "DELETE FROM login_method_oauth2 WHERE uid=$1 AND opid = ANY ($2 :: int[])"

validateToken
  :: UserID
  -> Provider
  -> Text
  -> AppHandler (Either Error Bool)
validateToken u p t = run $ query (u, p, t) stmt
  where
    stmt = statement sql encode (DE.singleRow $ DE.value DE.bool) True
    encode = contrazip3 (EN.value EN.int4)
      (EN.value $ (fromIntegral . providerOpid) >$< EN.int4)
      (EN.value EN.text)
    sql = "SELECT $1 IN (SELECT uid FROM users \
          \JOIN login_method_oauth2 USING (uid) \
          \WHERE opid=$2 AND identification=$3"

validatePriv
  :: UserID
  -> PrivData
  -> ValidateMethod
  -> AppHandler (Either (Either AccountUpdateError NeedsValidation) ())
validatePriv u a val = runExceptT $ do
  let pwFail = case a of
        (UpdateAccount n n' _ _ _ _ _) -> let
          pwChange = maybe False (not . T.null) n
          pwMismatch = n /= n'
          in pwChange && pwMismatch
        _ -> False
  case (val, pwFail) of
    (_, True) -> throwE $ Left AccountNewPasswordMismatch
    (OAuth2 provider, _) -> throwE $ Right $ NeedsValidation provider a
    (Password p, _) ->
      if T.null p then throwE $ Left AccountPasswordMissing else do
        pwOk <- withExceptT (Left . AccountSqlError) $
          ExceptT $ run $ query (u, p) checkPassword
        if pwOk then return () else throwE $ Left AccountPasswordWrong
    (Trusted, _) -> return ()

tryUpdatePriv
  :: UserID
  -> PrivData
  -> AppHandler (Either Error ())
tryUpdatePriv u a@(UpdateAccount _ _ _ _ _ _ _) = run $ query (u, a) stmt
  where
    stmt :: Query (UserID, PrivData) ()
    stmt = proc params -> do
      let (usr, ac) = params
      if passwordless ac then deletePassword -< usr else let pw = newPassword ac in
        if maybe False (not . T.null) pw then updatePassword -< params
        else returnA -< ()
      if not $ null $ oauth2Removes ac
        then deleteOAuth2Login -< (usr, oauth2Removes ac)
        else returnA -< ()
      updatePriv -< params

tryUpdatePriv u (AttachProvider provider token) =
  run $ query (u, providerOpid provider, token) stmt
  where
    stmt :: Query (UserID, AuthID, Text) ()
    stmt = statement sql encode (DE.unit) True
    encode = contrazip3 (EN.value EN.int4)
      (EN.value EN.int4) (EN.value EN.text)
    sql = "INSERT INTO login_method_oauth2 \
          \(opid, identification, uid) VALUES ($2, $3, $1)"

getUserEmail
  :: UserID
  -> AppHandler Text
getUserEmail = (fromMaybe "" . hush <$>) . run . flip query stmt
  where
    stmt = statement sql (EN.value EN.int4) (DE.singleRow $ DE.value DE.text) False
    sql = "SELECT email FROM users WHERE uid=$1"
