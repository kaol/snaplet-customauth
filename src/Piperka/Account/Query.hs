{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Piperka.Account.Query (getAccountSettings, updateUnpriv, tryUpdatePriv) where

import Control.Applicative
import Control.Arrow
import Contravariant.Extras.Contrazip
import Data.Functor.Contravariant
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Hasql.Decoders as DE
import Hasql.Encoders as EN
import Hasql.Session hiding (run, sql)
import Hasql.Query
import Snap.Snaplet.Hasql (run)

import Application
import Piperka.Account.Types ( AccountUpdateError(..)
                             , AccountUpdate(..)
                             , UserAccountSettings(..))
import Piperka.Listing.Types (columnsToInt)
import Piperka.Profile.Types (intToPrivacy, privacyToInt)

getAccountSettings
  :: UserID
  -> AppHandler (Either Error UserAccountSettings)
getAccountSettings u = do
  run $ query u stmt
  where
    stmt = statement sql encode (DE.singleRow decode) True
    encode = EN.value EN.int4
    decode = UserAccountSettings
             <$> (liftA intToPrivacy $ DE.value DE.int4)
             <*> DE.nullableValue DE.text
             <*> DE.nullableValue DE.text
    sql = "SELECT privacy, email, writeup FROM users WHERE uid=$1"

checkPassword
  :: Query (UserID, AccountUpdate) Bool
checkPassword = statement sql encode (DE.singleRow $ DE.value DE.bool) True
  where
    encode = (contramap fst (EN.value EN.int4)) <>
             (contramap (password . snd) (EN.value EN.text))
    sql = "SELECT (hash = crypt($2, hash)) AS pwmatch \
          \FROM users JOIN user_login USING (uid) \
          \JOIN login_method_passwd USING (lmid) WHERE uid=$1"

updateUnpriv
  :: UserID
  -> AccountUpdate
  -> AppHandler (Either Error ())
updateUnpriv u a = run $ query (u, a) stmt
  where
    stmt = statement sql encode DE.unit True
    encode = contrazip2 (EN.value EN.int4)
             ((contramap newWindows (EN.value EN.bool)) <>
              (contramap (fromIntegral . rows') (EN.value EN.int4)) <>
              (contramap (columnsToInt . columns') (EN.value EN.int4)))
    sql = "UPDATE users SET new_windows=$2, display_rows=$3, display_columns=$4 WHERE uid=$1"


updatePriv
  :: Query (UserID, AccountUpdate) ()
updatePriv = statement sql encode DE.unit True
  where
    scrub = (=<<) (\x -> if T.null x then Nothing else Just x)
    encode = contrazip2 (EN.value EN.int4)
             ((contramap (scrub . email') (EN.nullableValue EN.text)) <>
              (contramap (privacyToInt . privacy') (EN.value EN.int4)) <>
              (contramap (scrub . writeup') (EN.nullableValue EN.text)))
    sql = "UPDATE users SET email=$2, privacy=$3, writeup=$4 WHERE uid=$1"

-- TODO: needs to be rethought when other login methods are available
updatePassword
  :: Query (UserID, AccountUpdate) ()
updatePassword = statement sql encode DE.unit True
  where
    encode = contrazip2 (EN.value EN.int4)
      (contramap (fromJust . newPassword) (EN.value EN.text))
    sql = "UPDATE login_method_passwd SET hash=crypt($2, hash) FROM user_login \
          \WHERE login_method_passwd.lmid=user_login.lmid AND uid=$1"

tryUpdatePriv
  :: UserID
  -> AccountUpdate
  -> AppHandler (Either Error (Maybe AccountUpdateError))
tryUpdatePriv u a = run $ query (u, a) stmt
  where
    checkPwEmpty = T.null . password . snd
    checkPwChange = maybe False (not . T.null) . newPassword . snd
    checkPwMismatch a' = let n = newPassword $ snd a'
                             n' = newPasswordRetype $ snd a'
                         in n /= n'
    stmt :: Query (UserID, AccountUpdate) (Maybe AccountUpdateError)
    stmt = proc params -> do
      pwEmpty <- arr checkPwEmpty -< params
      pwChange <- arr checkPwChange -< params
      pwMismatch <- arr checkPwMismatch -< params
      case (pwEmpty, pwChange && pwMismatch) of
        (True, _) -> returnA -< Just AccountPasswordMissing
        (_, True) -> returnA -< Just AccountNewPasswordMismatch
        _ -> do
          pwOk <- checkPassword -< params
          if not pwOk
            then returnA -< Just AccountPasswordWrong
            else do
              updatePriv -< params
              if pwChange
                then updatePassword -< params
                else returnA -< ()
              returnA -< Nothing
