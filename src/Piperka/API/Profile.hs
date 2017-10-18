{-# LANGUAGE OverloadedStrings #-}

module Piperka.API.Profile (profileSubmission) where

import Contravariant.Extras.Contrazip
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Maybe
import Data.String (IsString)
import qualified Data.Vector as V
import Hasql.Decoders as DE
import Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session hiding (run, sql)
import Snap
import Snap.Snaplet.Hasql

import Application
import Piperka.API.Common
import Piperka.Util (getParamText)

data Action = GetSubs | Follow | UnFollow | Permit | Deny
  deriving (Eq)

parseAction
  :: (IsString a, Eq a)
  => a
  -> Maybe Action
parseAction "getSubs" = Just GetSubs
parseAction "follow" = Just Follow
parseAction "unfollow" = Just UnFollow
parseAction "permit" = Just Permit
parseAction "deny" = Just Deny
parseAction _ = Nothing

profileSubmission
  :: AppHandler ()
profileSubmission = do
  name <- maybe (simpleFail 400 "Required parameter name missing") return =<<
          getParamText "name"
  act <- maybe (simpleFail 400 "Unknown action") return =<<
         maybe (simpleFail 400 "Required parameter action missing") (return . parseAction) =<<
         getParam "action"
  runMaybeUserQueries $ \usr -> do
    let u = uid <$> usr
    tgt <- ExceptT $ run $ query
      (u, name) $
      statement sql (contrazip2 (EN.nullableValue EN.int4) (EN.value EN.text))
      (DE.maybeRow ((,) <$> (DE.value DE.int4) <*> (DE.value DE.bool))) True
    let noSuchUser = lift $ simpleFail 200 "No such user"
    (target, perm) <- maybe noSuchUser return tgt
    when (not perm) noSuchUser
    let profileAction sql' = do
          when (isNothing u) $ lift $ simpleFail 200 "You are not logged in."
          u' <- uid <$> (lift $ validateCsrf)
          when (target == u') $
            lift $ simpleFail 200 "Tricksy, trying to make a relationship with yourself."
          ExceptT $ withTop db $ run $ query (u', target) $
            statement sql' (contrazip2 (EN.value EN.int4) (EN.value EN.int4))
            DE.unit True
    case act of
      GetSubs -> do
        subs <- if Just target == u then return V.empty else
          ExceptT $ run $ query target $
          statement sql' (EN.value EN.int4)
          (DE.rowsVector (DE.value DE.int4)) True
        lift $ writeLBS $ encode $ object [ "subs" .= subs ]
          where sql' = "SELECT cid FROM subscriptions WHERE uid=$1"
      Follow -> profileAction
        "INSERT INTO follower (uid, interest) VALUES ($1, $2) \
        \ON CONFLICT (uid, interest) DO NOTHING"
      UnFollow -> profileAction
        "DELETE FROM follower WHERE uid=$1 AND interest = $2"
      Permit -> profileAction
        "INSERT INTO follow_permission (uid, followee) VALUES ($1, $2) \
        \ON CONFLICT (uid, followerr) DO NOTHING"
      Deny -> profileAction
        "DELETE FROM follow_permission WHERE uid=$1 AND followee = $2"
    where
      sql = "SELECT uid, (privacy=3 OR (privacy=2 AND uid IN \
            \(SELECT followee FROM follow_permission WHERE followee=$1))) \
            \FROM users WHERE LOWER(name)=LOWER($2)"
