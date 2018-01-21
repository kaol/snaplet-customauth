{-# LANGUAGE OverloadedStrings #-}

module Piperka.Profile.Statements where

import Contravariant.Extras.Contrazip
import Control.Applicative
import Data.Int
import Data.Text (Text)
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query

import Piperka.Profile.Types

decodeProfile :: DE.Row CommonProfile
decodeProfile =
  CommonProfile
  <$> DE.value DE.int4
  <*> DE.value DE.text
  <*> DE.value DE.bool
  <*> (liftA intToPrivacy $ DE.value DE.int4)
  <*> DE.nullableValue DE.text
  <*> DE.value DE.int4
  <*> DE.value DE.int4

decodeOwnProfile :: DE.Row OwnProfile
decodeOwnProfile =
  OwnProfile
  <$> DE.value DE.int4
  <*> DE.value DE.int4
  <*> decodeProfile

decodeOtherProfile :: DE.Row OtherProfile
decodeOtherProfile =
  OtherProfile
  <$> (liftA intToPrivacy $ DE.value DE.int4)
  <*> DE.value DE.bool
  <*> DE.value DE.bool
  <*> DE.value DE.bool
  <*> decodeProfile

profileDataFetch :: Query Text (Maybe CommonProfile)
profileDataFetch = statement sql encode (DE.maybeRow decodeProfile) True
  where
    encode = EN.value EN.text
    sql = "SELECT uid, name, perm, privacy, writeup, (totals).\"1\", (totals).\"2\" FROM \
          \(SELECT uid, name, privacy=3 AS perm, privacy, writeup, \
          \(SELECT CAST((COALESCE(SUM(ord), 0), COUNT(*)) AS integerpair) \
          \ FROM subscriptions WHERE uid=users.uid) AS totals \
          \FROM users WHERE LOWER(name)=LOWER($1)) AS x"

profileOwnDataFetch :: Query Int32 OwnProfile
profileOwnDataFetch = statement sql encode (DE.singleRow decodeOwnProfile) True
  where
    encode = EN.value EN.int4
    sql = "SELECT followers, followees, \
          \uid, name, perm, privacy, writeup, (totals).\"1\", (totals).\"2\" FROM \
          \(SELECT cast((SELECT COUNT(*) FROM follower JOIN users AS u USING (uid) \
          \ WHERE privacy > 1 AND follower.interest=users.uid) as integer) AS followers, \
          \cast((SELECT COUNT(*) FROM follower JOIN users AS u ON interest=u.uid \
          \ WHERE privacy > 1 AND follower.uid=users.uid) as integer) AS followees, \
          \uid, name, true AS perm, privacy, writeup, \
          \(SELECT CAST((COALESCE(SUM(ord),0), COUNT(*)) AS integerpair) \
          \ AS totals FROM subscriptions WHERE uid=users.uid) \
          \FROM users WHERE uid=$1) AS x"

profileOtherDataFetch :: Query (Text, Int32) (Maybe OtherProfile)
profileOtherDataFetch = statement sql encode (DE.maybeRow decodeOtherProfile) True
  where
    encode = contrazip2 (EN.value EN.text) (EN.value EN.int4)
    sql = "SELECT rprivacy, rperm, titfortat, interest, \
          \uid, name, perm, privacy, writeup, (totals).\"1\", (totals).\"2\" FROM \
          \(SELECT me.privacy AS rprivacy, \
          \(me.uid, them.uid) IN (SELECT uid, followee \
          \ FROM follow_permission) AS rperm, \
          \me.privacy=1 AND them.privacy=2 AS titfortat, \
          \(me.uid, them.uid) IN (SELECT uid, interest \
          \ FROM follower) AS interest, \
          \them.uid AS uid, them.name, \
          \CASE them.privacy WHEN 3 THEN true WHEN 2 THEN \
          \ CASE me.privacy WHEN 1 THEN false ELSE \
          \ (them.uid, me.uid) IN (SELECT uid, followee \
          \ FROM follow_permission) END \
          \END AS perm, them.privacy, them.writeup, \
          \(SELECT CAST((COALESCE(SUM(ord),0), COUNT(*)) AS integerpair) \
          \AS totals FROM subscriptions WHERE uid=them.uid) \
          \FROM users AS them, users AS me \
          \WHERE LOWER(them.name)=LOWER($1) AND me.uid=$2) AS x"
