{-# LANGUAGE OverloadedStrings #-}

module Piperka.OAuth2.Query where

import Contravariant.Extras.Contrazip
import Data.Functor.Contravariant
import Data.Text (Text)
import Hasql.Decoders as DE
import Hasql.Encoders as EN
import Hasql.Session hiding (run, sql)
import Hasql.Query
import Snap
import Snap.Snaplet.Hasql (Hasql, run)

import Application
import Piperka.OAuth2.Types

reserveOAuth2Identity
  :: Provider
  -> Text
  -> Handler App Hasql (Either Error AuthID)
reserveOAuth2Identity provider token = run $ query (provider, token) stmt
  where
    stmt = statement sql encode (DE.singleRow decode) True
    encode = contrazip2
      (EN.value $ (fromIntegral . providerOpid) >$< EN.int4)
      (EN.value EN.text)
    decode = DE.value DE.int4
    sql = "INSERT INTO login_method_oauth2 (opid, identification) \
          \VALUES ($1, $2) RETURNING lmid"
