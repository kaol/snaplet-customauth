{-# LANGUAGE OverloadedStrings #-}

module Piperka.Listing.Header.Query (alphabetIndex) where

import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Query
import Hasql.Session (query, Error)
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Snap.Snaplet.Hasql

import Application

alphabetIndex
  :: AppHandler (Either Error (Vector (Text, Int)))
alphabetIndex = run $ query () stmt
  where
    stmt = statement sql (EN.unit) (DE.rowsVector decode) True
    decode = (,)
             <$> DE.value DE.text
             <*> (fromIntegral <$> DE.value DE.int4)
    sql = "SELECT letter, ord FROM alphabet_index ORDER BY letter"
