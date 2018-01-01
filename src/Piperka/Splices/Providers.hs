{-# LANGUAGE OverloadedStrings #-}
module Piperka.Splices.Providers (renderProviders) where

import Control.Monad.Trans
import Data.Map.Syntax
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session (query, Error)
import Heist
import Heist.Compiled
import Snap.Snaplet.Hasql

import Application
import Piperka.Error.Splices
import Heist.Compiled.Extra

renderProviders
  :: RuntimeAppHandler a
renderProviders _ =
  eitherDeferMap  return stdSqlErrorSplice
  (manyWith runChildren providerSplices providerAttrSplices) $
  lift getProviders

providerSplices
  :: Splices (RuntimeAppHandler (Text, Text))
providerSplices = do
  "name" ## pureSplice . textSplice $ snd

providerAttrSplices
  :: Splices (RuntimeSplice AppHandler (Text, Text) -> AttrSplice AppHandler)
providerAttrSplices = "href" ## \n t -> do
  i <- fst <$> n
  return [("href", t <> i)]

getProviders
  :: AppHandler (Either Error (Vector (Text, Text)))
getProviders = run $ query () stmt
  where
    stmt = statement sql EN.unit (DE.rowsVector decoder) True
    sql = "SELECT name, display_name FROM oauth2_provider \
          \ORDER BY display_name DESC"
    decoder = (,)
      <$> DE.value DE.text
      <*> DE.value DE.text
