{-# LANGUAGE OverloadedStrings #-}

module Piperka.Splices.Revert (renderRecent) where

import Control.Applicative
import Control.Monad.Trans
import Data.Map.Syntax
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Text as T
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session (Error, query)
import Heist
import Heist.Compiled
import Heist.Compiled.Extra (eitherDeferMap)
import Snap.Snaplet.Hasql (run)

import Application
import Piperka.Error.Splices

renderRecent
  :: RuntimeAppHandler MyData
renderRecent n =
  eitherDeferMap return stdSqlErrorSplice
  (manyWith runChildren renderSplices renderAttrSplices) ((lift . getRecent) =<< n)

renderSplices
  :: Splices (RuntimeAppHandler (Text, Text))
renderSplices = "name" ## pureSplice . textSplice $ snd

renderAttrSplices
  :: Splices (RuntimeSplice AppHandler (Text, Text) -> AttrSplice AppHandler)
renderAttrSplices = "id" ## \n t -> do
  c <- fst <$> n
  let idVal = "recent" <> c
  return $ case t of
    "id" -> [("id", idVal), ("value", c)]
    "for" -> [("for", idVal)]
    _ -> undefined

getRecent
  :: MyData
  -> AppHandler (Either Error (Vector (Text, Text)))
getRecent p = do
  let u = uid p
  run $ query u stmt
  where
    stmt = statement sql (EN.value EN.int4) (DE.rowsVector decoder) True
    sql = "SELECT cid, comics.title FROM recent JOIN comics USING (cid) \
          \WHERE recent.uid=$1 ORDER BY recent.used_on DESC"
    decoder = (,)
      <$> (liftA (T.pack . show) (DE.value DE.int4))
      <*> DE.value DE.text
