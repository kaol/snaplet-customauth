{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Piperka.Readers (renderReaders) where

import Contravariant.Extras.Contrazip
import Control.Applicative
import Control.Arrow
import Control.Error.Util
import Control.Monad.Trans
import Data.Map.Syntax
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.Query
import Hasql.Session (query, Error)
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Heist
import Heist.Compiled
import Heist.Compiled.Extra (eitherDeferMap, runConditionalChildren)
import qualified HTMLEntities.Text as HTML
import Network.URI.Encode (encodeText)
import Snap.Snaplet.Hasql

import Application
import Piperka.Error.Splices
import Piperka.Util (getCid)

data ReadersInfo = ReadersInfo
  { title :: Text
  , total :: Int
  , users :: Vector Reader
  }

data Reader = Reader
  { name :: Text
  , activeUser :: Bool
  }

decodeReadersInfo
  :: DE.Row (Vector Reader -> ReadersInfo)
decodeReadersInfo =
  ReadersInfo
  <$> liftA HTML.text (DE.value DE.text)
  <*> liftA fromIntegral (DE.value DE.int4)

decodeUsers
  :: DE.Row Reader
decodeUsers =
  Reader
  <$> DE.value DE.text
  <*> DE.value DE.bool

decodeUsers'
  :: DE.Row Reader
decodeUsers' =
  Reader
  <$> DE.value DE.text
  <*> pure False

queryReadersInfo
  :: Maybe UserID
  -> Int
  -> AppHandler (Either Error (Maybe ReadersInfo))
queryReadersInfo u c =
  run $ query (c, u) stmt
  where
    readersUnlog = statement sql2 (EN.value EN.int4)
                   (DE.rowsVector decodeUsers') True
    readersLogged = statement sql3
                    (contrazip2 (EN.value EN.int4) (EN.value EN.int4))
                    (DE.rowsVector decodeUsers) True
    stmt = proc params -> do
      let c' = fromIntegral $ fst params
      info1 <- (statement sql1 (EN.value EN.int4)
                (DE.maybeRow decodeReadersInfo) True) -< c'
      if isNothing info1
        then returnA -< Nothing
        else do
        info2 <- if isNothing $ snd params
                 then readersUnlog -< c'
                 else readersLogged -< (c', fromJust $ snd params)
        returnA -< info1 <*> pure info2
    sql1 = "SELECT title, readers FROM comics WHERE cid=$1 "
    sql2 = "SELECT name FROM users JOIN subscriptions USING (uid) \
           \WHERE cid=$1 AND countme AND privacy=3"
    sql3 = "SELECT name, countme OR users.uid = $2 \
           \FROM users JOIN subscriptions USING (uid) \
           \LEFT JOIN (SELECT cid, interest AS uid, interest \
           \FROM permitted_interest WHERE uid=$2) AS i USING (cid, uid) \
           \WHERE cid=$1 AND ((privacy=3 AND countme) OR \
           \interest IS NOT NULL OR uid=$2) \
           \ORDER BY interest IS NOT NULL DESC, LOWER(name)"

renderReaders
  :: RuntimeAppHandler (Maybe MyData)
renderReaders = eitherDeferMap getParams stdSqlErrorSplice
                (withSplices runChildren readersSplices)
  where
    getParams n = lift $ do
      let u = uid <$> n
      maybe (return $ Right Nothing)
        (\c -> (fmap ((fmap . fmap) (c,))) $ queryReadersInfo u c) =<<
        (fmap snd) <$> getCid

readersSplices
  :: Splices (RuntimeAppHandler (Maybe (Int, ReadersInfo)))
readersSplices = do
  "positive" ## mayDeferMap return
    (withSplices runChildren positiveSplices)
  "missing" ## runConditionalChildren . fmap isNothing

positiveSplices
  :: Splices (RuntimeAppHandler (Int, ReadersInfo))
positiveSplices = do
  "title" ## pureSplice . textSplice $ title . snd
  "total" ## pureSplice . textSplice $ T.pack . show . total . snd
  "cid" ## pureSplice . textSplice $ T.pack . show . fst
  "some" ## runConditionalChildren . fmap (not . V.null . users . snd)
  "none" ## runConditionalChildren . fmap (V.null . users . snd)
  "list" ## manyWith runChildren listSplices listAttrSplices . fmap (users . snd)
  "hidden" ## runConditionalChildren . fmap ((>0) . total . snd)

listAttrSplices
  :: Splices (RuntimeSplice AppHandler Reader -> AttrSplice AppHandler)
listAttrSplices = "passive" ## \n _ -> do
  return . bool [("class", "passiveuser")] [] . activeUser =<< n

listSplices
  :: Splices (RuntimeAppHandler Reader)
listSplices = mapV (pureSplice . textSplice) $ do
  "user" ## HTML.text . name
  "userURL" ## encodeText . name
