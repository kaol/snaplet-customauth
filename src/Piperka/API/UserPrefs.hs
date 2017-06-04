{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Arrows #-}

{-# OPTIONS -Wno-unused-top-binds #-}

module Piperka.API.UserPrefs (userPrefs) where

import Contravariant.Extras.Contrazip
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import Data.UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import Hasql.Query
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Session (query)
import Snap
import Snap.Snaplet.Hasql

import Application hiding (rows)
import Piperka.API.Common

data Subscription = Subscription Int Int Int Int Int

instance ToJSON Subscription where
  toJSON (Subscription a b c d e) = Array $ V.fromList $
    map (Number . flip scientific 0 . toInteger) [a, b, c, d, e]

decodeSubscription :: DE.Row Subscription
decodeSubscription =
  Subscription
  <$> (liftA fromIntegral $ DE.value DE.int4)
  <*> (liftA fromIntegral $ DE.value DE.int4)
  <*> (liftA fromIntegral $ DE.value DE.int4)
  <*> (liftA fromIntegral $ DE.value DE.int4)
  <*> (liftA fromIntegral $ DE.value DE.int4)

data UserInfo = UserInfo {
    name :: Text
  , new_windows :: Bool
  , rows :: Int
  , cols :: Int
  , subscriptions :: Vector Subscription
  } deriving (Generic)

decodeUserPrefs
  :: DE.Row (UserID, Vector Subscription -> UserInfo)
decodeUserPrefs =
  (,)
  <$> DE.value DE.int4
  <*> (UserInfo
       <$> DE.value DE.text
       <*> DE.value DE.bool
       <*> (liftA fromIntegral $ DE.value DE.int4)
       <*> (liftA fromIntegral $ DE.value DE.int4)
      )

instance ToJSON UserInfo where
  toEncoding = genericToEncoding defaultOptions

userPrefs :: AppHandler ()
userPrefs = do
  bookmark <- ((decode :: B.ByteString -> Maybe [Value]) . fromStrict =<<)
              <$> getParam "bookmark[]"
  maybe readPrefs (\b -> runUserQueries $ setBookmark b) bookmark
  where
    readPrefs = do
      ses <- maybe (simpleFail 403 "Session cookie required") return =<<
             (fromASCIIBytes . cookieValue =<<) <$> getCookie "_session"
      runQueries $ do
        (u, f) <-
          maybe (lift $ simpleFail 400 "User authentication failed") return =<<
          (ExceptT $ run $ query ses $ statement sql1
           (EN.value EN.uuid) (DE.maybeRow decodeUserPrefs) True)
        lift . writeLBS . encode . f =<<
          (ExceptT $ run $ query u $ statement sql2
            (EN.value EN.int4) (DE.rowsVector decodeSubscription) True)
    sql1 = "SELECT uid, name, new_windows, display_rows, display_columns \
           \FROM recover_session($1) JOIN users USING (uid)"
    sql2 = "SELECT cid, ord+\
           \CASE WHEN subord > COALESCE((SELECT MAX(subord) FROM page_fragments \
           \WHERE cid=subscriptions.cid AND ord=subscriptions.ord), 0) \
           \THEN 1 ELSE 0 END, \
           \max_ord_of(cid), max_subord_for(cid, max_ord_of(cid)), num \
           \FROM comics JOIN subscriptions USING (cid) \
           \JOIN comic_remain_frag($1) USING (cid) WHERE uid=$1 \
           \ORDER BY Num DESC, ordering_form(title)"

setBookmark
  :: [Value]
  -> UserPrefs
  -> UserQueryHandler ()
setBookmark bookmark p = do
  lift $ validateCsrf
  let u = uid $ fromJust $ user p
  let act = case bookmark of
        [Number c, o] ->
          toBoundedInteger c >>= \cid ->
          case o of
            Number o' -> do
              ord <- (\n -> if n < 0 then 0 else n) <$> toBoundedInteger o'
              return ((ExceptT $ run $ query (u, cid, ord) $
                       statement sql1
                       (contrazip3
                        (EN.value EN.int4)
                        (EN.value EN.int4)
                        (EN.value EN.int4))
                       DE.unit True) >> (return $ Just ord))
            String "max" ->
              return $ ExceptT $ run $ query (u, cid) $
              statement sql2
              (contrazip2 (EN.value EN.int4) (EN.value EN.int4))
              (DE.singleRow $ liftA Just $ DE.value DE.int4) True
            String "del" ->
              return $ ((ExceptT $ run $ query (u, cid) $
                         statement sql3
                         (contrazip2 (EN.value EN.int4) (EN.value EN.int4))
                         DE.unit True) >> return Nothing)
            _ -> Nothing
        _ -> Nothing
  ord <- fromMaybe (lift $ simpleFail 400 "Invalid bookmark[] parameter") act
  lift $ writeLBS $ encode $ object $ maybe
    id (\o -> ("ord" .= o :)) ord $ ["ok" .= True]
  where
    sql1 = "INSERT INTO subscriptions (uid, cid, ord, subord) \
           \SELECT $1, cid, ord, CASE WHEN $3 = 0 THEN 0 ELSE \
           \COALESCE((SELECT MAX(subord)+1 \
           \FROM page_fragments WHERE cid=updates.cid \
           \AND ord=updates.ord), 1) END FROM updates WHERE cid=$2 AND \
           \ord = CASE WHEN $3 = 0 THEN 0 ELSE $3-1 END \
           \ON CONFLICT (uid, cid) \
           \DO UPDATE SET ord = EXCLUDED.ord, subord = EXCLUDED.subord"
    sql2 = "INSERT INTO subscriptions (uid, cid, ord, subord) \
           \SELECT $1, cid, ord, \
           \COALESCE((SELECT MAX(subord)+1 FROM page_fragments \
           \WHERE cid=updates.cid AND ord=updates.ord), 1) \
           \FROM updates WHERE cid=$2 ORDER BY ord DESC LIMIT 1 \
           \ON CONFLICT (uid, cid) \
           \DO UPDATES SET ord = EXCLUDED.ord, subord = EXCLUDED.subord \
           \RETURNING ord+1"
    sql3 = "DELETE FROM subscriptions WHERE uid=$1 AND cid=$2"
