{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS -Wno-unused-top-binds #-}

module Piperka.API.UserPrefs (userPrefs) where

import Contravariant.Extras.Contrazip
import Control.Applicative
import Control.Error.Util (bool, hoistMaybe)
import Control.Monad (void)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (split)
import Data.Monoid
import Data.Scientific
import Data.Text (Text)
import Data.UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import Hasql.Query
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Session (query, Error)
import Snap
import Snap.Snaplet.Hasql

import Application hiding (rows)
import Piperka.API.Common
import Piperka.Util (maybeParseInt)

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

data BookmarkSet = Max | Del | Page Int
  deriving (Show)

decodeBookmarkSet
  :: B.ByteString
  -> Maybe (Int, BookmarkSet)
decodeBookmarkSet b = do
  [c, d] <- return $ split ' ' b
  c' <- maybeParseInt c
  d' <- case d of
          "max" -> return Max
          "del" -> return Del
          _ -> Page <$> maybeParseInt d
  return (c', d')

userPrefs
  :: ByteString
  -> AppHandler ()
userPrefs hostname = do
  modifyResponse $
    addHeader "Access-Control-Allow-Origin" ("http://" <> hostname) .
    addHeader "Access-Control-Allow-Credentials" "true"
  bookmark <- (decodeBookmarkSet =<<) <$> getParam "bookmark[]"
  maybe readPrefs (\b -> runUserQueries $ setBookmark b) bookmark
  where
    readPrefs :: AppHandler ()
    readPrefs = do
      ses <- (fromASCIIBytes . cookieValue =<<) <$>
             getCookie sessionCookieName
      tok <- (fromASCIIBytes =<<) <$> getParam "token"
      void $ runMaybeT $
        (hoistMaybe ses >>= lift . getFullPrefs) <|>
        (hoistMaybe tok >>= lift . getOnlySubscriptions)
    getFullPrefs :: UUID -> AppHandler ()
    getFullPrefs ses = runQueries $ do
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
    -- For Piperka App
    getOnlySubscriptions :: UUID -> AppHandler ()
    getOnlySubscriptions tok = runQueries $ do
      u <-
        maybe (lift $ simpleFail 400 "Invalid or missing token") return =<<
        tokenAuth tok
      lift . writeLBS . encode . (\subs -> object ["subscriptions" .= subs]) =<<
        (ExceptT $ run $ query u $ statement sql2
         (EN.value EN.int4) (DE.rowsVector decodeSubscription) True)

setBookmark
  :: (Int, BookmarkSet)
  -> MyData
  -> UserQueryHandler ()
setBookmark (c, bookmark) p = do
  getUnread <- lift $ maybe False (== "1") <$> getParam "getunread"
  let u = uid p
  let cid = fromIntegral c
  ord <- case bookmark of
    Page o' ->
      let ord = fromIntegral $ bool 0 o' (o' > 0)
      in (ExceptT $ run $ query (u, cid, ord) $
           statement sql1
           (contrazip3
             (EN.value EN.int4)
             (EN.value EN.int4)
             (EN.value EN.int4))
           DE.unit True) >> (return $ Just ord)
    Max ->
      ExceptT $ run $ query (u, cid) $
      statement sql2
      (contrazip2 (EN.value EN.int4) (EN.value EN.int4))
      (DE.singleRow $ liftA Just $ DE.value DE.int4) True
    Del ->
      ((ExceptT $ run $ query (u, cid) $
         statement sql3
         (contrazip2 (EN.value EN.int4) (EN.value EN.int4))
         DE.unit True) >> return Nothing)
  let stats =
        (\(totalNew, newIn) ->
            (("total_new" .= totalNew :) . ("new_in" .= newIn :))) <$>
        (ExceptT $ run $ query u $
         statement sql4 (EN.value EN.int4)
         (DE.singleRow $ (,) <$> DE.value DE.int4 <*> DE.value DE.int4) True)
  addStats <- bool (return id) stats getUnread
  lift $ writeLBS $ encode $ object $ addStats . maybe
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
           \DO UPDATE SET ord = EXCLUDED.ord, subord = EXCLUDED.subord \
           \RETURNING ord+1"
    sql3 = "DELETE FROM subscriptions WHERE uid=$1 AND cid=$2"
    sql4 = "SELECT total_new, new_in FROM user_unread_stats($1)"

tokenAuth
  :: UUID
  -> ExceptT Error AppHandler (Maybe UserID)
tokenAuth token =
  ExceptT $ run $ query token $ statement sql (EN.value EN.uuid)
  (DE.maybeRow $ DE.value DE.int4) True
  where
    sql = "SELECT uid FROM p_session WHERE token_for IS NOT NULL AND ses=$1"
