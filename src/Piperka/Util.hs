{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Piperka.Util
  ( encodePathToText
  , urlEncodeTextToBuilder
  , plural
  , formatTime'
  , if'
  , maybeParseInt
  , maybeDecodeText
  , monadicTuple2
  , monadicTuple3
  , randomString
  , getParamText
  , getParamInt
  , getCid
  ) where

import Control.Error.Util (hush, hoistMaybe)
import Control.Monad
import Control.Monad.Trans.Maybe
import Network.HTTP.Types.URI (Query, encodePath, urlEncode)
import Blaze.ByteString.Builder (Builder, toByteString)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Text
import Data.Time.Format
import Snap (getParam, MonadSnap, Handler)
import System.Random (randomRIO)

encodePathToText
  :: [Text]
  -> Query
  -> Text
encodePathToText = (.) (decodeUtf8 . toByteString) . encodePath

urlEncodeTextToBuilder
  :: Text
  -> Builder
urlEncodeTextToBuilder = fromByteString . urlEncode False . encodeUtf8

plural
  :: (Num a, Eq a)
  => Text
  -> Text
  -> a
  -> Text
plural s m n = if n == 1 then s else m

formatTime'
  :: (FormatTime t)
  => t
  -> String
formatTime' = formatTime defaultTimeLocale "%F %T"

if' :: a -> a -> Bool -> a
if' t f v = if v then t else f

maybeParseInt
  :: B.ByteString
  -> Maybe Int
maybeParseInt s = do
  (i,x) <- B.readInt s
  guard $ B.null x
  return i

maybeDecodeText
  :: B.ByteString
  -> Maybe Text
maybeDecodeText = hush . decodeUtf8'

monadicTuple2
  :: forall (m :: * -> *) a b. Monad m
  => (m a, m b)
  -> m (a, b)
monadicTuple2 (a, b) = do
  a' <- a
  b' <- b
  return (a', b')

monadicTuple3
  :: forall (m :: * -> *) a b c. Monad m
  => (m a, m b, m c)
  -> m (a, b, c)
monadicTuple3 (a, b, c) = do
  a' <- a
  b' <- b
  c' <- c
  return (a', b', c')

randomSafeCharFromInt
  :: Int
  -> Char
randomSafeCharFromInt i
  | i < 10 = chr (i+48)
  | i < 36 = chr (i+55)
  | otherwise = chr (i+61)

randomString
  :: Int
  -> IO Text
randomString n = pack <$> replicateM n (randomSafeCharFromInt <$> randomRIO (0,61))

getParamText
  :: forall (f :: * -> *).
     MonadSnap f
  => ByteString
  -> f (Maybe Text)
getParamText name = (hush . decodeUtf8' =<<) <$> getParam name

getParamInt
  :: ByteString
  -> Handler b v (Maybe (ByteString, Int))
getParamInt name = runMaybeT $ do
  raw <- MaybeT $ getParam name
  (c,x) <- hoistMaybe $ B.readInt raw
  guard $ B.null x
  return (raw, c)

getCid
  :: Handler b v (Maybe (ByteString, Int))
getCid = getParamInt "cid"
