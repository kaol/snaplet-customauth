{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Piperka.Util where

import Control.Monad
import Network.HTTP.Types.URI (Query, encodePath, urlEncode)
import Blaze.ByteString.Builder (Builder, toByteString)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text
import Data.Time.Format

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
