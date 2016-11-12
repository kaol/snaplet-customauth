module Piperka.Util where

import Network.HTTP.Types.URI (Query, encodePath, urlEncode)
import Blaze.ByteString.Builder (Builder, toByteString)
import Blaze.ByteString.Builder.ByteString (fromByteString)
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
