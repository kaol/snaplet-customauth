module Piperka.Util where

import Network.HTTP.Types.URI (Query, encodePath)
import Blaze.ByteString.Builder (toByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Text

encodePathToText
  :: [Text]
  -> Query
  -> Text
encodePathToText = (.) (decodeUtf8 . toByteString) . encodePath

plural
  :: (Num a, Eq a)
  => Text
  -> Text
  -> a
  -> Text
plural s m n = if n == 1 then s else m
