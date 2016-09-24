module Piperka.Util where

import Network.HTTP.Types.URI (Query, encodePath)
import Blaze.ByteString.Builder (toByteString)
import Data.Text.Encoding (decodeLatin1)
import Data.Text

encodePathToText
  :: [Text]
  -> Query
  -> Text
encodePathToText = (.) (decodeLatin1 . toByteString) . encodePath
