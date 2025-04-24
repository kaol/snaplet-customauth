module Snap.Snaplet.CustomAuth.Challenge where

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI, mk)
import Data.Char
import Data.Word
import Prelude hiding (takeWhile)

data WWWAuthenticateChallenge = WWWAuthenticateChallenge
  { challengeScheme :: CI ByteString
  , challengeParams :: [(CI ByteString, ByteString)]
  }
  deriving (Show, Eq)

parseWWWAuthenticateChallenge :: ByteString -> [WWWAuthenticateChallenge]
parseWWWAuthenticateChallenge x = either (const []) id $ flip parseOnly x $
  parseChallenge `sepBy` (char ',' *> spaces)
  where
    f :: (Enum a, Enum b) => a -> b
    f = toEnum . fromEnum
    spaces = skipWhile (isSpace . f)
    parseChallenge =
      WWWAuthenticateChallenge
      <$> (mk <$> takeWhile1 (/= (f ' ')) <* spaces)
      <*> authParam `sepBy1` (char ',' *> spaces)
    authParam =
      (,)
      <$> (mk <$> takeWhile1 (/= f '='))
      <* char '='
      <*> (((char '"' *> takeWhile (/= f '"') <* char '"') <|>
           (takeWhile (isAlphaNum . f))
           ) <|>
           (takeWhile (/= f ' ')))
      <* spaces
