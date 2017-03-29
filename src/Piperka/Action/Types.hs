module Piperka.Action.Types where

import Data.Int
import Data.Text hiding (empty)
import Data.Vector (Vector, empty)
import qualified Hasql.Session as S

data ActionError = SqlError S.Error
                 | CsrfFail
                 | CsrfFailWithComic Text
                 | UnknownAction
                 | NeedsLogin
                 deriving (Show, Eq)

data Action = Logout
            | Bookmark [(Int,Text,(Maybe (Int, Int, Bool)))]
            | Subscribe Int Bool
            | Unsubscribe Int
            | Revert (Vector Int32)
            deriving (Show, Eq)

instance Enum Action where
  fromEnum Logout = 0
  fromEnum (Bookmark _) = 1
  fromEnum (Subscribe _ _) = 2
  fromEnum (Unsubscribe _) = 3
  fromEnum (Revert _) = 4
  toEnum 0 = Logout
  toEnum 1 = Bookmark []
  toEnum 2 = Subscribe 0 False
  toEnum 3 = Unsubscribe 0
  toEnum 4 = Revert empty
  toEnum _ = error "Action"

instance Bounded Action where
  minBound = Logout
  maxBound = Revert empty
