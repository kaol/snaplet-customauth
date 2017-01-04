module Piperka.Action.Types where

import Data.Text
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
            deriving (Show, Eq)
