{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Piperka.Splices.Account
       ( nullCreateSplice
       , paramValueSplice
       , accountCreateFailSplices
       ) where

import Control.Monad.Trans
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Map.Syntax
import qualified Hasql.Session
import Heist
import Heist.Compiled
import Snap
import Text.XmlHtml

import Application
import Piperka.Error.Splices
import Snap.Snaplet.CustomAuth.Types

nullCreateSplice
  :: Splice AppHandler
  -> Splice AppHandler
nullCreateSplice =
  withLocalSplices ("onCreateError" ## return mempty)
  ("value" ## paramsFromQuery)

paramValueSplice
  :: Splice AppHandler
  -> Splice AppHandler
paramValueSplice =
  withLocalSplices mempty ("value" ## paramsFromQuery)

paramsFromQuery
  :: Text
  -> RuntimeSplice AppHandler [(Text, Text)]
paramsFromQuery paramName = (lift $ getParam (encodeUtf8 paramName)) >>=
  return . maybe [] (\a -> [("value", decodeUtf8 a)])


accountCreateFailSplices
  :: Splices (RuntimeAppHandler (CreateFailure Hasql.Session.Error))
accountCreateFailSplices =
  "onCreateError" ## withSplices runChildren ("on" ## onErrSplice)

getSqlError
  :: forall a. (Eq a, Show a)
  => CreateFailure a
  -> Maybe a
getSqlError (AvailError e) = Just e
getSqlError (CreateError e) = Just e
getSqlError _ = Nothing

testErrorType
  :: forall a. (Eq a, Show a)
  => CreateFailure a
  -> Text
  -> Bool
testErrorType MissingName = (== "MissingName")
testErrorType NameUsed = (== "NameUsed")
testErrorType PasswordMismatch = (== "PasswordMismatch")
testErrorType NoPassword = (== "PasswordMissing")
testErrorType (AvailError _) = (== "AvailError")
testErrorType (CreateError _) = (== "CreateError")

onErrSplice
  :: RuntimeAppHandler (CreateFailure Hasql.Session.Error)
onErrSplice n = do
  nodeErr <- fromJust . getAttribute "err" <$> getParamNode
  plain <- withLocalSplices ("sqlErr" ## return mempty) mempty runChildren
  withSqlErr <- mayDeferMap (return . getSqlError)
                (withSplices (callTemplate "_sqlErr") sqlErrorSplices) n
  flip bindLater n $ \err -> do
    if testErrorType err nodeErr
      then codeGen $ if isJust $ getSqlError err then withSqlErr else plain
      else return mempty
