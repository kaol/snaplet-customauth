{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Piperka.Splices.Account
       ( nullCreateSplice
       , paramValueSplice
       , accountCreateFailSplices
       ) where

import Control.Monad.Trans
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Map.Syntax
import qualified Hasql.Session
import Heist
import Heist.Compiled
import Text.XmlHtml

import Application
import Piperka.Error.Splices
import Piperka.Util (getParamText)
import Snap.Snaplet.CustomAuth.Types
import Heist.Compiled.Extra

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
paramsFromQuery paramName = (lift $ getParamText (encodeUtf8 paramName)) >>=
  return . maybe [] (\a -> [("value", a)])


accountCreateFailSplices
  :: Splices (RuntimeAppHandler (Either Hasql.Session.Error CreateFailure))
accountCreateFailSplices =
  "onCreateError" ## withSplices runChildren (do
                                                 "on" ## onErrSplice
                                                 "otherwise" ## otherError)

onErrSplice
  :: RuntimeAppHandler (Either Hasql.Session.Error CreateFailure)
onErrSplice n = do
  nodeErr <- read . T.unpack . fromJust . getAttribute "err" <$> getParamNode
  cs <- withLocalSplices ("sqlErr" ## return mempty) mempty runChildren
  flip bindLater n $ \err -> do
    case err of
      Right e -> if e == nodeErr then codeGen cs else return mempty
      Left _ -> return mempty

otherError
  :: RuntimeAppHandler (Either Hasql.Session.Error CreateFailure)
otherError n = eitherDeferMap return
  (withSplices (callTemplate "_sqlErr") sqlErrorSplices) (const $ return mempty) n
