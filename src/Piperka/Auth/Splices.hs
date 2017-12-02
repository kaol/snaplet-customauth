{-# LANGUAGE OverloadedStrings #-}

module Piperka.Auth.Splices (authErrorSplices) where

import Data.Maybe (fromJust, isJust)
import Data.Monoid
import qualified Data.Text as T
import Data.Map.Syntax
import Hasql.Session (Error)
import Heist
import Heist.Compiled
import qualified Text.XmlHtml as X
import Snap.Snaplet.CustomAuth(AuthFailure(..), CreateFailure(..))
import Snap.Snaplet.CustomAuth.OAuth2(AuthFailure(..), OAuth2Failure(..))

import Application
import Piperka.Error.Splices

authErrorSplices
  :: Splices (RuntimeAppHandler (AuthFailure Error))
authErrorSplices = do
  let getLoginFail (Login x) = Just x
      getLoginFail _ = Nothing
      getCreateFail (Create x) = Just x
      getCreateFail _ = Nothing
      getActionFail (Action x) = Just x
      getActionFail _ = Nothing
      getOtherFail (UserError x) = Just x
      getOtherFail _ = Nothing
      authDefer f splices = mayDeferMap (return . f)
        (withSplices runChildren splices)

  "loginFailure" ## authDefer getLoginFail onErr
  "createFailure" ## authDefer getCreateFail $ onErr <> providerError
  "actionFailure" ## authDefer getActionFail onErr
  "otherwise" ## mayDeferMap (return . getOtherFail) stdSqlErrorSplice

onErr
  :: (Read e, Eq e)
  => Splices (RuntimeAppHandler e)
onErr = "on" ## \n -> do
  test <- (==) . read . T.unpack . fromJust . (X.getAttribute "err") <$> getParamNode
  s <- runChildren
  flip bindLater n $ \x -> if test x then codeGen s else return mempty

providerError
  :: Splices (RuntimeAppHandler CreateFailure)
providerError = "providerError" ##
  let getProviderError (OAuth2Failure (ProviderError e)) = Just e
      getProviderError _ = Nothing in
    mayDeferMap (return . getProviderError)
    (withSplices runChildren ("error" ## renderError))
  where
    renderError n = do
      def <- runChildren
      t <- deferMany (return . yieldRuntimeText) n
      flip bindLater n $ \x -> codeGen $ if isJust x then t else def
