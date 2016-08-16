{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Heist.Compiled.Extra where

import Heist.Compiled
import Heist.Compiled.LowLevel
import Heist
import qualified Data.Text as T
import Text.XmlHtml

eitherDeferMap :: Monad n
               => (a -> RuntimeSplice n (Either b c))
               -> (RuntimeSplice n b -> Splice n)
               -> (RuntimeSplice n c -> Splice n)
               -> RuntimeSplice n a -> Splice n
eitherDeferMap f pfs pff n = do
  p2 <- newEmptyPromise
  p3 <- newEmptyPromise
  actionSuccess <- pfs $ getPromise p2
  actionFailure <- pff $ getPromise p3
  return $ yieldRuntime $ do
    either
      (\x -> putPromise p2 x >> codeGen actionSuccess)
      (\x -> putPromise p3 x >> codeGen actionFailure) =<< f =<< n

checkedSplice
  :: forall (n :: * -> *) a. Monad n
  => (a -> Bool)
  -> RuntimeSplice n a
  -> Splice n
checkedSplice f runtime = do
  tpl <- runChildren
  checkVal <- maybe True (read . T.unpack) . getAttribute "check"
              <$> getParamNode
  return $ yieldRuntime $ do
    check <- fmap f runtime
    if check == checkVal then codeGen tpl else mempty

conditionalChildren
  :: forall (n :: * -> *) t. Monad n
  => (RuntimeSplice n t -> Splice n)
  -> (t -> Bool)
  -> RuntimeSplice n t
  -> Splice n
conditionalChildren splice test runtime = do
  cs <- splice runtime
  return $ yieldRuntime $ do
    prefs <- runtime
    if (test prefs)
      then codeGen cs
      else mempty

