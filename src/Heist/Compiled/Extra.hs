{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Heist.Compiled.Extra where

import Data.Map.Syntax
import Heist.Compiled
import Heist.Compiled.LowLevel
import Heist
import qualified Data.Text as T
import Text.XmlHtml

defer
  :: Monad n
  => (RuntimeSplice n a -> Splice n)
  -> RuntimeSplice n a
  -> Splice n
defer = deferMap return

eitherDeferMap :: Monad n
               => (a -> RuntimeSplice n (Either b c))
               -> (RuntimeSplice n b -> Splice n)
               -> (RuntimeSplice n c -> Splice n)
               -> RuntimeSplice n a -> Splice n
eitherDeferMap f pff pfs n = do
  pf <- newEmptyPromise
  ps <- newEmptyPromise
  actionSuccess <- pfs $ getPromise ps
  actionFailure <- pff $ getPromise pf
  return $ yieldRuntime $
    either
      (\x -> putPromise pf x >> codeGen actionFailure)
      (\x -> putPromise ps x >> codeGen actionSuccess) =<< f =<< n

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

checkedAttrSplice
  :: forall (n :: * -> *) t. Monad n
  => (t -> Bool)
  -> RuntimeSplice n t
  -> Splices (AttrSplice n)
checkedAttrSplice test runtime = do
  "checked" ## const $ do
    val <- runtime
    return $ if test val then [("checked", "")] else []
