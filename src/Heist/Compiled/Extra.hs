{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Heist.Compiled.Extra where

import Data.List
import qualified Data.IntMap as I
import qualified Data.Map.Strict as M
import Data.Map.Syntax
import Data.Text (Text)
import qualified Data.Text as T
import Heist.Compiled
import Heist.Compiled.LowLevel
import Heist
import Text.XmlHtml

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

emptySplices :: [T.Text] -> Splices (Splice a)
emptySplices = mapM_ (\x -> x ## return mempty)

emptySplices' :: [T.Text] -> Splices (b -> Splice a)
emptySplices' = mapM_ (\x -> x ## const $ return mempty)

data IndexedAction a b n = Simple
                         | forall c.
                           WithParam (a -> b -> RuntimeSplice n c)
                           (Splice n -> RuntimeSplice n c -> Splice n)

-- TODO: Error handling
stdConditionalSplice
  :: forall a b n. (Eq a, Bounded a, Enum a, Monad n)
  => (a -> (Text, IndexedAction a b n))
  -> RuntimeSplice n (a,b)
  -> Splice n
stdConditionalSplice act n = do
  node <- getParamNode
  let cs = M.fromList $ map (\x -> (elementTag x, runNodeList $ elementChildren x)) $
           childElements node
      makeSplice (t, Simple, i) = do
        x <- cs M.! t
        return (i, x)
      makeSplice (t, WithParam f m, i) = do
        let s' = cs M.! t
        x <- deferMap (uncurry f) (m s') n
        return (i, x)
      unfoldrCond f = f minBound :
        unfoldr (\a -> if a == maxBound
                       then Nothing
                       else let a' = succ a in Just (f a', a')) minBound
      nodeNames = unfoldrCond $ fst . act
      actions = unfoldrCond $ snd . act
  m <- I.fromList <$> (mapM makeSplice $ zip3 nodeNames actions $ map fromEnum
                       (enumFromTo minBound maxBound :: [a]))
  flip bindLater n $ \x -> codeGen $ m I.! (fromEnum $ fst x)
