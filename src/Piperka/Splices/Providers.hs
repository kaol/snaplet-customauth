{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Piperka.Splices.Providers (renderProviders) where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Configurator as C
import Data.Map.Syntax
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Heist
import Heist.Compiled
import Snap.Snaplet

import Application

renderProviders
  :: Splice AppHandler
renderProviders = do
  manyWith runChildren providerSplices providerAttrSplices $ do
    cfg <- lift getSnapletUserConfig
    liftIO $ catMaybes <$>
      (mapM (\n -> runMaybeT $ do
                _ :: Int <- MaybeT $ C.lookup cfg ("oauth2." <> n <> ".opid")
                MaybeT $ (fmap (n,)) <$> C.lookup cfg ("oauth2." <> n <> ".label")
            ) =<<
       C.lookupDefault [] cfg "oauth2.providers")

providerSplices
  :: Splices (RuntimeAppHandler (Text, Text))
providerSplices = do
  "name" ## pureSplice . textSplice $ snd

providerAttrSplices
  :: Splices (RuntimeSplice AppHandler (Text, Text) -> AttrSplice AppHandler)
providerAttrSplices = "href" ## \n t -> do
  i <- fst <$> n
  return [("href", t <> i)]
