{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Piperka.API.Submit.Splices (submitAPISplices) where

import Control.Lens
import Control.Monad.Trans
import Data.Maybe
import Data.Monoid
import qualified Data.IntMap.Lazy as M
import Data.Map.Syntax
import Data.Text as T
import Heist
import Heist.Compiled
import qualified HTMLEntities.Text as HTML
import Snap.Snaplet
import qualified Text.XmlHtml as X

import Application
import Piperka.API.Submit.Types

submitAPISplices
  :: Splice AppHandler
submitAPISplices = do
  nodes <- X.childElements <$> getParamNode
  let result = fromJust <$> (lift $ withTop' id $ view submitResult)
  msgs <- M.fromList <$>
    mapM (\node -> let i = read $ T.unpack $ fromJust $ X.getAttribute "id" node
                       spl = runNodeList $ X.childElements node
                   in (i,) <$> subSplice i spl result) nodes
  return $ yieldRuntime $ do
    i <- resultNumbering <$> result
    codeGen $ msgs M.! i

subSplice
  :: Int
  -> Splice AppHandler
  -> RuntimeSplice AppHandler SubmitResult
  -> Splice AppHandler
subSplice 1 s = withSplices s
  (mapV (pureSplice . textSplice) $ do
      "url" ## ("/info.html?cid=" <>) . fst
      "name" ## snd) . ((\(~(SValidation ~(ComicExists a))) -> a) <$>)
subSplice 2 s = withSplices s
  ("err" ## pureSplice . textSplice $ HTML.text . T.pack . show) .
  ((\(~(SValidation ~(Database a))) -> a) <$>)
subSplice 4 s = withSplices s
  ("err" ## pureSplice . textSplice $ HTML.text . T.pack . show) .
  ((\(~(EValidation ~(Database2 a))) -> a) <$>)
subSplice 9 s = withSplices s
  (mapV (pureSplice . textSplice) $ do
      "width" ## T.pack . show . fst
      "height" ## T.pack . show . snd) .
  ((\(~(BValidation (InvalidDimensions w h))) -> (w,h)) <$>)
subSplice 12 s = withSplices s
  ("url" ## pureSplice . textSplice $ ("/info.html?cid=" <>) . T.pack . show) .
  ((\(~(Success ~(EditSubmitted c))) -> c) <$>)
subSplice 13 s = withSplices s
  ("url" ## pureSplice . textSplice $ ("/info.html?cid=" <>) . T.pack . show) .
  ((\(~(Success ~(EditedSuccessfully c))) -> c) <$>)
subSplice _ s = const s
