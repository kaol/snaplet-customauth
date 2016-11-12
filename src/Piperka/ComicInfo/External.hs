{-# LANGUAGE OverloadedStrings #-}

-- Read and parse external entries from XML at splice compile time

module Piperka.ComicInfo.External (generateExternal) where

import Control.Error.Util (hush)
import Control.Monad
import qualified Data.ByteString as B
import Data.List
import Data.Text (Text)
import Data.Text.Read
import qualified Text.XmlHtml as X
import Text.XmlHtml (Node)

import Piperka.ComicInfo.Types

generateExternal :: IO (Either String (Int -> Text -> Maybe ExternalEntry))
generateExternal = do
  doc <- X.parseXML "x/epedias.xml" <$> B.readFile "x/epedias.xml"
  return $ process =<< doc

process
  :: X.Document
  -> Either String (Int -> Text -> Maybe ExternalEntry)
process doc = maybe (Left "failure parsing epedias data")
              (return . mkLookup) $ do
  epedias <- find (\x -> X.isElement x && X.elementTag x == "epedias") $
             X.docContent doc
  sequence $ map mkEntry $ X.childElements epedias

mkEntry :: Node -> Maybe (Int, Text -> ExternalEntry)
mkEntry x = do
  guard $ X.isElement x && X.elementTag x == "entry"
  i <- fst <$> (join $ hush . decimal <$> (lookup "id" $ X.elementAttrs x))
  t <- lookup "tag" $ X.elementAttrs x
  n <- lookup "name" =<< X.elementAttrs <$>
       (find ((== "link") . X.elementTag) $ X.childElements x)
  d <- X.nodeText <$> (find (\y -> X.isElement y && X.elementTag y == "description") $
                       X.childNodes x)
  b <- lookup "href" =<< X.elementAttrs <$>
       (find ((== "urlbase") . X.elementTag) $ X.childElements x)
  return $ (i, ExternalEntry d t n i b)

mkLookup :: (Eq a, MonadPlus m) => [(a, b -> c)] -> a -> b -> m c
mkLookup el = maybe (const mzero) (return .) . flip lookup el
