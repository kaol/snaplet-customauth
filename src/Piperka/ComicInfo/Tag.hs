{-# LANGUAGE OverloadedStrings #-}

module Piperka.ComicInfo.Tag where

import Control.Error.Util (hush)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.IntSet as S
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Read
import System.Exit
import System.Process hiding (readCreateProcessWithExitCode)
import System.Process.ByteString
import qualified Text.XmlHtml as X
import Text.XmlHtml (Node)

import Piperka.ComicInfo.Types

-- The function outputs tags in the canonical order as defined by the
-- XML file.
generateTag :: IO (Either String ([Int] -> [ComicTag]))
generateTag = do
  doc <- X.parseXML "x/tags.xml" <$> B.readFile "x/tags.xml"
  return $ process =<< doc
  where
    process doc = maybe (Left "failure parsing tags data")
                  (return . mkLookup) $ do
      ts <- find (\x -> X.isElement x && X.elementTag x == "tags") $
            X.docContent doc
      return $ concatMap (readCategories []) $ X.childElements ts

readCategories
  :: [Text]
  -> Node
  -> [ComicTag]
readCategories path x =
  (readCat name path x) <>
  (concatMap (readCategories (name:path)) $
   filter ((== "category") . X.elementTag) $ X.childElements x)
  where
    name = fromJust $ lookup "name" $ X.elementAttrs x

readCat :: Text -> [Text] -> Node -> [ComicTag]
readCat name path x = maybeToList $ do
  i <- fst <$> (join $ hush . decimal <$> (lookup "id" $ X.elementAttrs x))
  let dNode = find (\y -> X.isElement y && X.elementTag y == "description") $
              X.childElements x
  let d = X.nodeText <$> dNode
  let n = T.intercalate ":" $ reverse $ name:path
  return $ ComicTag i n d

mkLookup
  :: [ComicTag]
  -> [Int]
  -> [ComicTag]
mkLookup ts used = filter match ts
  where
    match = flip S.member (S.fromList used) . tagid

generateTagFormPart
  :: IO (Either String Text)
generateTagFormPart = do
  let cfg = System.Process.shell "xsltproc x/tags.xslt x/tags.xml | \
                                 \xpath -e '//body/*/' /dev/stdin 2>/dev/null"
  status <- readCreateProcessWithExitCode cfg ""
  case status of
    (ExitSuccess, out, _) -> return $ either (Left . show) Right $ decodeUtf8' out
    (ExitFailure i, _, err) -> return $ Left $ ((show i) ++ " " ++ (show err))
