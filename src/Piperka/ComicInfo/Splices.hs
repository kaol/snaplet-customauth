{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Piperka.ComicInfo.Splices
  (
    renderComicInfo
  , renderWithCid
  )
  where

import Control.Error.Util
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Lens (view)
import Data.Maybe
import Data.Monoid
import Data.Map.Syntax
import Heist
import Heist.Compiled
import Heist.Compiled.Extra
import qualified HTMLEntities.Text as HTML
import Snap
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Text.XmlHtml as X

import Application
import Piperka.ComicInfo.Query
import Piperka.ComicInfo.Types
import Piperka.Error.Splices
import Piperka.Util (formatTime', getCid)

-- When used as a top level splice
renderComicInfo
  :: RuntimeAppHandler (Maybe MyData)
renderComicInfo n = do
  let success n' = withSplices runChildren comicInfoSplices n'

  let failure n' = do
        sqlErr <- withSplices (callTemplate "_sqlErr") sqlErrorSplices $
                  (\(SqlError e) -> e) <$> n'
        missing <- callTemplate "_cidMissing"
        let errs Missing = lift (modifyResponse $ setResponseCode 404) >> codeGen missing
            errs (SqlError _) = codeGen sqlErr
            errs FoundDead = lift (modifyResponse $ setResponseCode 404) >> codeGen missing
        bindLater errs n'

  render False success failure n

renderWithCid
  :: RuntimeSplice AppHandler (Maybe MyData)
  -> Splice AppHandler
renderWithCid n = do
  deadPage <- maybe False (read . T.unpack) . X.getAttribute "dead" <$>
              getParamNode

  let success n' = withSplices runChildren
                   ((emptySplices' ["dead", "missing"]) <>
                    ("exists" ## renderExists deadPage)) n'

  let failure n' = do
        sqlErr <- withSplices (callTemplate "_sqlErr") sqlErrorSplices $
                  (\(SqlError e) -> e) <$> n'
        d <- withLocalSplices
             (emptySplices ["exists", "missing"] <>
              ("dead" ## runChildren)) mempty runChildren
        missing <- withLocalSplices
                   (emptySplices ["exists", "dead"] <>
                    ("missing" ## runChildren)) mempty runChildren
        let errs Missing = lift (modifyResponse $ setResponseCode 404) >> codeGen missing
            errs (SqlError _) = codeGen sqlErr
            errs FoundDead = codeGen d
        bindLater errs n'

  render deadPage success failure n

render
  :: Bool
  -> RuntimeAppHandler ComicInfo
  -> RuntimeAppHandler ComicInfoError
  -> RuntimeAppHandler (Maybe MyData)
render deadPage success failure n = do
  let getCid' p = fmap (\(a,b) -> (a,(b,p))) <$> lift getCid
  let getData b = do
        tlookup <- lift $ view taglookup
        elookup <- lift $ view extlookup
        let getInfo = getComicInfo tlookup elookup
            getDeadInfo = getDeadComicInfo tlookup

        runExceptT $ do
          (c, p) <- snd <$> (hoistEither $ note Missing b)
          info <- ExceptT $ (if deadPage then getDeadInfo else getInfo) c p
          return info

  deferMap getCid'
    (\n' -> withSplices (eitherDeferMap getData failure success $ n')
            ("cid" ## pureSplice . textSplice $
             maybe "" (T.decodeLatin1 . fst)) n') n

renderExists :: Bool -> RuntimeAppHandler ComicInfo
renderExists deadPage = withSplices runChildren $ do
  "thisPage" ## pureSplice . textSplice $
    ((if deadPage then "deadinfo.html?cid=" else "info.html?cid=") <>) .
    T.pack . show . cid
  "related" ## const $ return mempty -- TODO
  "comicInfo" ## \n -> withSplices (callTemplate "/include/cinfo")
                       comicInfoSplices n
  "ifSubscribed" ## \n -> do
    nodes <- X.elementChildren <$> getParamNode
    let nodeFilter name = runNodeList $ concatMap X.childNodes $
                          filter (maybe False (== name) . X.tagName) nodes
    t <- nodeFilter "true"
    f <- nodeFilter "false"
    flip bindLater n $ \info -> do
      codeGen $ if fromMaybe False $ subscribed info then f else t
  "crawlErrors" ## renderCrawlErrors
  "ifMapped" ## \n -> do
    x <- runChildren
    flip bindLater n $ \n' -> do
      if mapped n' then codeGen x else return mempty

renderCrawlErrors
  :: RuntimeAppHandler ComicInfo
renderCrawlErrors n = do
  let rowSplices = mapV (pureSplice . textSplice) $ do
        "ord" ## T.pack . show . ord
        "time" ## T.pack . formatTime' . time
        "url" ## archiveUrl
        "code" ## T.pack . show . code
        "msg" ## msg
  let renderRows = manyWithSplices runChildren rowSplices $ crawlErrors <$> n
  inner <- withLocalSplices ("rows" ## renderRows) mempty runChildren
  flip bindLater n $ \info ->
    if V.null $ crawlErrors info then return mempty else codeGen inner

comicInfoSplices
  :: Splices (RuntimeAppHandler ComicInfo)
comicInfoSplices = do
  "comicInfo" ## const $ runChildren
  "banner" ## renderBanner
  "title" ## pureSplice . textSplice $ title
  "readersLink" ## renderReadersLink
  "subscriptions" ## pureSplice . textSplice $ T.pack . show . readers
  "hasFragments" ## renderHasFragments
  "fragmentCount" ## pureSplice . textSplice $ T.pack . show . fragmentCount
  "pageCount" ## pureSplice . textSplice $ T.pack . show . pageCount
  "ifKnowPages" ## renderIfKnowPages
  "homepage" ## pureSplice . textSplice $ homepage  -- TODO: Escape?
  "homepageText" ## pureSplice . textSplice $ HTML.text . homepage
  "ifExternLinks" ## renderIfExternLinks
  "ifAddDate" ## renderIfAddDate
  "categories" ## renderCategories
  where
    renderBanner n = do
      x <- withSplices runChildren
           ("bannerUrl" ## pureSplice . textSplice $ id) $
           (fromJust . banner) <$> n
      flip bindLater n $ \info ->
        if (isJust $ banner info) then codeGen x else return mempty
    renderReadersLink n = do
      node <- getParamNode
      noLink <- runChildren
      withLink <- runNode $ X.Element "a"
                  [("href", "readers.html?cid=${h:cid}")] $
                  X.elementChildren node
      flip bindLater n $ \info ->
        codeGen $ if publicReaders info then withLink else noLink
    renderHasFragments n = do
      x <- runChildren
      flip bindLater n $ \info ->
        if fragmentCount info > 0 then codeGen x else return mempty
    renderIfKnowPages n = do
      nodes <- X.elementChildren <$> getParamNode
      let archivePagesSplices = do
            "firstPageUrl" ## pureSplice . textSplice $ \(x, _, _) -> x
            "lastPageUrl" ## pureSplice . textSplice $ \(_, x, _) -> x
            "ifFixedHead" ## \n' -> do
              c <- runChildren
              flip bindLater n' $ \(_, _, x) ->
                if x then codeGen c else return mempty
      let nodeFilter name = runNodeList $
                            concatMap X.childNodes $
                            filter (maybe False (== name) . X.tagName) nodes
      t <- withSplices (nodeFilter "true") archivePagesSplices $
           fromJust . archivePages <$> n
      f <- nodeFilter "false"
      flip bindLater n $ \info ->
        codeGen $ if (isJust $ archivePages info) then t else f
    renderIfExternLinks n = do
      c <- withSplices runChildren ("externLink" ## renderExternLink) $
           extern <$> n
      flip bindLater n $ \info ->
        if null $ extern info then return mempty else codeGen c
    renderExternLink :: RuntimeSplice AppHandler [ExternalEntry] -> Splice AppHandler
    renderExternLink = manyWithSplices runChildren $
                       mapV (pureSplice . textSplice) $ do
                         "url" ## \n -> (base n <> urlPart n)
                         "description" ## description
                         "siteName" ## epediaTagName
    renderIfAddDate n =
      manyWithSplices runChildren
      ("addDate" ## pureSplice . textSplice $ T.pack . formatTime') $
      addedOn <$> n
    renderCategories n =
      let splices = do
            "name" ## pureSplice . textSplice $ tagName . snd
          attrSplices = mapV (\f x _ -> f <$> x) $ do
            "description" ##
              maybe [] (\d -> [(T.pack "title", d)]) . tagDescription . snd
            "class" ## bool [(T.pack "class", "odd")] [] . fst
      in manyWith runChildren splices attrSplices $
         (zip (cycle [False, True]) . tags) <$> n
