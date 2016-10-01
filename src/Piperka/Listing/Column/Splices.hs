{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Piperka.Listing.Column.Splices (listingColumnSplices) where

import Data.Maybe
import Data.Text (pack, unpack)
import qualified Data.Vector as V
import qualified HTMLEntities.Text as HTML
import Heist
import Heist.Compiled as C
import Heist.Compiled.LowLevel as C
import Heist.Compiled.Extra as C
import Text.XmlHtml
import Data.Map.Syntax

import Application
import Piperka.Listing.Types hiding (listing', listing'')
import Piperka.Update.Types

listingColumnSplices
  :: ListingMode
  -> Splices (RuntimeSplice AppHandler
              (UserPrefs, Int, ListingParam)
              -> C.Splice AppHandler)
listingColumnSplices mode = do
  "columnMode" ## renderMode mode

renderMode
  :: ListingMode
  -> (RuntimeSplice AppHandler
      (UserPrefs, Int, ListingParam))
  -> C.Splice AppHandler
renderMode mode runtime = do
  col <- read . unpack . fromJust . getAttribute "col" <$> getParamNode
  tplL <- mkColumnSplices paramsL
  tplU <- mkColumnSplices paramsU
  tplP <- mkColumnSplices paramsP
  return $ C.yieldRuntime $ do
    (prefs, offset, param) <- runtime
    let userCols = columns prefs
    let nrows = fromIntegral $ rows prefs
    if (col /= userCols)
      then return mempty
      else do
      let splitFunc =
            zipWith (\c (a,b) -> (a,(b,c)))
            [(1+offset),(1+offset+nrows)..] .
            takeWhile (not . V.null . snd) .
            case userCols of
             OneColumn -> (:[]) . (1,)
             TwoColumn -> \v -> let (v1, v2) = V.splitAt nrows v
                                in [(1,v1), (2,v2)]
             ThreeColumn -> \v -> let (v1, v1') = V.splitAt nrows v
                                      (v2, v3) = V.splitAt nrows v1'
                                  in [(1,v1), (2,v2), (3,v3)]
      tpl <- mconcat [ tplL param splitFunc
                     , tplU param splitFunc
                     , tplP param splitFunc
                     ]
      C.codeGen tpl
  where
    paramsL = (ListingMode, listingItemSplices, extractListing)
    paramsU = (UserMode, userListingItemSplices, extractUserListing)
    paramsP = (UpdateMode, updateListingItemSplices, extractUpdateListing)
    mkColumnSplices (itemMode, itemSplices, extract) = do
      promise <- C.newEmptyPromise
      tpl <- C.deferMap (return . fst)
             (let withUpdateSplices spl =
                    C.deferMap (return . snd) (C.withSplices spl updateListingSplices)
                    (C.getPromise promise) in
              (case itemMode of UpdateMode -> withUpdateSplices
                                _ -> id) . (C.withSplices C.runChildren
                                            (columnSplices mode itemMode itemSplices)))
             (C.getPromise promise)
      return $ \param f -> maybe (return mempty)
                           (\lst -> C.putPromise promise (f lst, param) >> return tpl) $
                           extract param

columnSplices
  :: ListingMode
  -> ListingItemMode
  -> Splices (RuntimeSplice AppHandler a -> Splice AppHandler)
  -> Splices (RuntimeSplice AppHandler [(Int, (V.Vector a, Int))] -> Splice AppHandler)
columnSplices mode itemMode itemSplices = "column" ## \runtime -> do
  colNum <- read . unpack . fromJust . getAttribute "column" <$>
            getParamNode
  mayDeferMap (return . lookup colNum)
    (\runtime' ->
      (case mode of
        Top -> \spl -> C.withSplices spl
                       ("startNum" ## C.pureSplice . C.textSplice $ pack . show . snd)
                       runtime'
        _ -> id) $
      C.withSplices (C.callTemplate "_column")
      (singleColumnSplices mode itemMode itemSplices) $
      fmap fst runtime') runtime

updateListingSplices
  :: Splices (RuntimeSplice AppHandler ListingParam -> Splice AppHandler)
updateListingSplices = do
  "holdbookmark" ## C.checkedSplice (holdBookmark . getUpdateParam)
  "offsetBackParam" ## \runtime -> return $ yieldRuntimeText $ do
    offset <- offsetMode . getUpdateParam <$> runtime
    return $ if offset then "&offset_back=1" else ""


singleColumnSplices
  :: ListingMode
  -> ListingItemMode
  -> Splices (RuntimeSplice AppHandler a -> Splice AppHandler)
  -> Splices (RuntimeSplice AppHandler (V.Vector a) -> Splice AppHandler)
singleColumnSplices mode itemMode itemSplices = do
  "listingMode" ## renderSingleColumn mode
  "item" ## renderItem itemMode itemSplices
  "listingStdItem" ## const $ C.callTemplate "_listingStdItem"

renderSingleColumn
  :: ListingMode
  -> RuntimeSplice AppHandler (V.Vector a)
  -> Splice AppHandler
renderSingleColumn mode _ = do
  modes :: [ListingMode] <- read . unpack . fromJust . getAttribute "type"
                            <$> getParamNode
  if mode `elem` modes
    then C.runChildren
    else return $ C.yieldPure mempty

renderItem
  :: ListingItemMode
  -> Splices (RuntimeSplice AppHandler a -> Splice AppHandler)
  -> RuntimeSplice AppHandler (V.Vector a)
  -> Splice AppHandler
renderItem itemMode itemSplices runtime = do
  allowedMode :: ListingItemMode <- read . unpack . fromJust .
                                    getAttribute "type" <$> getParamNode
  if (itemMode /= allowedMode)
    then return $ C.yieldPure mempty
    else C.manyWithSplices C.runChildren itemSplices runtime

listingItemSplices
  :: Splices (RuntimeSplice AppHandler ListingItem -> Splice AppHandler)
listingItemSplices = mapV (C.pureSplice . C.textSplice) $ do
  "title" ## HTML.text . title
  "cid" ## pack . show . cid

userListingItemSplices
  :: Splices (RuntimeSplice AppHandler UserListingItem -> Splice AppHandler)
userListingItemSplices = do
  "title" ## C.pureSplice . C.textSplice $ HTML.text . title . listing
  "cid" ## C.pureSplice . C.textSplice $ pack . show . cid . listing
  "followee" ## C.checkedSplice friends
  "subscribed" ## C.checkedSplice subs

updateListingItemSplices
  :: Splices (RuntimeSplice AppHandler UpdateListingItem -> Splice AppHandler)
updateListingItemSplices = mapV (C.pureSplice . C.textSplice) $ do
  "title" ## HTML.text . title . listing
  "cid" ## pack . show . cid . listing
  "new" ## pack . show . new
