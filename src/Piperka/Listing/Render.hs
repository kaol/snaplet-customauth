{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Piperka.Listing.Render (renderListing) where

import Control.Error.Util (note)
import Control.Lens (view)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B (unpack, readInt)
import Data.Map.Syntax
import Data.Maybe
import Data.Text (unpack, Text)
import Heist
import Heist.Compiled as C
import Heist.Compiled.Extra as C
import Network.HTTP.Types.URI (Query)
import Text.XmlHtml
import Data.Monoid
import Snap
import Data.Text.Encoding (encodeUtf8)

import Application hiding (prefs)
import Piperka.Error.Splices
import Piperka.Listing.Types hiding (listing', listing'')
import qualified Piperka.Listing.Types.Ordering as L (Ordering(..), orderingToText)
import Piperka.Listing.Query (getListing)
import Piperka.Listing.Statements (parseOrdering)
import Piperka.Listing.Navigate.Splices
import Piperka.Listing.Header.Splices
import Piperka.Listing.Column.Splices
import Piperka.Profile.Types (profile, perm)


success
  :: ListingMode
  -> RuntimeAppHandler ((UserPrefs, Int, ListingParam),
                        (Bool, Maybe (([Text], Query), Int, Int, Int))
                       )
success mode n = do
  onlyLst <- C.withSplices (C.callTemplate "_listing")
             (listingColumnSplices mode) (fst <$> n)
  lst <- C.eitherDeferMap (return . note () . snd . snd)
         (const $ return onlyLst)
         (C.withSplices (C.callTemplate "_navigate")
          (("apply-content" ## const $ return onlyLst) <>
           (navigateSplices $ listingModeSubscribes mode))) n
  hd <- withLocalSplices
        (mapV ($ (((\(_, _, x) -> x) . fst <$> n))) $
         listingHeaderSplices mode)
        listingHeaderAttrSplices runChildren
  let content = return $ yieldRuntime $ do
        (_, (useMinimal, _)) <- n
        lst' <- case mode of
                 Profile -> do
                   havePerm <- perm . profile . getProfile .
                               (\(_, _, x) -> x) . fst <$> n
                   return $ if havePerm then lst else mempty
                 _ -> return lst
        codeGen $ if useMinimal then lst' else hd <> lst'
  withLocalSplices ("apply-content" ## content)
    (offsetHref $ (fmap (\(a, b, _, _) -> (a, b))) . snd . snd <$> n)
    (callTemplate "_listingForm")

renderListing
  :: RuntimeAppHandler (Maybe MyData)
renderListing runtime = do
  mode <- read . unpack . fromJust . getAttribute "mode" <$> getParamNode

  let failure action = do
        missing <- runMaybeT
                   (MaybeT (return $ case mode of
                             Update -> Just "_updateMissing"
                             Profile -> Just "_profileMissing"
                             _ -> Nothing)
                    >>= lift . C.callTemplate)
        sqlErr <- C.deferMap (\(SqlError err) -> return err)
                  (C.withSplices (C.callTemplate "_sqlErr") sqlErrorSplices) action
        C.bindLater (\val -> case val of
                      Missing -> C.codeGen $ fromJust missing
                      SqlError _ -> C.codeGen sqlErr) action

      addSort ord (p, q) = (p, q ++ [("sort", Just $ encodeUtf8 $ L.orderingToText ord)])

      getListingData usr = do
        let prefs = getPrefs usr
        (ord, paramOrd) <- case mode of
          Top -> return (L.TopDesc, Nothing)
          Graveyard -> return (L.TitleAsc, Nothing)
          -- Update ord will get overridden by user setting from DB.
          Update -> return (L.TitleAsc, Nothing)
          _ -> do
            paramOrd <- lift $ fmap (parseOrdering . B.unpack) <$> getParam "sort"
            return $ maybe (L.TitleAsc, Nothing) ((,) <$> id <*> Just) paramOrd
        useMinimal <- lift $ withTop' id $ view minimal
        offset <- lift $ (fromIntegral . maybe 0 ((maybe 0 fst) . B.readInt))
                  <$> getParam "offset"
        let limit = (rows prefs) * (columnsToInt $ columns prefs)
        lst <- getListing mode ord offset limit (((,) <$> uid <*> uname)
                                                 <$> usr)
        let makeResult param =
              let pathQuery = maybe id addSort paramOrd $ getListingPathQuery mode param
                  tot = extractTotal param
                  navParams = (useMinimal,) $
                              if useMinimal || tot <= limit
                              then Nothing
                              else Just (pathQuery, fromIntegral offset,
                                         fromIntegral limit, fromIntegral tot)
              in ((prefs, fromIntegral offset, param), navParams)
        return $ fmap makeResult lst

  C.eitherDeferMap getListingData failure (success mode) runtime
