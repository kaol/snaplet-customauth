{-# LANGUAGE OverloadedStrings #-}

module Piperka.Listing.Render (renderListing) where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B (unpack)
import Data.ByteString.Read (int)
import Data.Maybe
import Data.Text (unpack)
import Heist
import Heist.Compiled as C
import Heist.Compiled.Extra as C
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


renderListing
  :: RuntimeSplice AppHandler UserPrefs
  -> C.Splice AppHandler
renderListing runtime = do
  mode <- read . unpack . fromJust . getAttribute "mode" <$> getParamNode

  let success n = do
        tpl <- C.withSplices (C.callTemplate "_listing")
               (listingColumnSplices mode) (fst <$> n)
        let n' = snd <$> n
        nTpl1 <- deferMany (C.withSplices (C.callTemplate "_navigate")
                            (navigateSplices False)) n'
        nTpl2 <- deferMany (C.withSplices (C.callTemplate "_navigate")
                            (navigateSplices True)) n'
        hd <- withSplices runChildren (listingHeaderSplices mode) $
              ((\(_, _, x) -> x) . fst <$> n)
        let lst = nTpl1 <> tpl <> nTpl2
        return $ yieldRuntime $ do
          lst' <- case mode of
                   Profile -> do
                     havePerm <- perm . profile . getProfile .
                                 (\(_, _, x) -> x) . fst <$> n
                     return $ if havePerm then lst else mempty
                   _ -> return lst
          useMinimal <- lift $ isJust <$> getParam "minimal"
          codeGen $ if useMinimal then lst' else hd <> lst'

      failure action = do
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

      getListingData prefs = do
        (ord, paramOrd) <- case mode of
          Top -> return (L.TopDesc, Nothing)
          Graveyard -> return (L.TitleAsc, Nothing)
          -- Update ord will get overridden by user setting from DB.
          Update -> return (L.TitleAsc, Nothing)
          _ -> do
            paramOrd <- lift $ fmap (parseOrdering . B.unpack) <$> getParam "sort"
            return $ maybe (L.TitleAsc, Nothing) ((,) <$> id <*> Just) paramOrd
        offset <- lift $ (fromIntegral . maybe 0 ((maybe 0 fst) . int))
                  <$> getParam "offset"
        let limit = (rows prefs) * (columnsToInt $ columns prefs)
        lst <- getListing mode ord offset limit (((,) <$> uid <*> uname)
                                                 <$> user prefs)
        let makeResult param =
              let pathQuery = maybe id addSort paramOrd $ getListingPathQuery mode param
                  tot = extractTotal param
                  navParams = if tot <= limit
                              then Nothing
                              else Just (pathQuery, fromIntegral offset,
                                         fromIntegral limit, fromIntegral tot)
              in ((prefs, fromIntegral offset, param), navParams)
        return $ fmap makeResult lst

  C.eitherDeferMap getListingData failure success runtime
