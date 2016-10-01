{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Piperka.Listing.Query (
  getListing
  ) where

import Application hiding (uid, uname)
import Piperka.Listing
import Piperka.Listing.Types
import qualified Piperka.Listing.Types.Ordering as L (Ordering(..))
import qualified Piperka.Profile.Types as PT
import Piperka.Update.Types (UpdateOptions)
import Piperka.Update.Statements
import Piperka.Profile.Statements
import Piperka.Listing.Statements
import Piperka.Update.Types (bookmarkSort)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Int
import Data.Maybe
import Data.Text (Text, toCaseFold)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V
import Hasql.Session hiding (run)
import Heist
import Snap
import Snap.Snaplet.Hasql

import Prelude hiding (Ordering)

getListing
  :: ListingMode
  -> L.Ordering
  -> Int32
  -> Int32
  -> Maybe (Int32, Text)
  -> RuntimeSplice (Handler App App) (Either ListingError ListingParam)
getListing Browse ord offset limit ux =
  getListing' False ord offset limit $ fmap fst ux
getListing Top ord offset limit ux =
  getListing' True ord offset limit $ fmap fst ux

getListing Profile ord offset limit ux = do
  let uid = fmap fst ux
  let uname = fromJust $ fmap snd ux
  name <- (fmap . fmap) decodeUtf8 $ lift $ getParam "name"
  runExceptT $ do
    prof <- ExceptT . fmap
            (either
             (Left . SqlError)
             (maybe (Left Missing) Right)) . runExceptT $
               case (name, uid) of
                 (Nothing, Nothing) -> return Nothing
                 (Nothing, Just uid') -> ownData uid'
                 (Just name', Nothing) -> otherData PT.Common $
                                          query name' profileDataFetch
                 (Just name', Just uid') ->
                   if ((\f a b -> f a == f b) toCaseFold name' uname)
                   then ownData uid'
                   else otherData PT.Other $
                        query (name', uid') profileOtherDataFetch
    ExceptT $ return $ case prof of
      PT.Other p -> checkSecretProfile p
      PT.Common p -> checkSecretProfile p
      _ -> Right ()
    let pUid = PT.uid $ PT.profile prof
    if PT.perm $ PT.profile prof
      then ExceptT . fmap (either (Left . SqlError) Right) $
           lift $ withTop db $ run $ maybe
           (fmap (ProfileParam (commonToPlain prof)) $ query
            (pUid, limit, offset) (profileFetch ord))
           (\uid' -> fmap (UserProfileParam prof) $ query
                     (pUid, uid', limit, offset)
                     (profileFetchSubscribed ord))
           uid
      else ExceptT $ return $ Right $ maybe
           (ProfileParam (commonToPlain prof) V.empty)
           (const $ UserProfileParam prof V.empty) uid
  where
    ownData uid = fmap (Just . PT.Own) $ ExceptT $ lift $ withTop db $
                  run $ query uid profileOwnDataFetch
    otherData f = (fmap . fmap) f . ExceptT . lift . withTop db . run
    commonToPlain (PT.Common x) = x
    commonToPlain _ = undefined
    checkSecretProfile p = let p' = PT.profile p in
      if PT.privacy p' /= PT.Private then Right () else Left Missing


-- Update mode gets its ordering always from user settings.
getListing Update _ offset limit ux =
  flip (maybe $ return $ Left Missing) ux $ \(uid', _) -> lift $ withTop db $ do
    res :: Either Error ListingParam <- runExceptT $ do
      updateOptions :: UpdateOptions <- ExceptT $ run $ query uid' updateOptionsFetch
      let ord = bookmarkSort updateOptions
      fmap (UpdateParam updateOptions) $
        ExceptT $ run $ query (uid', limit, offset) $ updatesFetch ord
    return $ either (Left . SqlError) Right res

getListing Graveyard _ offset limit _ =
  lift $ withTop db $ do
    res :: Either Error ListingParam <- runExceptT $ do
      total :: Int <- fmap fromIntegral $ ExceptT $ run $ query () graveyardTotalFetch
      fmap (GraveyardParam total) $ ExceptT $ run $ query (limit, offset) graveyardFetch
    return $ either (Left . SqlError) Right res

-- Helper function for common case of Top/Browse
getListing' ::
  Bool
  -> L.Ordering
  -> Int32
  -> Int32
  -> Maybe Int32
  -> RuntimeSplice (Handler App App) (Either ListingError ListingParam)
getListing' isTop ord offset limit uid = lift $ withTop db $ do
  res <- runExceptT $ do
    total :: Int <- ExceptT $ (fmap . fmap) fromIntegral $ run $ query () comicsTotalFetch
    maybe
      (fmap (ListingParam isTop total) $ ExceptT $ run $ query (limit, offset)
       (comicsFetch ord))
      (\uid' -> fmap (UserParam isTop total) $ ExceptT $ withTop db $ run $
                query (uid', limit, offset)
                (comicsFetchSubscribed ord)) uid
  return $ either (Left . SqlError) Right res

