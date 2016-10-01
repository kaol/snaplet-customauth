{-# LANGUAGE OverloadedStrings #-}

module Piperka.Listing.Header.Splices (listingHeaderSplices) where

import Control.Monad.Trans
import Data.Monoid
import Data.Map.Syntax
import Heist
import Heist.Compiled
import Heist.Compiled.Extra (checkedSplice, checkedAttrSplice)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import qualified Data.Text as T
import qualified HTMLEntities.Text as HTML
import qualified Text.XmlHtml as X
import Network.HTTP.Types.URI (encodePath)
import Data.Maybe
import Snap

import Piperka.Util
import Piperka.Listing.Types
import Piperka.Profile.Types
import Application (AppHandler)

commonHeaderSplices
  :: ListingMode
  -> Splices (RuntimeSplice AppHandler ListingParam -> Splice AppHandler)
commonHeaderSplices mode = do
  "hilightButton" ## const $ callTemplate "_hilightButton"
  "sortOptions" ## const $ do
    let linkSplice = do
          xs <- X.childNodes <$> getParamNode
          typ <- fromJust . X.getAttribute "type" <$> getParamNode
          let nod = X.Element "a" [("href", mkLink typ)] xs
          return $ yieldPure $ htmlNodeSplice id [nod]
    let splices = do
          "isProfile" ## return $ mempty
          "lnk" ## linkSplice
    withLocalSplices splices mempty $ callTemplate "_sortOptions"
  "qSearch" ## const $ do
    mo <- read . T.unpack . fromJust . X.getAttribute "mode" <$> getParamNode
    let sortSplice = case mo of
          Top -> return $ yieldPureText "top"
          _ -> return $ yieldRuntimeText $
               lift $ maybe "name" (HTML.text . decodeLatin1) <$>
               getQueryParam "sort"
    withLocalSplices ("sort" ## sortSplice) mempty runChildren
  where
    mkLink nam = uncurry encodePathToText $
                 (\(p, q) -> (p, ("sort", Just $ encodeUtf8 nam):q)) $
                 getListingPathQuery mode undefined

-- Profile splices
listingHeaderSplices
  :: ListingMode
  -> Splices (RuntimeSplice AppHandler ListingParam -> Splice AppHandler)
listingHeaderSplices Profile = commonHeaderSplices Profile <> profileSplices
listingHeaderSplices mode = commonHeaderSplices mode
--listingHeaderSplices Profile = {-commonHeaderSplices <>-} mapV (. fmap getProfile) $ do

--none = const $ return $ yieldRuntime mempty

profileSplices :: Splices (RuntimeSplice AppHandler ListingParam -> Splice AppHandler)
profileSplices = mapV (. fmap getProfile) $ do
  "profileName" ## pureSplice . textSplice $ HTML.text . name . profile
  "isPrivate" ## isPrivate
  "isMine" ## isMine
  "mayAllowFollow" ## mayAllowFollow
  "requesting" ## requesting
  "mayInterest" ## mayInterest
  "publicFollow" ## publicFollow
  "havePermission" ## checkedSplice (perm . profile)
  "yourOrProfileName" ## yourOrProfileName
  "youOrThisUser" ## youOrThisUser
  "grandTotal" ## pureSplice . textSplice $ T.pack . show . total . profile
  "nComics" ## pureSplice . textSplice $ T.pack . show . inComics . profile
  "mapLink" ## mapLink
  "writeUp" ## pureSplice . textSplice $ maybe "" id . writeup . profile
  "profileSortOptions" ## sortOptions
  where
    isPrivate =
      checkedSplice (\p -> case p of
                            Own _ -> (privacy $ profile p) == Private
                            _ -> False)
    isMine = mayDeferMap (\p -> return $ case p of Own x -> Just x
                                                   _ -> Nothing) $
             withSplices runChildren isMineSplices
    isMineSplices = mapV (pureSplice . textSplice) $ do
      "numFollowers" ## (\num -> (if num == 0 then "no" else T.pack $ show num) <>
                                 " " <> plural "follower" "followers" num) . followers
      "numFollowees" ## (\num -> if num == 0 then "nobody"
                                 else (T.pack $ show num) <>
                                      " " <> plural "user" "users" num) . followees
    mayAllowFollow =
      mayDeferMap (\p -> return $ case p of
                                   Other x -> case myPrivacy x of
                                               Friends -> Just x
                                               _ -> Nothing
                                   _ -> Nothing) $
      \n -> withLocalSplices mempty (checkedAttrSplice myPerm n) runChildren
    requesting =
      mayDeferMap (\p -> case p of Other x -> return $ if perm $ profile p
                                                       then Just x else Nothing
                                   _ -> return Nothing) $
      withSplices runChildren requestingSplices
    requestingSplices = do
      "viewerIsPrivate" ## checkedSplice titfortat
      "interest" ## checkedSplice (\p -> (not $ titfortat p) && interest p)
    mayInterest =
      checkedSplice $ \p -> case p of Common x -> not $ perm x
                                      Other x -> not $ perm $ profile x
                                      _ -> False
    publicFollow =
      mayDeferMap (\p -> return $ case p of
                                   Other x -> if (privacy $ profile p) == Public
                                              then Just x else Nothing
                                   _ -> Nothing) $
      \n -> withLocalSplices mempty (checkedAttrSplice interest n) runChildren
    yourOrProfileName n = return $ yieldRuntimeText $ do
      p <- n
      return $ case p of Own _ -> "your comics"
                         _ -> name $ profile p
    youOrThisUser n = return $ yieldRuntimeText $ do
      p <- n
      return $ case p of Own _ -> "you"
                         _ -> "this user"
    mapLink = bindLater $ \p ->
      return $ encodePath ["/map/"]
        [("profile", Just $ encodeUtf8 $ name $ profile p)]
    sortOptions n = do
      let linkSplice = do
            typ <- fromJust . X.getAttribute "type" <$> getParamNode
            xs <- X.childNodes <$> getParamNode
            return $ yieldRuntime $ do
              nam <- name . profile <$> n
              let link = encodePathToText ["profile.html"]
                         [ ("name", Just $ encodeUtf8 nam)
                         , ("sort", Just $ encodeUtf8 typ)]
              let nod = X.Element "a" [("href", link)] xs
              return $ htmlNodeSplice id [nod]
      let splices = do
            "isProfile" ## runChildren
            "lnk" ## linkSplice
      withLocalSplices splices mempty $ callTemplate "_sortOptions"
