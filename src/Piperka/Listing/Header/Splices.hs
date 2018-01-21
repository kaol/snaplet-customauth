{-# LANGUAGE OverloadedStrings #-}

module Piperka.Listing.Header.Splices
  ( listingHeaderSplices
  , listingHeaderAttrSplices
  ) where

import Control.Monad.Trans
import Data.Maybe
import Data.Monoid
import Data.Map.Syntax
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Vector (Vector)
import Heist
import Heist.Compiled
import Heist.Compiled.Extra (checkedSplice, checkedAttrSplice, eitherDeferMap)
import qualified HTMLEntities.Text as HTML
import Network.HTTP.Types.URI (encodePath)
import Snap
import qualified Text.XmlHtml as X

import Application
import Piperka.Error.Splices
import Piperka.Listing.Header.Query
import Piperka.Listing.Types
import Piperka.Profile.Types
import Piperka.Util

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
    let sortSplice = case mode of
          Top -> return $ yieldPureText "top"
          _ -> return $ yieldRuntimeText $
               lift $ maybe "name" (HTML.text . decodeLatin1) <$>
               getQueryParam "sort"
    withLocalSplices ("sort" ## sortSplice) mempty $ callTemplate "_qSearch"
  "alphabetIndex" ## eitherDeferMap (const $ lift alphabetIndex) stdSqlErrorSplice
    (withSplices (callTemplate "_alphabetIndex") alphabetIndexSplices)
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

alphabetIndexSplices
  :: Splices (RuntimeAppHandler (Vector (Text, Int)))
alphabetIndexSplices = "jumpTo" ## manyWith runChildren
  ("letter" ## pureSplice . textSplice $ fst)
  ("href" ## \n _ -> n >>= \n' -> return
    [("href", "browse.html?offset=" <> (T.pack $ show $ snd n'))])

listingHeaderAttrSplices
  :: Splices (AttrSplice AppHandler)
listingHeaderAttrSplices = "guardName" ## const $ do
  mode <- lift $ getParam "sort"
  return $ if maybe True (== "name") mode then [] else [("class", "script")]
