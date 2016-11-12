{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Piperka.Splices
  (
    piperkaSplices
  )
  where

import Heist
import Heist.Compiled as C
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Snap
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Snap.Snaplet.CustomAuth
import Backend()
import qualified Text.XmlHtml as X
import Data.Map.Syntax
import qualified Heist.Compiled.Extra as C

import Data.Maybe
import Data.UUID
import qualified HTMLEntities.Text as HTML

import Application
import Piperka.Splices.Flavor
import Piperka.Listing.Render
import Piperka.Listing.Types (ViewColumns(..))
import Piperka.ComicInfo.Splices
import Piperka.Messages
import Piperka.Util

defaultUserPrefsWithStats :: UserPrefsWithStats
defaultUserPrefsWithStats = UserPrefsWithStats
  { newComics = 0
  , unreadCount = (0,0)
  , prefs = defaultUserPrefs
  }

defaultUserPrefs :: UserPrefs
defaultUserPrefs = UserPrefs
  { user = Nothing
  , rows = 40
  , columns = TwoColumn
  , newExternWindows = False
  }

loginFailed :: forall b b1 v. b -> Handler App v b1
loginFailed = const $ do
  saveMessage "login failed"
  redirect' "/" 303

piperkaSplices
  :: Splices (C.Splice AppHandler)
piperkaSplices = do
  "piperka" ## renderPiperka
-- Splice definition overridden if used via withCid.
  "comicInfo" ## renderMinimal renderComicInfo
  <> messagesSplices

renderMinimal
  :: RuntimeAppHandler UserPrefs
  -> C.Splice AppHandler
renderMinimal action =
  (\n -> withSplices (action n) (contentSplices []) n) `C.defer`
  ((fromMaybe defaultUserPrefs) <$>
   (lift $ withTop apiAuth $ combinedLoginRecover loginFailed))

renderPiperka
  :: C.Splice AppHandler
renderPiperka = do
  xs <- X.childNodes <$> getParamNode
  let prefsWithStats = lift $ withTop auth $ combinedLoginRecover loginFailed
  let getInner = C.withSplices (C.callTemplate "_base") (contentSplices xs)
  let getInner' = C.withSplices (C.runNodeList xs) (contentSplices [])
  normal <- (\n -> do
                let inner = getInner $ prefs <$> n
                C.withSplices inner statsSplices n) `C.defer`
            ((fromMaybe defaultUserPrefsWithStats) <$> prefsWithStats)
  bare <- renderMinimal getInner'
  return $ yieldRuntime $ do
    useMinimal <- lift $ isJust <$> getParam "minimal"
    codeGen $ if useMinimal then bare else normal

statsSplices
  :: Splices (RuntimeAppHandler UserPrefsWithStats)
statsSplices = do
  "unreadStats" ## \runtime -> return $ C.yieldRuntimeText $ do
    p <- runtime
    return $ case unreadCount p of
     (0,_) -> ""
     (n,m) -> T.pack $ "(" ++ (show n) ++ " new in " ++ (show m) ++ ")"
  "newLink" ## \n -> do
    let splices = do
          "new" ## return $ yieldRuntimeText $ do
            stats <- n
            return $ T.pack $ show $ newComics stats
-- TODO: bring newest cid to this link for cache invalidation
    let attrSplices = "href" ## const $ return [(T.pack "href", T.pack "browse.html?sort=new")]
    content <- withLocalSplices splices attrSplices runChildren
    flip bindLater n $ \stats -> do
      let new = newComics stats
      if new > 0 then codeGen content else return mempty

contentSplices
  :: [X.Node]
  -> Splices (RuntimeAppHandler UserPrefs)
contentSplices childNodes = do
  "kaolSubs" ## renderKaolSubs
  "apply-content" ## (C.withSplices (C.runNodeList childNodes) (contentSplices []))
  "ifLoggedIn" ## C.deferMany (C.withSplices C.runChildren loggedInSplices) .
                  \n -> runMaybeT $ do
                    u <- MaybeT $ user <$> n
                    return (uname u, ucsrfToken u)
  "ifLoggedOut" ## C.conditionalChildren
    (const C.runChildren)
    (isNothing . user)
  "listing" ## renderListing
  "externA" ## renderExternA
  "withCid" ## renderWithCid

loggedInSplices
  :: Splices (RuntimeAppHandler (Text, UUID))
loggedInSplices = do
  "loggedInUser" ## C.pureSplice . C.textSplice $ HTML.text . fst
  "csrf" ## C.pureSplice . C.textSplice $ toText . snd
  "csrfForm" ## csrfForm
  "profileLink" ## profileLink

profileLink
  :: RuntimeAppHandler (Text, UUID)
profileLink =
  let mkLink = \prof -> encodePathToText ["profile.html"]
                        [("name", Just $ encodeUtf8 prof)]
  in C.pureSplice . C.textSplice $ mkLink . fst

csrfForm
  :: RuntimeAppHandler (Text, UUID)
csrfForm _ = do
  node <- getParamNode
  let csrfInput = X.Element "input" [ ("type", "hidden")
                                    , ("name", "csrf_ham")
                                    , ("value", "${h:csrf}")
                                    ] []
  let node' = node { X.elementTag = "form"
                   , X.elementChildren = csrfInput:(X.elementChildren node)
                   }
  runNode node'

renderExternA
  :: RuntimeAppHandler UserPrefs
renderExternA runtime = do
  node <- getParamNode
  let node' = node {X.elementTag = "a"}
  externTpl <- C.runNode
               . X.setAttribute "target" "_blank"
               . X.setAttribute "rel" "noopener noreferrer" $ node'
  localTpl <- C.runNode node'
  return $ C.yieldRuntime $ do
    p <- runtime
    C.codeGen (if (newExternWindows p)
               then externTpl
               else localTpl)
