{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Piperka.Splices
  (
    piperkaSplices
  )
  where

import Heist
import qualified Heist.Compiled as C
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
import Piperka.Messages
import Piperka.Util

defaultUserPrefs :: UserPrefs
defaultUserPrefs = UserPrefs
  { user = Nothing
  , newComics = 0
  , unreadCount = (0,0)
  , rows = 40
  , columns = TwoColumn
  , newExternWindows = False
  }

piperkaSplices
  :: SnapletLens App (AuthManager UserPrefs App)
  -> Splices (C.Splice AppHandler)
piperkaSplices a = do
  "piperka" ## renderPiperka (runtimePiperka a)
  <> messagesSplices

runtimePiperka
  :: SnapletLens App (AuthManager UserPrefs App)
  -> RuntimeSplice AppHandler UserPrefs
runtimePiperka a = do
  u <- lift $ withTop a (combinedLoginRecover $ \_ -> do
                            saveMessage "login failed"
                            redirect' "/" 303)
  return $ maybe defaultUserPrefs id u

renderPiperka
  :: RuntimeSplice AppHandler UserPrefs
  -> C.Splice AppHandler
renderPiperka runtime = do
  childNodes <- fmap X.childNodes getParamNode
  C.withSplices
    (C.callTemplate "_base")
    (contentSplices childNodes) `C.defer` runtime

contentSplices
  :: [X.Node]
  -> Splices (RuntimeSplice AppHandler UserPrefs
              -> C.Splice AppHandler)
contentSplices childNodes = do
  "kaolSubs" ## renderKaolSubs
  "unreadStats" ## \runtime -> return $ C.yieldRuntimeText $ do
    prefs <- runtime
    return $ case unreadCount prefs of
     (0,_) -> ""
     (n,m) -> T.pack $ "(" ++ (show n) ++ " new in " ++ (show m) ++ ")"
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

loggedInSplices
  :: Splices (RuntimeSplice AppHandler (Text, UUID)
              -> C.Splice AppHandler)
loggedInSplices = do
  "loggedInUser" ## C.pureSplice . C.textSplice $ HTML.text . fst
  "csrf" ## C.pureSplice . C.textSplice $ toText . snd
  "profileLink" ## profileLink

profileLink
  :: RuntimeSplice AppHandler (Text, UUID)
  -> C.Splice AppHandler
profileLink =
  let mkLink = \prof -> encodePathToText ["profile.html"]
                        [("name", Just $ encodeUtf8 prof)]
  in C.pureSplice . C.textSplice $ mkLink . fst

renderExternA
  :: RuntimeSplice AppHandler UserPrefs
  -> C.Splice AppHandler
renderExternA runtime = do
  node <- getParamNode
  let node' = node {X.elementTag = "a"}
  externTpl <- C.runNode
               . X.setAttribute "target" "_blank"
               . X.setAttribute "rel" "noopener noreferrer" $ node'
  localTpl <- C.runNode node'
  return $ C.yieldRuntime $ do
    prefs <- runtime
    C.codeGen (if (newExternWindows prefs)
               then externTpl
               else localTpl)
