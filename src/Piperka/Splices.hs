{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Piperka.Splices
  (
    piperkaSplices
  )
  where

import Heist
import Heist.Compiled as C
import Control.Error.Util (note, bool)
import Control.Monad (when)
import Control.Monad.State
import Control.Lens
import Data.DList (DList)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Snap
import Control.Applicative ((<|>))
import Data.Monoid
import Snap.Snaplet.CustomAuth
import Backend()
import qualified Text.XmlHtml as X
import Data.Map.Syntax
import qualified Heist.Compiled.Extra as C

import Data.Maybe
import Data.UUID
import qualified HTMLEntities.Text as HTML

import Application
import Piperka.Account
import Piperka.Action.Splices
import Piperka.Action.Types
import Piperka.Auth (currentUserPlain)
import Piperka.Auth.Splices
import Piperka.OAuth2.Splices
import Piperka.Splices.Account
import Piperka.Splices.Flavor
import Piperka.Splices.Providers
import Piperka.Splices.Revert
import Piperka.Submission
import Piperka.Submit.Splices
import Piperka.Listing.Render
import Piperka.ComicInfo.Splices
import Piperka.Messages
import Piperka.Readers
import Piperka.Recover
import Piperka.Util

piperkaSplices
  :: AppInit
  -> Splices (C.Splice AppHandler)
piperkaSplices ini = do
  "piperka" ## renderPiperka ini
-- Splice definition overridden if used via withCid.
  "comicInfo" ## renderMinimal ini renderComicInfo
  <> messagesSplices

renderMinimal
  :: AppInit
  -> RuntimeAppHandler (Maybe MyData)
  -> C.Splice AppHandler
renderMinimal ini action =
  (\n -> withSplices (action n) (contentSplices' ini) n) `C.defer`
  (lift currentUserPlain)

renderContent
  :: AppInit
  -> [X.Node]
  -> RuntimeAppHandler (Maybe MyData)
renderContent ini ns n = do
  authContent <- deferMany (withSplices (callTemplate "_authFailure") authErrorSplices) $ do
    suppress <- lift $ withTop' id $ view suppressError
    if suppress then return Nothing else do
      err1 <- lift $ withTop auth $ getAuthFailData
      err2 <- lift $ withTop apiAuth $ getAuthFailData
      return $ err1 <|> err2
  content <- withSplices (runNodeList ns) (contentSplices' ini) n
  return $ authContent <> content

renderPiperka
  :: AppInit
  -> C.Splice AppHandler
renderPiperka ini = do
  node <- getParamNode
  let ads = maybe True (read . T.unpack) $ X.getAttribute "ads" node
  let xs = X.childNodes node
  let getInner n = do
        content <- renderContent ini xs $ snd <$> n
        C.withSplices (C.callTemplate "_base")
          (contentSplices ini content) n
      renderInner n =
        let inner = getInner $ (\(a, u) -> (a, user <$> u)) <$> n
        in C.eitherDeferMap (return . note () . snd)
           (C.withSplices inner nullStatsSplices)
           (C.withSplices inner statsSplices) n
  normal <- (renderInner `C.defer`) $ do
    u <- lift $ withTop auth currentUser
    a <- lift $ withTop' id $ view actionResult
    return (a, u)
  bare <- renderMinimal ini $ nullCreateSplice .
          (C.withSplices (C.runNodeList xs)
           ((contentSplices' ini) <> ("onlyWithStats" ## const $ return mempty)))
  return $ yieldRuntime $ do
    when (not ads) (lift $ modify $ set adsEnabled False)
    useMinimal <- lift $ view minimal
    codeGen $ if useMinimal then bare else normal

statsSplices
  :: Splices (RuntimeAppHandler UserWithStats)
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
  "modStats" ## \n -> do
    let modSplices =
          (mapV (C.pureSplice . C.textSplice . (fmap (T.pack . show))) $ do
              "modCount" ## fst
              "modDays" ## snd
          )
          <> ("haveModCount" ## C.conditionalChildren (const C.runChildren) ((>0) . fst))
          <> ("nag" ## C.conditionalChildren (const C.runChildren) ((>6) . snd))
    spl <- C.mayDeferMap (return . modStats)
           (C.withSplices C.runChildren modSplices) n
    return $ yieldRuntime $ do
      m <- moderator . user <$> n
      if m then C.codeGen spl else return mempty
  "onlyWithStats" ## const $ C.runChildren

nullStatsSplices
  :: Splices (RuntimeAppHandler a)
nullStatsSplices = mapV (const . return) $ do
  "unreadStats" ## mempty
  "newLink" ## mempty
  "modStats" ## mempty
  "onlyWithStats" ## mempty

contentSplices
  :: AppInit
  -> DList (Chunk AppHandler)
  -> Splices (RuntimeAppHandler (Maybe (Maybe ActionError, Maybe Action), Maybe MyData))
contentSplices ini content =
  ("action" ## (\n -> renderAction (return content) $ fst <$> n)) <>
  (mapV (. fmap snd)) (contentSplices' ini)

contentSplices'
  :: AppInit
  -> Splices (RuntimeAppHandler (Maybe MyData))
contentSplices' ini = do
  "subscribeForm" ## const $ callTemplate "_subscribe"
  "kaolSubs" ## renderKaolSubs
  "ifLoggedIn" ## C.deferMany (C.withSplices C.runChildren loggedInSplices)
  "ifLoggedOut" ## C.conditionalChildren
    (const C.runChildren)
    isNothing
  "notMod" ## \n -> do
    spl <- runChildren
    return $ yieldRuntime $ do
      m <- maybe False moderator <$> n
      if m
        then return mempty
        else lift (modifyResponse $ setResponseCode 403) >> codeGen spl
  "listing" ## renderListing
  "externA" ## \n -> renderExternA $ getPrefs <$> n
  "withCid" ## renderWithCid
  "csrfForm" ## csrfForm
  "passwordRecovery" ## renderPasswordRecovery
  "usePasswordHash" ## renderUsePasswordHash
  "submit" ## const $ renderSubmit ini
  "submissions" ## const $ renderSubmissions
  "ifMod" ## C.conditionalChildren
    (const C.runChildren)
    (maybe False moderator)
  "email" ## defer $ \n -> return $ yieldRuntimeText $
    maybe (return "") (lift . getUserEmail . uid) =<< n
  "paramAttrs" ## const $ withLocalSplices mempty ("value" ## paramValue) runChildren
  "providers" ## renderProviders
  "oauth2Create" ## renderOAuth2
  "readers" ## renderReaders
  "ad" ## const $ do
    x <- runChildren
    return $ yieldRuntime $ bool (return mempty) (C.codeGen x) =<<
      (lift $ view adsEnabled)
  "adInit" ## const $ callTemplate "_projectWonderful"

loggedInSplices
  :: Splices (RuntimeAppHandler MyData)
loggedInSplices = do
  "loggedInUser" ## C.pureSplice . C.textSplice $ HTML.text . uname
  "csrf" ## C.pureSplice . C.textSplice $ toText . ucsrfToken
  "profileLink" ## profileLink
  "listOfEdits" ## renderListOfEdits
  "accountForm" ## renderAccountForm
  "recent" ## renderRecent

profileLink
  :: RuntimeAppHandler MyData
profileLink =
  let mkLink = \prof -> encodePathToText ["profile.html"]
                        [("name", Just $ encodeUtf8 prof)]
  in C.pureSplice . C.textSplice $ mkLink . uname

csrfForm
  :: RuntimeAppHandler a
csrfForm _ = do
  rootNode <- getParamNode
  let sub = X.childNodes rootNode
  inner <- runNodeList sub
  let formSplices = do
        "apply-form-content" ## return inner
        "form" ## do
          node <- getParamNode
          let node' = node { X.elementTag = "form"
                           , X.elementAttrs = X.elementAttrs rootNode
                           , X.elementChildren = X.childElements node
                           }
          runNode node'
  withLocalSplices formSplices mempty (C.callTemplate "_csrfForm")

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

paramValue
  :: AttrSplice AppHandler
paramValue n = do
  t <- lift $ getParamText $ encodeUtf8 n
  return $ maybe [] ((:[]) . ("value",) . HTML.text) t
