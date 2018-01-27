{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Piperka.Splices
  (
    piperkaSplices
  )
  where

import Control.Applicative ((<|>))
import Control.Error.Util (note, bool)
import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Data.DList (DList)
import Data.Map.Syntax
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.UUID
import Heist
import Heist.Compiled as C
import qualified Heist.Compiled.Extra as C
import qualified HTMLEntities.Text as HTML
import Snap
import Snap.Snaplet.CustomAuth
import qualified Text.XmlHtml as X

import Application
import Backend()
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
  "script" ## hashTaggedNode "script" "src"
  "stylesheet" ## hashTaggedNode "link" "href"
  "subscribeForm" ## callTemplate "_subscribe"
  "submit" ## renderSubmit ini
  "ad" ## do
    x <- runChildren
    return $ yieldRuntime $ bool (return mempty) (C.codeGen x) =<<
      (lift $ view adsEnabled)
  "adInit" ## callTemplate "_projectWonderful"
  "paramAttrs" ## withLocalSplices mempty ("value" ## paramValue) runChildren
  "piperka" ## renderPiperka
-- Splice definition overridden if used via withCid.
  "comicInfo" ## renderMinimal renderComicInfo
  <> messagesSplices
  where
    hashTaggedNode tagName srcAttr = do
      node <- getParamNode
      let src = fromJust $ X.getAttribute srcAttr node
      token <- maybe (liftIO $ randomString 6) return $
        lookup src $ scriptHash ini
      runNode $ X.setAttribute srcAttr (src <> "?v=" <> token) $
        node {X.elementTag = tagName}

renderMinimal
  :: RuntimeAppHandler (Maybe MyData)
  -> C.Splice AppHandler
renderMinimal action =
  (\n -> withSplices (action n) contentSplices' n) `C.defer`
  (lift currentUserPlain)

renderContent
  :: [X.Node]
  -> RuntimeAppHandler (Maybe MyData)
renderContent ns n = do
  authContent <- deferMany (withSplices (callTemplate "_authFailure") authErrorSplices) $ do
    suppress <- lift $ withTop' id $ view suppressError
    if suppress then return Nothing else do
      err1 <- lift $ withTop auth $ getAuthFailData
      err2 <- lift $ withTop apiAuth $ getAuthFailData
      return $ err1 <|> err2
  content <- withSplices (runNodeList ns) contentSplices' n
  return $ authContent <> content

renderPiperka
  :: C.Splice AppHandler
renderPiperka = do
  node <- getParamNode
  let ads = maybe True (read . T.unpack) $ X.getAttribute "ads" node
  let xs = X.childNodes node
  let getInner n = do
        content <- renderContent xs $ snd <$> n
        C.withSplices (C.callTemplate "_base")
          (contentSplices content) n
      renderInner n =
        let inner = getInner $ (\(a, u) -> (a, user <$> u)) <$> n
        in C.eitherDeferMap (return . note () . snd)
           (C.withSplices inner nullStatsSplices)
           (C.withSplices inner statsSplices) n
  normal <- (renderInner `C.defer`) $ do
    u <- lift $ withTop auth currentUser
    a <- lift $ withTop' id $ view actionResult
    return (a, u)
  bare <- renderMinimal $ nullCreateSplice .
          (C.withSplices (C.runNodeList xs)
           (contentSplices' <> ("onlyWithStats" ## const $ return mempty)))
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
  :: DList (Chunk AppHandler)
  -> Splices (RuntimeAppHandler (Maybe (Maybe ActionError, Maybe Action), Maybe MyData))
contentSplices content =
  ("action" ## (\n -> renderAction (return content) $ fst <$> n)) <>
  (mapV (. fmap snd)) contentSplices'

contentSplices'
  :: Splices (RuntimeAppHandler (Maybe MyData))
contentSplices' = do
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
  "ifMod" ## C.conditionalChildren
    (const (C.withLocalSplices ("submissions" ## renderSubmissions) mempty C.runChildren))
    (maybe False moderator)
  "email" ## defer $ \n -> return $ yieldRuntimeText $
    maybe (return "") (lift . getUserEmail . uid) =<< n
  "providers" ## renderProviders
  "oauth2Create" ## renderOAuth2
  "readers" ## renderReaders

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
