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
import Data.DList (DList)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Snap
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Snap.Snaplet.CustomAuth
import Backend()
import qualified Text.XmlHtml as X
import Data.Map.Syntax
import qualified Heist.Compiled.Extra as C
import qualified Hasql.Session

import Data.Maybe
import Data.UUID
import qualified HTMLEntities.Text as HTML

import Application
import Piperka.Account
import Piperka.Action
import Piperka.Action.Splices
import Piperka.Action.Types
import Piperka.Splices.Account
import Piperka.Splices.Flavor
import Piperka.Splices.Revert
import Piperka.Submit.Splices
import Piperka.Listing.Render
import Piperka.ComicInfo.Splices
import Piperka.Messages
import Piperka.Recover
import Piperka.Util

loginFailed :: forall b b1 v. b -> Handler App v b1
loginFailed = const $ do
  saveMessage "login failed"
  redirect' "/" 303

accountCreationFailed
  :: C.Splice AppHandler
  -> RuntimeAppHandler (Either Hasql.Session.Error CreateFailure)
accountCreationFailed content n = do
  withSplices content (accountCreateFailSplices <> nullStatsSplices) n

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
  -> RuntimeAppHandler UserPrefs
  -> C.Splice AppHandler
renderMinimal ini action =
  (\n -> withSplices (action n) (contentSplices' ini) n) `C.defer`
  (fromMaybe defaultUserPrefs <$>
   (lift $ withTop apiAuth $ combinedLoginRecover loginFailed))

renderContent
  :: AppInit
  -> [X.Node]
  -> RuntimeAppHandler UserPrefs
renderContent ini ns = C.withSplices (C.runNodeList ns) $ contentSplices' ini

prefsMayCreate
  :: ExceptT (Either Hasql.Session.Error CreateFailure) (RuntimeSplice AppHandler)
     (Maybe UserPrefsWithStats)
prefsMayCreate = do
  p <- lift $ lift $ withTop auth $ combinedLoginRecover loginFailed
  case p of
   Just _ -> return p
   Nothing -> do
     isCreate <- lift $ lift $ (== (Just "Create account")) <$> getParam "action"
     if isCreate
       then (ExceptT $ lift $ withTop auth createAccount) >>=
            (const $ lift $ lift $ redirect' "/welcome.html" 303)
       else return Nothing

getParamAttrBool
  :: T.Text
  -> HeistT AppHandler IO Bool
getParamAttrBool n =
  maybe False (read . T.unpack) . X.getAttribute n <$> getParamNode

renderPiperka
  :: AppInit
  -> C.Splice AppHandler
renderPiperka ini = do
  mayCreateUser <- getParamAttrBool "newUser"
  xs <- X.childNodes <$> getParamNode
  let prefsWithStats = lift $ withTop auth $ combinedLoginRecover loginFailed
  let getInner n = do
        content <- renderContent ini xs $ snd <$> n
        C.withSplices (C.callTemplate "_base")
          (contentSplices ini content) n
      defaultInner = getInner $ return (Nothing, defaultUserPrefs)
      renderInner s n = let inner = getInner $ (\(a, p) -> (a, prefs p)) <$> n
                        in C.withSplices inner (statsSplices <> s) n
  normal <- if mayCreateUser
            then
              (C.eitherDeferMap return (paramValueSplice . accountCreationFailed defaultInner)
               (nullCreateSplice . renderInner mempty))
              `C.defer`
              (runExceptT $ (lift . processAction) =<<
               fromMaybe defaultUserPrefsWithStats <$> prefsMayCreate)
            else
              renderInner mempty `C.defer`
              (processAction =<< fromMaybe defaultUserPrefsWithStats <$> prefsWithStats)
  bare <- renderMinimal ini $ nullCreateSplice .
          (C.withSplices (C.runNodeList xs)
           ((contentSplices' ini) <> ("onlyWithStats" ## const $ return mempty)))
  return $ yieldRuntime $ do
    useMinimal <- lift $ isJust <$> getParam "minimal"
    codeGen $ if useMinimal then bare else normal

statsSplices
  :: Splices (RuntimeAppHandler (a, UserPrefsWithStats))
statsSplices = mapV (. fmap snd) $ do
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
      m <- maybe False moderator . (user . prefs) <$> n
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
  -> Splices (RuntimeAppHandler (Maybe (Maybe ActionError, Maybe Action), UserPrefs))
contentSplices ini content =
  ("action" ## renderAction $ return content) <>
  (mapV (. fmap snd)) (contentSplices' ini)

contentSplices'
  :: AppInit
  -> Splices (RuntimeAppHandler UserPrefs)
contentSplices' ini = do
  "subscribeForm" ## const $ callTemplate "_subscribe"
  "kaolSubs" ## renderKaolSubs
  "ifLoggedIn" ## C.deferMany (C.withSplices C.runChildren loggedInSplices) .
    \n -> user <$> n
  "ifLoggedOut" ## C.conditionalChildren
    (const C.runChildren)
    (isNothing . user)
  "notMod" ## \n -> do
    spl <- runChildren
    return $ yieldRuntime $ do
      m <- maybe False moderator . user <$> n
      if m
        then return mempty
        else lift (modifyResponse $ setResponseCode 403) >> codeGen spl
  "listing" ## renderListing
  "externA" ## renderExternA
  "withCid" ## renderWithCid
  "csrfForm" ## csrfForm
  "passwordRecovery" ## renderPasswordRecovery
  "usePasswordHash" ## renderUsePasswordHash
  "accountForm" ## renderAccountForm
  "recent" ## renderRecent
  "submit" ## \n -> renderSubmit ini n
  "ifMod" ## C.conditionalChildren
    (const C.runChildren)
    (maybe False moderator . user)

loggedInSplices
  :: Splices (RuntimeAppHandler MyData)
loggedInSplices = do
  "loggedInUser" ## C.pureSplice . C.textSplice $ HTML.text . uname
  "csrf" ## C.pureSplice . C.textSplice $ toText . ucsrfToken
  "profileLink" ## profileLink
  "listOfEdits" ## renderListOfEdits

profileLink
  :: RuntimeAppHandler MyData
profileLink =
  let mkLink = \prof -> encodePathToText ["profile.html"]
                        [("name", Just $ encodeUtf8 prof)]
  in C.pureSplice . C.textSplice $ mkLink . uname

csrfForm
  :: RuntimeAppHandler UserPrefs
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
