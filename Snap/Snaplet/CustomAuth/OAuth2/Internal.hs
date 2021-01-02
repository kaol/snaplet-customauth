{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Snap.Snaplet.CustomAuth.OAuth2.Internal
  ( oauth2Init
  , saveAction
  , redirectToProvider
  ) where

import Control.Error.Util hiding (err)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Aeson
import qualified Data.Binary
import Data.Binary (Binary)
import Data.Binary.Instances ()
import qualified Data.ByteString.Base64
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.Char (chr)
import qualified Data.Configurator as C
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Network.HTTP.Client (Manager)
import Network.OAuth.OAuth2
import Prelude hiding (lookup)
import Snap hiding (path)
import Snap.Snaplet.Session
import System.Random
import URI.ByteString

import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.Types hiding (name)
import Snap.Snaplet.CustomAuth.User (setUser, currentUser, recoverSession)
import Snap.Snaplet.CustomAuth.Util (getStateName, getParamText, setFailure)

oauth2Init
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Initializer b (AuthManager u e b) (HashMap Text Provider)
oauth2Init s = do
  cfg <- getSnapletUserConfig
  root <- getSnapletRootURL
  hostname <- liftIO $ C.require cfg "hostname"
  scheme <- liftIO $ C.lookupDefault "https" cfg "protocol"
  names <- liftIO $ C.lookupDefault [] cfg "oauth2.providers"
  -- TODO: use discovery
  let makeProvider name = let
        name' = "oauth2." <> name
        lk = MaybeT . C.lookup cfg . (name' <>)
        lku n = lk n >>=
          MaybeT . return . hush . parseURI strictURIParserOptions . encodeUtf8
        callback = URI (Scheme scheme)
                   (Just $ Authority Nothing (Host hostname) Nothing)
                   ("/" <> root <> "/oauth2callback/" <> (encodeUtf8 name))
                   mempty Nothing
        in Provider
           <$> (MaybeT $ return $ pure $ T.toLower $ name)
           <*> (MaybeT $ return $ pure $ Nothing)
           <*> lk ".scope"
           <*> lku ".endpoint.identity"
           <*> lk ".identityField"
           <*> (OAuth2
                <$> lk ".clientId"
                <*> (lift $ runMaybeT $ lk ".clientSecret")
                <*> lku ".endpoint.auth"
                <*> lku ".endpoint.access"
                <*> (pure $ Just callback))
  addRoutes $ mapped._2 %~ (bracket s) $
    [ ("oauth2createaccount", oauth2CreateAccount s)
    , ("oauth2callback/:provider", oauth2Callback s)
    , ("oauth2login/:provider", redirectLogin)
    ]
  liftIO $ M.fromList . map (\x -> (providerName x, x)) . catMaybes <$>
    (mapM (runMaybeT . makeProvider) names)

redirectLogin
  :: Handler b (AuthManager u e b) ()
redirectLogin = do
  provs <- gets providers
  provider <- (flip M.lookup provs =<<) <$> getParamText "provider"
  maybe pass toProvider provider
  where
    toProvider p = do
      success <- redirectToProvider $ providerName p
      if success then return () else pass

getRedirUrl
  :: Provider
  -> Text
  -> URI
getRedirUrl p token =
  appendQueryParams [("state", encodeUtf8 token)
                    ,("scope", encodeUtf8 $ scope p)] $ authorizationUrl $ oauth p

redirectToProvider
  :: Text
  -> Handler b (AuthManager u e b) Bool
redirectToProvider pName = do
  maybe (return False) redirectToProvider' =<< M.lookup pName <$> gets providers

redirectToProvider'
  :: Provider
  -> Handler b (AuthManager u e b) Bool
redirectToProvider' provider = do
  -- Generate a state token and store it in SessionManager
  store <- gets stateStore'
  stamp <- liftIO $ (T.pack . show) <$> getCurrentTime
  name <- getStateName
  let randomChar i
        | i < 10 = chr (i+48)
        | i < 36 = chr (i+55)
        | otherwise = chr (i+61)
      randomText n = T.pack <$> replicateM n (randomChar <$> randomRIO (0,61))
  token <- liftIO $ randomText 20
  withTop' store $ do
    setInSession name token
    setInSession (name <> "_stamp") stamp
    commitSession
  let redirUrl = serializeURIRef' $ getRedirUrl provider token
  redirect' redirUrl 303

getUserInfo
  :: MonadIO m
  => Manager
  -> Provider
  -> AccessToken
  -> ExceptT (Maybe ByteString) m Text
getUserInfo mgr provider token = do
  let endpoint = identityEndpoint provider
  (withExceptT Just $ ExceptT $ liftIO $ authGetJSON mgr token endpoint) >>=
    (maybe (throwE Nothing) pure . lookupProviderInfo)
  where
    lookup' a b = maybeText =<< M.lookup a b
    maybeText (String x) = Just x
    maybeText _ = Nothing
    lookupProviderInfo = lookup' (identityField provider)

oauth2Callback
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Handler b (AuthManager u e b) ()
oauth2Callback s = do
  provs <- gets providers
  maybe pass (oauth2Callback' s) =<<
    ((flip M.lookup provs =<<) <$> getParamText "provider")

oauth2Callback'
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Provider
  -> Handler b (AuthManager u e b) ()
oauth2Callback' s provider = do
  name <- getStateName
  let ss = stateStore s
      mgr = httpManager s
  res <- runExceptT $ do
    let param = oauth provider
    expiredStamp <- lift $ withTop' ss $
      maybe (return True) (liftIO . isExpiredStamp) =<<
      fmap (read . T.unpack) <$> getFromSession (name <> "_stamp")
    when expiredStamp $ throwE ExpiredState
    hostState <- maybe (throwE StateNotStored) return =<<
      (lift $ withTop' ss $ getFromSession name)
    providerState <- maybe (throwE StateNotReceived) return =<<
      (lift $ getParamText "state")
    when (hostState /= providerState) $ throwE BadState
    _ <- runMaybeT $ do
      err <- MaybeT $ lift $ getParam "error"
      lift $ throwE $ ProviderError $ hush $ decodeUtf8' err
    -- Get the user id from provider
    (maybe (throwE (IdExtractionFailed Nothing)) pure =<<
      (fmap ExchangeToken) <$> (lift $ getParamText "code")) >>=
    -- TODO: catch?
      (liftIO . fetchAccessToken mgr param >=>
       either (const $ throwE AccessTokenFetchError) pure >=>
      -- TODO: get user id (sub) from idToken in token, if
      -- available. Requires JWT handling.
       withExceptT (IdExtractionFailed . ((hush . decodeUtf8' . toStrict) =<<)) .
        getUserInfo (httpManager s) provider . accessToken)
  either (setFailure ((oauth2Failure s) SCallback) (Just $ providerName provider) .
          Right . Create . OAuth2Failure)
    (oauth2Success s provider) res

-- User has successfully completed OAuth2 login.  Get the stored
-- intended action and perform it.
oauth2Success
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Provider
  -> Text
  -> Handler b (AuthManager u e b) ()
oauth2Success s provider token = do
  key <- getActionKey $ providerName provider
  store <- gets stateStore'
  name <- getStateName
  act <- withTop' store $ runMaybeT $ do
    act <- MaybeT $ getFromSession key
    lift $ deleteFromSession key >> commitSession
    return act
  withTop' store $ do
    setInSession (name <> "_provider") (providerName provider)
    setInSession (name <> "_token") token
    commitSession
  -- When there's no user defined action stored, treat this as a
  -- regular login
  maybe (doOauth2Login s provider token) (doResume s provider token) act

doOauth2Login
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Provider
  -> Text
  -> Handler b (AuthManager u e b) ()
doOauth2Login s provider token = do
  -- Sanity check: See if the user is already logged in.
  recoverSession
  currentUser >>=
    maybe proceed (const $ setFailure ((oauth2Failure s) SLogin)
                   (Just $ providerName provider) $
                   Right $ Create $ OAuth2Failure AlreadyLoggedIn)
  where
    proceed = do
      res <- runExceptT $ do
        usr <- ExceptT $ (oauth2Login s) (providerName provider) token
        maybe (return ()) (lift . setUser) usr
        return usr
      either (setFailure ((oauth2Failure s) SLogin)
              (Just $ providerName provider) . Left)
        (const $ oauth2LoginDone s) res

isExpiredStamp
  :: UTCTime
  -> IO Bool
isExpiredStamp stamp = do
  current <- getCurrentTime
  let diff = diffUTCTime current stamp
  return $ diff < 0 || diff > 300

prepareOAuth2Create'
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Provider
  -> Text
  -> Handler b (AuthManager u e b) (Either (Either e CreateFailure) i)
prepareOAuth2Create' s provider token =
  (prepareOAuth2Create s) (providerName provider) token >>=
  either checkDuplicate (return . Right)
  where
    checkDuplicate e = do
      isE <- isDuplicateError e
      return $ Left $ if isE then Right $ OAuth2Failure IdentityInUse else Left e

-- Check that stored action is not too old and that user matches
doResume
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Provider
  -> Text
  -> Text
  -> Handler b (AuthManager u e b) ()
doResume s provider token d = do
  recoverSession
  user <- currentUser
  userId <- runMaybeT $ lift . getUserId =<< (MaybeT $ return user)
  res <- runExceptT $ do
    d' <- ExceptT . return $ maybe (Left $ Right ActionDecodeError) Right $
      ((fmap $ \(_, _, x) -> x) . hush . Data.Binary.decodeOrFail . fromStrict) =<<
      (hush $ Data.ByteString.Base64.decode $ encodeUtf8 d)
    when (requireUser d' && isNothing user) $ throwE (Right AttachNotLoggedIn)
    u <- ExceptT $ return . either (Left . Left) Right =<<
      (oauth2Check s) (providerName provider) token
    -- Compare current user with action's stored user
    when (userId /= actionUser d') $
     throwE (Right ActionUserMismatch)
    case requireUser d' of
      -- Compare current user with identity owner
      True -> when (maybe True ((/= userId) . Just) u) $
        throwE (Right ActionUserMismatch)
      -- Ensure that the identity is not yet used
      False -> when (isJust u) $ throwE (Right AlreadyAttached)
    expired <- liftIO $ isExpiredStamp (actionStamp d')
    when expired $ throwE (Right ActionTimeout)
    return $ savedAction d'
  either (setFailure ((oauth2Failure s) SAction)
          (Just $ providerName provider) . fmap Action)
    ((resumeAction s) (providerName provider) token) res

-- User has successfully signed in via oauth2 and the provider/token
-- did not match with an existing user.  This is the endpoint for
-- requesting account creation afterwards.
oauth2CreateAccount
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Handler b (AuthManager u e b) ()
oauth2CreateAccount s = do
  store <- gets stateStore'
  provs <- gets providers
  usrName <- ((hush . decodeUtf8') =<<) <$>
    (getParam =<< ("_new" <>) <$> gets userField)
  name <- getStateName
  provider <- (flip M.lookup provs =<<) <$>
    (withTop' store $ getFromSession (name <> "_provider"))
  user <- runExceptT $ do
    -- Sanity check: See if the user is already logged in.
    u <- lift $ recoverSession >> currentUser
    when (isJust u) $ throwE (Right $ OAuth2Failure AlreadyUser)
    -- Get userName
    userName <- hoistEither $ note (Right MissingName) usrName
    -- Get the token and provider from session store
    res <- maybe (throwE $ Right $ OAuth2Failure NoStoredToken) return =<<
           (lift $ withTop' store $ runMaybeT $ do
               provider' <- MaybeT $ return provider
               token <- MaybeT $ getFromSession (name <> "_token")
               return (provider', token))
    ExceptT $ fmap (,userName) <$> prepareOAuth2Create' s (fst res) (snd res)
  res <- runExceptT $ do
    (i, userName) <- hoistEither user
    usr <- ExceptT $ create userName i
    lift $ setUser usr
    return usr
  case (user, res) of
    (Right (i,_), Left _) -> cancelPrepare i
    _ -> return ()
  either (setFailure ((oauth2Failure s) SCreate) (providerName <$> provider) . fmap Create)
    (oauth2AccountCreated s) res

getActionKey
  :: Text
  -> Handler b (AuthManager u e b) Text
getActionKey p = do
  path <- maybe "auth" id . hush . decodeUtf8' <$> getSnapletRootURL
  name <- maybe "auth" id <$> getSnapletName
  return $ "__" <> name <> "_" <> path <> "_action_" <> p

saveAction
  :: (IAuthBackend u i e b, Binary a)
  => Bool
  -> Text
  -> a
  -> Handler b (AuthManager u e b) ()
saveAction require provider a = do
  provs <- gets providers
  guard $ provider `elem` (M.keys provs)
  let d = Data.Binary.encode a
  key <- getActionKey provider
  store <- gets $ stateStore'
  stamp <- liftIO $ getCurrentTime
  i <- runMaybeT $ lift . getUserId =<< MaybeT currentUser
  let payload = SavedAction {
          actionProvider = provider
        , actionStamp = stamp
        , actionUser = i
        , requireUser = require
        , savedAction = toStrict d
        }
  let d' = decodeLatin1 $ Data.ByteString.Base64.encode $
        toStrict . Data.Binary.encode $ payload
  withTop' store $ do
    setInSession key d'
    commitSession
