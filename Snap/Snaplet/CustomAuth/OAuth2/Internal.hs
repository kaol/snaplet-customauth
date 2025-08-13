{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Snap.Snaplet.CustomAuth.OAuth2.Internal
  ( oauth2Init
  , saveAction
  , redirectToProvider
  ) where

import Control.Error.Util hiding (err)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.State
import qualified Crypto.Hash
import Crypto.Random (getRandomBytes)
import Data.Aeson
import qualified Data.Binary
import Data.Binary (Binary)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Base64.URL as URL
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import qualified Data.Configurator as C
import Data.List (find, lookup)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, fromJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime(utctDayTime), getCurrentTime, diffUTCTime)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import qualified Network.URI
import Network.OAuth.OAuth2 hiding (fetchAccessToken, error, scope)
import Network.OAuth.OAuth2.AuthorizationRequest
import Network.OAuth.OAuth2.TokenRequest
import Prelude hiding (lookup)
import Snap hiding (path)
import Snap.Snaplet.Session
import URI.ByteString

import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.OAuth2.Internal.DPoP
import Snap.Snaplet.CustomAuth.OAuth2.Internal.PAR
import Snap.Snaplet.CustomAuth.OAuth2.Internal.UserInfo
import Snap.Snaplet.CustomAuth.Types hiding (name)
import Snap.Snaplet.CustomAuth.User (setUser, currentUser, recoverSession)
import Snap.Snaplet.CustomAuth.Util (getStateName, getParamText, setFailure, getTruncatedCurrentTime, fromURIByteString )

oauth2Init
  :: IAuthBackend u i e b
  => AuthSettings
  -> OAuth2Settings p u i e b
  -> Initializer b (AuthManager u e b) (Map Text Provider)
oauth2Init auths s = do
  cfg <- getSnapletUserConfig
  root <- getSnapletRootURL
  hostname <- liftIO $ C.require cfg "hostname"
  scheme <- liftIO $ C.lookupDefault "https" cfg "protocol"
  let names = map fst $ enabledProviders s
  -- TODO: use discovery
  let makeProvider name = let
        name' = "oauth2." <> name
        lk = MaybeT . C.lookup cfg . (name' <>)
        lku n = lk n >>=
          MaybeT . return . hush . parseURI strictURIParserOptions . encodeUtf8
        lkMaybe n = lift $
          (hush . parseURI strictURIParserOptions . encodeUtf8 =<<) <$>
          C.lookup cfg (name' <> n)
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
           <*> lkMaybe ".endpoint.par"
           <*> lkMaybe ".issuer"
           <*> (lift $ C.lookupDefault False cfg $ name' <> ".dpop")
           <*> lkMaybe ".endpoint.jwks"
           <*> (OAuth2
                <$> lk ".clientId"
                <*> (lift . fmap (fromMaybe "") . runMaybeT $ lk ".clientSecret")
                <*> lku ".endpoint.auth"
                <*> lku ".endpoint.access"
                <*> (pure callback))
  providers <- liftIO $ catMaybes <$> mapM (runMaybeT . makeProvider) names
  addRoutes $ mapped._2 %~ (bracket s) $
    [ ("oauth2createaccount", oauth2CreateAccount s)
    , ("oauth2callback/:provider", oauth2Callback s)
    , ("oauth2login/:provider", redirectLogin)
    ]
  maybe (pure ()) (\p -> error $ "JWK not provided while provider " <>
                         T.unpack (providerName p) <> " uses PAR") $
    guard (isNothing (auths ^. authJWK)) *> find (isJust . parEndpoint) providers
  return . M.fromList $ map ((,) <$> providerName <*> id) providers

redirectLogin
  :: Handler b (AuthManager u e b) ()
redirectLogin = do
  provs <- gets providers
  provider <- (flip M.lookup provs =<<) <$> getParamText "provider"
  maybe pass toProvider provider
  where
    toProvider p = do
      success <- redirectToProvider $ providerName p
      when (isNothing success) pass

userProvider
  :: OAuth2Settings p u i e b
  -> Provider
  -> p
userProvider s provider =
  fromJust $ lookup (providerName provider) $ enabledProviders s

getRedirUrl
  :: Provider
  -> B.ByteString
  -> B.ByteString
  -> URI
getRedirUrl p token pkceChallenge =
  appendQueryParams [("state", token)
                    ,("code_challenge", pkceChallenge)
                    ,("scope", encodeUtf8 $ scope p)] $ authorizationUrl $ oauth p

redirectToProvider
  :: Text
  -> Handler b (AuthManager u e b) (Maybe RedirectError)
redirectToProvider pName = do
  maybe (return $ Just UnknownProvider) redirectToProvider' =<< M.lookup pName <$> gets providers

redirectToProvider'
  :: Provider
  -> Handler b (AuthManager u e b) (Maybe RedirectError)
redirectToProvider' provider = fmap (either Just (const Nothing)) $ runExceptT $ do
  let oa = oauth provider
  -- Generate a state token and store it in SessionManager
  store <- lift $ gets stateStore'
  -- Some implementations may mind fractional iat/exp claims
  curr <- liftIO getTruncatedCurrentTime
  name <- (<> providerName provider) <$> lift getStateName
  -- PKCE verifier, used in access token fetch
  (token, pkceVerifier) <- liftIO genParams
  -- PKCE challenge, sent in the initial redirect/PAR request
  let pkceChallenge =
        decodeLatin1 . URL.encodeUnpadded . B.pack .
        ByteArray.unpack @(Crypto.Hash.Digest Crypto.Hash.SHA256) $
        Crypto.Hash.hash pkceVerifier
      baseParams =
        [ ("response_type", "code")
        , ("client_id", encodeUtf8 $ oauth2ClientId oa)
        , ("redirect_uri", serializeURIRef' $ oauth2RedirectUri oa)
        ]
      stateParams =
        [ ("state", token)
        , ("code_challenge", encodeUtf8 pkceChallenge)
        , ("code_challenge_method", "S256")
        , ("scope", encodeUtf8 $ scope provider)
        ]

  lift $ withTop' store $ do
    setInSession name $ decodeLatin1 token
    setInSession (name <> "_stamp") . T.pack $ show curr
    setInSession (name <> "_pkce_verifier") (decodeLatin1 $ pkceVerifier)
    commitSession
  lift . flip redirect' 303 =<< serializeURIRef' <$> maybe
    -- Known statically
    (lift . pure . appendQueryParams stateParams $ authorizationUrl oa)
    -- PAR. Need to fetch parameter with POST
    (parRequest provider curr (baseParams <> stateParams))
    (parEndpoint provider)
  where
    genParams =
      (,)
      -- State token
      <$> (URL.encodeUnpadded <$> getRandomBytes 20)
      -- PKCE verifier
      <*> (URL.encodeUnpadded <$> getRandomBytes 32)

oauth2Callback
  :: IAuthBackend u i e b
  => OAuth2Settings p u i e b
  -> Handler b (AuthManager u e b) ()
oauth2Callback s = do
  provs <- gets providers
  maybe pass (oauth2Callback' s) =<<
    ((flip M.lookup provs =<<) <$> getParamText "provider")

oauth2Callback'
  :: IAuthBackend u i e b
  => OAuth2Settings p u i e b
  -> Provider
  -> Handler b (AuthManager u e b) ()
oauth2Callback' s provider = do
  name <- (<> providerName provider) <$> getStateName
  mgr <- gets httpManager
  let ss = stateStore s
  res <- runExceptT $ do
    let param = oauth provider
    expiredStamp <- lift $ withTop' ss $
      maybe (return True) (liftIO . isExpiredStamp) =<<
      fmap (read . T.unpack) <$> getFromSession (name <> "_stamp")
    when expiredStamp $ throwE ExpiredState
    hostState <- maybe (throwE StateNotStored) return =<<
      (lift $ withTop' ss $ getFromSession name)
    pkceVerifier <- maybe (throwE StateNotStored) (pure . encodeUtf8) =<<
      (lift $ withTop' ss $ getFromSession $ name <> "_pkce_verifier")
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
      (fetchAccessToken mgr param pkceVerifier >=>
       withExceptT IdExtractionFailed .
        getUserInfo mgr provider . accessToken)
  either (setFailure ((oauth2Failure s) SCallback) (Just $ providerName provider) .
          Right . Create . OAuth2Failure)
    (oauth2Success s provider) res
  where
    -- Add PKCE challenge to access token
    fetchAccessToken :: Manager -> OAuth2 -> B.ByteString -> ExchangeToken -> ExceptT OAuth2Failure (Handler b (AuthManager u e b)) OAuth2Token
    fetchAccessToken mgr oa pkceVerifier code
      | dpopBoundAccessTokens provider = do
        let (uri, body) = accessTokenUrl oa code
        req <- liftIO (HTTP.requestFromURI $ fromURIByteString uri)
        curr <- liftIO getTruncatedCurrentTime
        clientAssertion <- withExceptT (DPoPRequestError . JOSEError) $ getClientAssertion provider curr
        resp <- withExceptT DPoPRequestError $ dpopReq provider id HTTP.httpLbs $ req
          & HTTP.urlEncodedBody
          ([("code_verifier", pkceVerifier)
           ,("client_assertion", clientAssertion)
           ,("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
           , ("client_id", encodeUtf8 $ oauth2ClientId oa)
           ] <> body)
        withExceptT AccessTokenFetchError $ except $ parseResponseFlexible $ HTTP.responseBody resp
      | otherwise = withExceptT AccessTokenFetchError $
        let (uri, body) = accessTokenUrl oa code
        in doSimplePostRequest mgr oa uri (("code_verifier", pkceVerifier):body) >>=
           except . parseResponseFlexible

-- User has successfully completed OAuth2 login.  Get the stored
-- intended action and perform it.
oauth2Success
  :: IAuthBackend u i e b
  => OAuth2Settings p u i e b
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
  => OAuth2Settings p u i e b
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
        usr <- ExceptT $ (oauth2Login s) (userProvider s provider) token
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
  => OAuth2Settings p u i e b
  -> Provider
  -> Text
  -> Handler b (AuthManager u e b) (Either (Either e CreateFailure) i)
prepareOAuth2Create' s provider token =
  ((prepareOAuth2Create s) (userProvider s provider) token) >>=
  either checkDuplicate (return . Right)
  where
    checkDuplicate e = do
      isE <- isDuplicateError e
      return $ Left $ if isE then Right $ OAuth2Failure IdentityInUse else Left e

-- Check that stored action is not too old and that user matches
doResume
  :: IAuthBackend u i e b
  => OAuth2Settings p u i e b
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
      (oauth2Check s) (userProvider s provider) token
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
  => OAuth2Settings p u i e b
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
