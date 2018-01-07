{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Snap.Snaplet.CustomAuth.OAuth2.Internal
  ( oauth2Init
  , attachProvider
  , saveAction
  , redirectToProvider
  , getOAuth2Provider
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
import Data.Binary.Orphans ()
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Char (chr)
import qualified Data.Configurator
import Data.HashMap.Lazy (HashMap, lookup)
import Data.Maybe (isJust)
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
import URI.ByteString.QQ

import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.Types hiding (name)
import Snap.Snaplet.CustomAuth.User (setUser, currentUser, recoverSession)
import Snap.Snaplet.CustomAuth.Util (getStateName, getParamText, setFailure)

oauth2Init
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Initializer b (AuthManager u e b) ()
oauth2Init s = do
  addRoutes $ mapped._2 %~ (bracket s) $
    [ ("oauth2createaccount", oauth2CreateAccount s)
    , ("oauth2callback/:provider", oauth2Callback s)
    , ("oauth2login/:provider", redirectLogin s)
    ]

getProvider
  :: Handler b (AuthManager u e b) (Maybe Provider)
getProvider = (parseProvider =<<) <$> getParam "provider"

redirectLogin
  :: OAuth2Settings u i e b
  -> Handler b (AuthManager u e b) ()
redirectLogin s = do
  provider <- ((\x -> do
                   p <- parseProvider x
                   guard $ p `elem` (enabledProviders s)
                   return p) =<<) <$>
              getParam "provider"
  maybe pass toProvider provider
  where
    toProvider p = do
      success <- redirectToProvider p
      if success then return () else pass

getRedirUrl
  :: Provider
  -> Text
  -> OAuth2
  -> URI
getRedirUrl p token o =
  appendQueryParams (("state", encodeUtf8 token):scope) $ authorizationUrl o
  where
    scope =  [("scope", scopeLoc)]
    scopeLoc = case p of
      Google -> "profile"
      Reddit -> "identity"

redirectToProvider
  :: Provider
  -> Handler b (AuthManager u e b) Bool
redirectToProvider provider = do
  param <- buildOAuth2Param provider
  maybe (return False) (\x -> redirectToProvider' provider x >> return True) param

redirectToProvider'
  :: Provider
  -> OAuth2
  -> Handler b (AuthManager u e b) ()
redirectToProvider' provider param = do
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
  let redirUrl = serializeURIRef' $ getRedirUrl provider token param
  redirect' redirUrl 303

buildOAuth2Param
  :: Provider
  -> Handler b (AuthManager u e b) (Maybe OAuth2)
buildOAuth2Param provider = do
  cfg <- getSnapletUserConfig
  root <- getSnapletRootURL
  liftIO $ runMaybeT $ do
    let providerName = T.toLower $ T.pack $ show provider
        lookupProvider x = MaybeT $ Data.Configurator.lookup cfg (providerName <> x)
    hostname <- MaybeT $ Data.Configurator.lookup cfg "hostname"
    k <- lookupProvider ".key"
    s <- lookupProvider ".secret"
    let callback = URI (Scheme "http")
                   (Just $ Authority Nothing (Host hostname) Nothing)
                   ("/" <> root <> "/oauth2callback/" <> (C8.pack $ show provider)) mempty Nothing
        (endpoint1, endpoint2, _) = endpoints provider
    return $ OAuth2
      { oauthClientId = k
      , oauthClientSecret = s
      , oauthCallback = Just callback
      , oauthOAuthorizeEndpoint = endpoint1
      , oauthAccessTokenEndpoint = endpoint2
      }

endpoints
  :: Provider
  -> (URI, URI, URI)
endpoints Google = ( [uri|https://accounts.google.com/o/oauth2/auth|]
                   , [uri|https://www.googleapis.com/oauth2/v4/token|]
                   , [uri|https://www.googleapis.com/oauth2/v3/userinfo|])
endpoints Reddit = ( [uri|https://www.reddit.com/api/v1/authorize|]
                   , [uri|https://www.reddit.com/api/v1/access_token|]
                   , [uri|https://oauth.reddit.com/api/v1/me|])

getUserInfo
  :: OAuth2Settings u i e b
  -> Provider
  -> AccessToken
  -> Handler b (AuthManager u e b) (Maybe Text)
getUserInfo s provider token = do
  let endpoint = (\(_, _, x) -> x) $ endpoints provider
  let mgr = httpManager s
  liftIO $ runMaybeT $ do
    dat <- MaybeT $ hush <$> authGetJSON' mgr token endpoint
    MaybeT . return $ lookupProviderInfo provider dat
  where
    authGetJSON' :: Manager -> AccessToken -> URI
                 -> IO (OAuth2Result (HashMap Text Value) (HashMap Text Value))
    authGetJSON' = authGetJSON
    lookup' a b = maybeText =<< lookup a b
    maybeText (String x) = Just x
    maybeText _ = Nothing
    lookupProviderInfo Google dat = lookup' "sub" dat
    lookupProviderInfo Reddit dat = lookup' "name" dat

oauth2Callback
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Handler b (AuthManager u e b) ()
oauth2Callback s = getProvider >>= maybe pass (oauth2Callback' s)

oauth2Callback'
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Provider
  -> Handler b (AuthManager u e b) ()
oauth2Callback' s provider = do
  when (not $ provider `elem` (enabledProviders s)) pass
  name <- getStateName
  let ss = stateStore s
      mgr = httpManager s
  res <- runExceptT $ do
    param <- noteT ConfigurationError $ MaybeT $ buildOAuth2Param provider
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
    (maybe (throwE IdExtractionFailed) return =<<) $ runMaybeT $ do
      code <- MaybeT $ (fmap ExchangeToken) <$> (lift $ getParamText "code")
      -- TODO: catch?
      token <- either (const $ lift $ throwE AccessTokenFetchError) return =<< liftIO
        (fetchAccessToken mgr param code)
      -- TODO: get user id (sub) from idToken in token, if
      -- available. Requires JWT handling.
      MaybeT $ lift $ getUserInfo s provider (accessToken token)
  either (setFailure ((oauth2Failure s) SCallback) (Just provider) .
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
  key <- getActionKey provider
  store <- gets stateStore'
  name <- getStateName
  act <- withTop' store $ runMaybeT $ do
    act <- MaybeT $ getFromSession key
    lift $ deleteFromSession key >> commitSession
    return act
  withTop' store $ do
    setInSession (name <> "_provider") (T.pack $ show provider)
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
    maybe proceed (const $ setFailure ((oauth2Failure s) SLogin) (Just provider) $
                   Right $ Create $ OAuth2Failure AlreadyLoggedIn)
  where
    proceed = do
      res <- runExceptT $ do
        usr <- ExceptT $ (oauth2Login s) provider token
        maybe (return ()) (lift . setUser) usr
        return usr
      either (setFailure ((oauth2Failure s) SLogin) (Just provider) . Left)
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
  (prepareOAuth2Create s) provider token >>=
  either checkDuplicate (return . Right)
  where
    checkDuplicate e = do
      isE <- isDuplicateError e
      return $ Left $ if isE then Right $ OAuth2Failure AlreadyAttached else Left e

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
    u <- ExceptT $ return . either (Left . Left) Right =<<
      (oauth2Check s) provider token
    -- Compare current user with action's stored user
    when (userId /= actionUser d') $
     throwE (Right ActionUserMismatch)
    -- Compare current user with identity owner
    when (maybe True ((/= userId) . Just) u) $
      throwE (Right ActionUserMismatch)
    expired <- liftIO $ isExpiredStamp (actionStamp d')
    when expired $ throwE (Right ActionTimeout)
    return $ savedAction d'
  let
    attach = maybe (setFailure ((oauth2Failure s) SAttach) (Just provider)
                    (Right (Create $ OAuth2Failure AttachNotLoggedIn))) attach' user
    attach' u = do
      usr <- prepareOAuth2Create' s provider token
      res' <- runExceptT $ do
        i' <- hoistEither usr
        ExceptT $ return . either (Left . Left) Right =<< attachLoginMethod u i'
      case (usr, res') of
        (Right i, Left _) -> cancelPrepare i
        _ -> return ()
      either (setFailure ((oauth2Failure s) SAttach) (Just provider) . fmap Create)
        (const $ return ()) res'
  either (setFailure ((oauth2Failure s) SAction) (Just provider) . fmap Action)
    (maybe attach ((resumeAction s) provider token)) res

-- User has successfully signed in via oauth2 and the provider/token
-- did not match with an existing user.  This is the endpoint for
-- requesting account creation afterwards.
oauth2CreateAccount
  :: IAuthBackend u i e b
  => OAuth2Settings u i e b
  -> Handler b (AuthManager u e b) ()
oauth2CreateAccount s = do
  store <- gets stateStore'
  usrName <- ((hush . decodeUtf8') =<<) <$>
    (getParam =<< ("_new" <>) <$> gets userField)
  name <- getStateName
  provider <- withTop' store $
    (parseProvider =<<) <$> getFromSession (name <> "_provider")
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
  either (setFailure ((oauth2Failure s) SCreate) provider . fmap Create)
    (oauth2AccountCreated s) res

getActionKey
  :: Provider
  -> Handler b (AuthManager u e b) Text
getActionKey p = do
  path <- maybe "auth" id . hush . decodeUtf8' <$> getSnapletRootURL
  name <- maybe "auth" id <$> getSnapletName
  return $ "__" <> name <> "_" <> path <> "_action_" <> (T.pack $ show p)

attachProvider
  :: IAuthBackend u i e b
  => Provider
  -> Handler b (AuthManager u e b) ()
attachProvider p = saveAction' p Nothing

saveAction
  :: (IAuthBackend u i e b, Binary a)
  => Provider
  -> a
  -> Handler b (AuthManager u e b) ()
saveAction p d = saveAction' p $ Just $ toStrict $ Data.Binary.encode d

saveAction'
  :: IAuthBackend u i e b
  => Provider
  -> Maybe ByteString
  -> Handler b (AuthManager u e b) ()
saveAction' provider d = do
  key <- getActionKey provider
  store <- gets $ stateStore'
  stamp <- liftIO $ getCurrentTime
  i <- runMaybeT $ lift . getUserId =<< MaybeT currentUser
  let payload = SavedAction {
          actionProvider = provider
        , actionStamp = stamp
        , actionUser = i
        , savedAction = d
        }
  let d' = decodeLatin1 $ Data.ByteString.Base64.encode $
        toStrict . Data.Binary.encode $ payload
  withTop' store $ do
    setInSession key d'
    commitSession

getOAuth2Provider
  :: Handler b (AuthManager u e b) (Maybe Provider)
getOAuth2Provider = get >>= return . oauth2Provider
