{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Snap.Snaplet.CustomAuth.OAuth2.Internal.DPoP where

import Control.Lens
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Crypto.JOSE.Header
import Crypto.JOSE.JWK
import Crypto.JOSE.JWA.JWS (Alg(ES256))
import Crypto.JWT
import Data.Aeson
import qualified Data.Aeson.KeyMap as M
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, decodeUtf8, encodeUtf8)
import Data.Text.Strict.Lens
import Data.Time
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
import Network.HTTP.Types.Method
import Network.OAuth.OAuth2
import URI.ByteString
import Snap hiding (Method)
import Snap.Snaplet.Session

import Snap.Snaplet.CustomAuth.AuthManager hiding (jwk)
import Snap.Snaplet.CustomAuth.Challenge
import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.CustomAuth.Util

data DPoPJWT = DPoPJWT
  { jwtClaims :: ClaimsSet
  , claimHtm :: Text
  , claimHtu :: Text
  , claimNonce :: Maybe Text
  -- | PDS endpoints only
  , claimAth :: Maybe Text
  }

instance HasClaimsSet DPoPJWT where
  claimsSet f s = fmap (\a' -> s { jwtClaims = a'}) (f (jwtClaims s))

instance FromJSON DPoPJWT where
  parseJSON = withObject "DPoP" $ \o ->
    DPoPJWT
    <$> parseJSON (Object o)
    <*> o .: "htm"
    <*> o .: "htu"
    <*> o .:? "nonce"
    <*> o .:? "ath"

instance ToJSON DPoPJWT where
  toJSON s =
    Object $ M.union
    (M.fromList $ (maybe id (\x -> (("nonce",String x):)) $ claimNonce s)
     [ ("htm", String $ claimHtm s)
     , ("htu", String $ claimHtu s)
     ])
    ((\(Object o) -> o) $ toJSON $ jwtClaims s)

dpopKeys :: MonadRandom m => m JWK
dpopKeys = do
  jwk <- genJWK (ECGenParam P_256)
  let h = view thumbprint jwk :: Digest SHA256
      kid = view (re (base64url . digest) . utf8) h
  pure $ set jwkKid (Just kid) jwk

dpopClaims :: MonadRandom m => UTCTime -> Method -> URI -> Maybe Text -> m DPoPJWT
dpopClaims t method uri nonce = do
  jti <- decodeLatin1 . Base64.encode <$> getRandomBytes 12
  pure $ DPoPJWT
    { jwtClaims = emptyClaimsSet
                  & claimJti ?~ jti
                  & claimIat ?~ NumericDate t
    , claimHtm = decodeLatin1 method
    , claimHtu = decodeUtf8 $ normalizeURIRef' httpNormalization uri
    , claimNonce = nonce
    , claimAth = Nothing
    }

dpopJWTSign :: (MonadRandom m, MonadError e m, AsError e) => JWK -> DPoPJWT -> m SignedJWT
dpopJWTSign key dpop = do
  let hdr = newJWSHeader ((), ES256)
        & typ ?~ HeaderParam () "dpop+jwt"
        & jwk ?~ (HeaderParam () $ fromJust $ view asPublicKey key)
  signJWT key hdr dpop

-- | Generate DPoP JWK
newDPoP
  :: Provider
  -> Handler b (AuthManager u e b) ()
newDPoP provider = do
  ss <- gets stateStore'
  let oa = oauth provider
  --name <- (("_" <> providerName provider) <>) <$> getStateName
  name <- getStateName
  k <- liftIO dpopKeys
  withTop' ss $ setInSession (name <> "_dpop_jwk") $ decodeLatin1 $ BL.toStrict $ encode k

recoverDPoPKey
  :: Provider
  -> Handler b (AuthManager u e b) (Maybe JWK)
recoverDPoPKey provider = do
  ss <- gets stateStore'
  name <- getStateName
  raw <- withTop' ss (getFromSession (name <> "_dpop_jwk"))
  pure $ maybe Nothing (decode . BL.fromStrict . encodeUtf8) raw

newtype ResponseBodyError = ResponseBodyError { respError :: Text }
  deriving (Show, Eq)

instance FromJSON ResponseBodyError where
  parseJSON = withObject "Response" $ \v -> ResponseBodyError <$> v .: "error"

dpopReq
  :: Provider
  -> (a -> BL.ByteString)
  -> (HTTP.Request -> HTTP.Manager -> IO (HTTP.Response a))
  -> HTTP.Request
  -> ExceptT RedirectError (Handler b (AuthManager u e b)) (HTTP.Response a)
dpopReq provider toLBS reqf req = do
  (ss, mgr) <- lift $ gets ((,) <$> stateStore' <*> httpManager)
  name <- lift getStateName
  let nonceName = name <> "_dpop_nonce"
  t <- liftIO getTruncatedCurrentTime
  storedNonce <- lift $ withTop' ss $ getFromSession nonceName
  key <- maybe (throwE DPoPKeyMissing) pure =<< lift (recoverDPoPKey provider)
  let htu = URI
        (Scheme $ if HTTP.secure req then "https" else "http")
        (Just $ Authority Nothing (Host $ HTTP.host req) (Just $ Port $ HTTP.port req))
        (HTTP.path req)
        (Query []) Nothing
      dpopReq' maybeNonce = do
        -- dpopClaims t (HTTP.method req) htu maybeNonce
        let createClaims = runJOSE $
              dpopClaims t (HTTP.method req) htu maybeNonce
        claims <- liftIO createClaims >>= either
          (throwE . JOSEError) pure
        let createJWT = runJOSE $
              dpopJWTSign key claims
        jwt <- liftIO createJWT >>= either
          (throwE . JOSEError) pure
        let req' = req
              { HTTP.requestHeaders =
                ("dpop", BL.toStrict $ encodeCompact jwt):
                (filter ((/= "dpop") . fst) (HTTP.requestHeaders req))
              }
        liftIO (reqf req' mgr) >>=
          (\case
              r | Just dpopNonce <- getHeaderValue "dpop-nonce" r -> do
                let n = decodeUtf8 dpopNonce
                lift $ withTop' ss $ do
                  setInSession nonceName n
                  commitSession
                pure (r, Just n)
              r -> pure (r, Nothing)
          ) >>= \case
          (r, n@(Just _)) | nonceRequired r -> do
            dpopReq' n
          (r, x) -> do
            let maybeChallenge =
                  find ((== "dpop") . challengeScheme) .
                  parseWWWAuthenticateChallenge =<<
                  getHeaderValue "www-authenticate" r
            pure r

  dpopReq' storedNonce
  where
    getHeaderValue n = lookup n . HTTP.responseHeaders

    nonceRequired r =
      let s = HTTP.responseStatus r
      in or
         [ isJust $ do
             challenge <-
               find ((== "dpop") . challengeScheme) . parseWWWAuthenticateChallenge =<<
               getHeaderValue "www-authenticate" r
             authError <- lookup "error" $ challengeParams challenge
             guard $
               statusCode s == 401 &&
               challengeScheme challenge == "DPoP" &&
               authError == "use_dpop_nonce"
         , isJust $ do
             contentType <- getHeaderValue "content-type" r
             (ResponseBodyError authError) <-
               guard (contentType == "application/json") *> (decode $ toLBS $ HTTP.responseBody r)
             guard $
               statusCode s == 400 &&
               authError == "use_dpop_nonce"
         ]
