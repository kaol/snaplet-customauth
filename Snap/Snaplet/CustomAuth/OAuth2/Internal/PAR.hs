{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Support for making requests to resource servers, including
-- handling of DPoP.

module Snap.Snaplet.CustomAuth.OAuth2.Internal.PAR where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.State
import Crypto.JWT hiding (jwk)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (toStrict)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8', encodeUtf8)
import Data.Time
import qualified Network.HTTP.Client as HTTP
import Network.OAuth.OAuth2
import qualified Network.URI
import Snap
import Unsafe.Coerce
import URI.ByteString

import Snap.Snaplet.CustomAuth.AuthManager
import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.CustomAuth.OAuth2.Internal.DPoP
import Snap.Snaplet.CustomAuth.Util

newtype RequestURI = RequestURI { reqURI :: Text }
  deriving (Show)

instance FromJSON RequestURI where
  parseJSON = withObject "RequestURI" $ \o -> RequestURI <$> o .: "request_uri"

getClientAssertion
  :: Provider
  -> UTCTime
  -> ExceptT JWTError (Handler b (AuthManager u e b)) ByteString
getClientAssertion provider curr = do
  maybeJWK <- lift $ gets jwk

  -- Tailored for bsky, may need generalisation for others
  let createClaims = do
        jti <- decodeLatin1 . Base64.encode <$> getRandomBytes 12
        let claims =
              emptyClaimsSet
              & claimAud ?~ Audience [aud]
              & claimIss ?~ sub
              & claimSub ?~ sub
              & claimJti ?~ jti
              & claimIat ?~ NumericDate curr
              & claimExp ?~ NumericDate (addUTCTime 90 curr)
        pure claims
      oa = oauth provider
      accessURI = oauth2AuthorizeEndpoint oa
      sub = fromJust $ preview stringOrUri $ oauth2ClientId oa
      aud = fromJust $ preview stringOrUri $ decodeLatin1 $
            schemeBS (uriScheme accessURI) <> "://" <>
            fromMaybe "" (hostBS . authorityHost <$> uriAuthority accessURI)

  claims <- liftIO createClaims

  let createAssertion = runJOSE $ do
        let j = fromJust maybeJWK
        hdr <- makeJWSHeader j
        encodeCompact <$> signClaims j hdr claims

  fmap toStrict $ ExceptT $ liftIO createAssertion

parRequest
  :: Provider
  -> UTCTime
  -> [(ByteString, ByteString)]
  -> URI
  -> ExceptT RedirectError (Handler b (AuthManager u e b)) (URIRef Absolute)
parRequest provider curr params parUrl = do
  let oa = oauth provider

  f <-
    if dpopBoundAccessTokens provider
    then do
      _ <- lift $ newDPoP provider
      pure $ dpopReq provider id
    else do
      mgr <- lift $ gets httpManager
      pure $ \f -> liftIO . flip f mgr

  clientAssertion <- withExceptT JOSEError $ getClientAssertion provider curr
  let parQuery =
        params <>
        [ ("client_assertion", clientAssertion)
        , ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
        ]

  req <- HTTP.urlEncodedBody parQuery <$> liftIO (HTTP.requestFromURI $ fromURIByteString parUrl)

  requestURI <-
    liftIO (HTTP.requestFromURI $ fromURIByteString parUrl) >>=
    f HTTP.httpLbs . HTTP.urlEncodedBody parQuery >>=
    maybe (throwE PARRequestURIParseError) (pure . reqURI) .
    decode . HTTP.responseBody

  let q = [ ("client_id", encodeUtf8 $ oauth2ClientId oa)
          , ("request_uri", encodeUtf8 requestURI)
          ]
  pure $ over (queryL . queryPairsL) (<> q) (oauth2AuthorizeEndpoint oa)
