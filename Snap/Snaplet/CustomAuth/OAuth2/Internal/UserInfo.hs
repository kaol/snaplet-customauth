{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.CustomAuth.OAuth2.Internal.UserInfo where

import Control.Applicative
import Control.Lens
import Control.Monad.Time
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Crypto.JOSE
import Crypto.JWT
import Data.Aeson (Value(..), eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Bifunctor
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import Network.OAuth.OAuth2

import Snap.Snaplet.CustomAuth.Types
import Snap.Snaplet.CustomAuth.OAuth2.Internal.DPoP
import Snap.Snaplet.CustomAuth.Util

getUserInfo
  :: MonadIO m
  => Manager
  -> Provider
  -> AccessToken
  -> ExceptT (Maybe Text) m Text
getUserInfo mgr provider token
  | Just jwksURI <- jwksEndpoint provider = do
      jwks <- withExceptT (Just . T.pack) $ ExceptT $ do
        liftIO $ eitherDecode . HTTP.responseBody <$>
          (HTTP.requestFromURI (fromURIByteString jwksURI) >>=
           flip HTTP.httpLbs mgr)
      claims <- withExceptT (Just . T.pack . show @JWTError) $ mapExceptT liftIO $
        ExceptT $ runJOSE $ do
        jwt <- (decodeCompact . BL.fromStrict . encodeUtf8 . atoken) token
        verifyClaims (defaultJWTValidationSettings (const True))
          (jwks :: JWKSet)
          (jwt :: SignedJWT)
      sub <- maybe (throwE $ Just "sub not found") pure $ claims ^. claimSub
      pure $ fromJust $ sub ^? string <|> (T.pack . show <$> sub ^? uri)
  | otherwise = do
      -- TODO PAR if needed
      let endpoint = identityEndpoint provider
      mapExceptT (fmap (first (Just . decodeUtf8Lenient . BL.toStrict)) . liftIO)
        (authGetJSON mgr token endpoint) >>=
        (maybe (throwE Nothing) pure . lookupProviderInfo)
  where
    lookup' a b = maybeText =<< M.lookup a b
    maybeText (String x) = Just x
    maybeText _ = Nothing
    lookupProviderInfo = lookup' (identityField provider)

