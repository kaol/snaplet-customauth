{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Snap.Snaplet.CustomAuth.Util where

import Control.Error.Util
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import qualified Network.URI
import Snap hiding (path)
import qualified URI.ByteString

import Snap.Snaplet.CustomAuth.AuthManager

getStateName
  :: Handler b (AuthManager u e b) Text
getStateName = do
  path <- maybe "auth" id . hush . decodeUtf8' <$> getSnapletRootURL
  name <- maybe "auth" id <$> getSnapletName
  return $ "__" <> name <> "_" <> path <> "_state"

getParamText
  :: forall (f :: Type -> Type).
     MonadSnap f
  => ByteString
  -> f (Maybe Text)
getParamText n = (hush . decodeUtf8' =<<) <$> getParam n

setFailure
  :: Handler b (AuthManager u e b) ()
  -> Maybe Text
  -> Either e (AuthFailure e)
  -> Handler b (AuthManager u e b) ()
setFailure action provider failure = do
  let failure' = either UserError id failure
  modify $ \s -> s { oauth2Provider = provider
                   , authFailData = Just failure'
                   }
  action

getTruncatedCurrentTime
  :: IO UTCTime
getTruncatedCurrentTime =
  (\t -> t { utctDayTime = fromInteger . floor . realToFrac $ utctDayTime t }) <$> getCurrentTime

fromURIByteString
  :: URI.ByteString.URI -> Network.URI.URI
fromURIByteString uri =
  Network.URI.URI
  { Network.URI.uriScheme = C8.unpack (URI.ByteString.schemeBS $ URI.ByteString.uriScheme uri)
                            <> ":"
  , Network.URI.uriAuthority =
      (\a -> Network.URI.URIAuth
             { Network.URI.uriUserInfo = "" -- TODO?
             , Network.URI.uriRegName = C8.unpack $ URI.ByteString.hostBS $
                                        URI.ByteString.authorityHost a
             , Network.URI.uriPort = "" -- TODO?
             }
      )
      <$> URI.ByteString.uriAuthority uri
  , Network.URI.uriPath = C8.unpack $ URI.ByteString.uriPath uri
  , Network.URI.uriQuery = C8.unpack $ URI.ByteString.serializeQuery'
                           URI.ByteString.httpNormalization $
                           URI.ByteString.uriQuery uri
  , Network.URI.uriFragment = C8.unpack . URI.ByteString.serializeFragment' $
                          URI.ByteString.uriFragment uri
  }
