{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Piperka.OAuth2.Types where

import Data.Binary
import Data.Int
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

data Provider = Reddit | Google | Github
  deriving (Show, Eq, Generic)

instance Binary Provider

type AuthID = Int32

providerOpid
  :: Provider
  -> AuthID
providerOpid Reddit = 2
providerOpid Google = 3
providerOpid Github = 4

opidProvider
  :: AuthID
  -> Provider
opidProvider 2 = Reddit
opidProvider 3 = Google
opidProvider 4 = Github
opidProvider _ = error "invalid opid"

parseProvider
  :: (Eq s, IsString s)
  => s
  -> Maybe Provider
parseProvider "reddit" = Just Reddit
parseProvider "Reddit" = Just Reddit
parseProvider "google" = Just Google
parseProvider "Google" = Just Google
parseProvider "github" = Just Github
parseProvider "Github" = Just Github
parseProvider _ = Nothing

providerName
  :: Provider
  -> Text
providerName Reddit = "reddit"
providerName Google = "google"
providerName Github = "github"
