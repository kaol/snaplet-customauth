module Piperka.API.OAuth2.Types where

import Snap.Snaplet.CustomAuth.Types (Provider(..))

import Application

providerOpid
  :: Provider
  -> AuthID
providerOpid Reddit = 2
providerOpid Google = 3

opidProvider
  :: AuthID
  -> Provider
opidProvider 2 = Reddit
opidProvider 3 = Google
opidProvider _ = error "invalid opid"
