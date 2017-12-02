{-# LANGUAGE OverloadedStrings #-}

module Piperka.Keys.Dummy where

import Data.Text (Text)
import Snap.Snaplet.CustomAuth.OAuth2 (Provider(..))

getKey :: Provider -> (Text, Text)
getKey Google = ("", "")
getKey Reddit = ("", "")
