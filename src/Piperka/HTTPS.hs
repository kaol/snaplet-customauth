{-# LANGUAGE OverloadedStrings #-}

module Piperka.HTTPS (sslRedirectHandler) where

import Data.ByteString (ByteString)
import Data.Monoid
import Snap hiding (path)

import Application

data Protocol = HTTP | HTTPS
  deriving (Show, Eq)

sslRedirectHandler
  :: ByteString
  -> AppHandler ()
sslRedirectHandler hostname = do
  rq <- getRequest
  let originalPort = getHeader "X-Original-Port" rq
      protocol = case originalPort of
        Just "80" -> Just HTTP
        Just "443" -> Just HTTPS
        _ -> Nothing
      path = rqPathInfo rq
      isReader = path ` elem` ["reader", "reader/"]
      -- Use cache-control for now to verify that it works
      redir scheme =
        (modifyResponse $ addHeader "Cache-Control" "max-age=3600") >>
        redirect' (scheme <> hostname <> rqURI rq) 301
  case (protocol, isReader) of
    (Just HTTPS, True) -> redir "http://"
    (Just HTTP, False) -> redir "https://"
    _ -> return ()
