{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wno-unused-top-binds #-}

module Piperka.API.Common
  (
    runQueries
  , runUserQueries
  , runModQueries
  , simpleFail
  , validateCsrf
  , UserQueryHandler
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import GHC.Generics
import Hasql.Session (Error)
import Network.IP.Addr
import Snap
import Snap.Snaplet.CustomAuth
import Snap.Snaplet.Hasql

import Application
import Backend ()
import Piperka.Util (getParamText)

type UserQueryHandler a = ExceptT Error (Handler App Hasql) a

data Fail = Errmsg { errmsg :: Text } deriving (Generic)
instance ToJSON Fail where
  toEncoding = genericToEncoding defaultOptions

failWithMsg
  :: Show a
  => a
  -> AppHandler b
failWithMsg msg = do
  modifyResponse $ setResponseStatus 500 "Internal Server Error"
  writeLBS $ encode $ Errmsg . ("Something went wrong: " <>) $
    T.pack $ show msg
  finishWith =<< getResponse

runQueries
  :: ExceptT Error (Handler App Hasql) a
  -> AppHandler a
runQueries actions = do
  modifyResponse (setHeader "Content-Type" "application/json")
  either failWithMsg return =<< withTop db (runExceptT actions)

runUserQueries
  :: (UserPrefs -> ExceptT Error (Handler App Hasql) a)
  -> AppHandler a
runUserQueries actions = do
  modifyResponse (setHeader "Content-Type" "application/json")
  either failWithMsg return =<<
    (withTop db $
      do
        withTop apiAuth recoverSession
        maybe (simpleFail 403 "User authentication failed") (runExceptT . actions) =<<
          (withTop apiAuth currentUser)
    )

runModQueries
  :: (UserPrefs -> ExceptT Error (Handler App Hasql) a)
  -> AppHandler a
runModQueries actions = runUserQueries $ \u ->
  if (moderator <$> user u) == Just True
  then actions u
  else lift $ simpleFail 403 "Moderator only"

validateCsrf
  :: Handler App v ()
validateCsrf = do
  u <- maybe (simpleFail 403 "User authentication failed") return =<<
       withTop apiAuth currentUser
  maybe (simpleFail 403 "CSRF token missing")
    (\csrf -> if Just csrf == (ucsrfToken <$> user u)
              then return ()
              else simpleFail 403 "CSRF validation failed") =<<
    ((Data.UUID.fromText =<<) <$> getParamText "csrf_ham")

simpleFail
  :: Int
  -> ByteString
  -> Handler b v a
simpleFail status msg = do
  modifyResponse $ setResponseStatus status msg
  finishWith =<< getResponse
  undefined

instance ToJSON (NetAddr IP) where
  toJSON = String . T.pack . show
