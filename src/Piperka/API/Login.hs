{-# LANGUAGE OverloadedStrings #-}

module Piperka.API.Login (apiLogin, tokenLogin) where

import Contravariant.Extras.Contrazip
import Control.Monad.Trans
import Data.Aeson
import Data.ByteString.UTF8 (fromString)
import Data.Text (Text)
import Data.Time.Clock
import Data.UUID (UUID, toASCIIBytes, toText)
import Hasql.Query
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Session (Error, query)
import Snap
import Snap.Snaplet.Hasql

import Application
import Piperka.API.Common
import Piperka.Auth (setCSRFCookie)
import Piperka.Util (getParamText)

login
  :: Text
  -> Text
  -> AppHandler (Either Error (Maybe (Text, UUID, UUID)))
login u p = run $ query (u, p) $ statement sql
            (contrazip2 (EN.value EN.text) (EN.value EN.text))
            (DE.maybeRow $ (,,)
             <$> (DE.value DE.text)
             <*> (DE.value DE.uuid)
             <*> (DE.value DE.uuid)) True
  where
    sql = "WITH lg AS (\
          \SELECT name, p_session, csrf_ham FROM auth_login($1, $2) \
          \JOIN users USING (uid)) \
          \, upd AS (INSERT INTO appuser_token SELECT csrf_ham FROM lg) \
          \SELECT name, p_session, csrf_ham FROM lg"

tokenLogin
  :: UUID
  -> AppHandler (Either Error (Maybe UserID))
tokenLogin u = run $ query u $ statement sql (EN.value EN.uuid)
               (DE.singleRow $ DE.nullableValue DE.int4) True
  where
    sql = "SELECT token_login($1)"

apiLogin
  :: AppHandler ()
apiLogin = do
  usr <- requiredParam "user" getParamText
  passwd <- requiredParam "passwd" getParamText
  lg <- either (simpleFail 403 . fromString . show) return =<<
    login usr passwd
  case lg of
    Nothing ->
      writeLBS $ encode $ object ["errmsg" .= ("Login failed" :: Text)]
    Just (name, sesTok, csrfTok) -> do
      now <- liftIO getCurrentTime
      modifyResponse $ addResponseCookie $
        Cookie sessionCookieName (toASCIIBytes sesTok)
        (Just $ addUTCTime (5*365*24*60*60) now) Nothing Nothing False True
      setCSRFCookie csrfTok
      writeLBS $ encode $ object ["name" .= name
                                 , "csrf_ham" .= toText csrfTok]
