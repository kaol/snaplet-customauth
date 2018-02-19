{-# LANGUAGE OverloadedStrings #-}

module Piperka.Recover
  (
    renderPasswordRecovery
  , renderUsePasswordHash
  ) where

import Contravariant.Extras.Contrazip
import Control.Error.Util (hush)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Map.Syntax
import Data.Monoid
import qualified Data.Text.Lazy as L
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session (query)
import Heist.Compiled
import Network.Mail.SMTP (Address(..), sendMail, simpleMail, plainTextPart)
import Network.HTTP.Types.URI (urlEncode)
import Snap hiding (urlEncode)
import Snap.Snaplet.Hasql

import Application
import Piperka.Error.Splices
import Piperka.Util (randomString, getParamText)


encoder :: EN.Params (Text, Text, Text)
encoder = contrazip3 (EN.value EN.text) (EN.value EN.text) (EN.value EN.text)

decoder :: DE.Row Bool
decoder = DE.value DE.bool

renderPasswordRecovery :: RuntimeAppHandler a
renderPasswordRecovery _ = do
  [bodyStart, bodyEnd] <- liftIO $ C.split '$' <$> B.readFile "x/recovery_template"
  let mkBody usr email = decodeLatin1 $
        bodyStart <> (urlEncode True $ encodeUtf8 usr) <>
        "&hash=" <> (urlEncode False $ encodeUtf8 email) <> bodyEnd
  let splices = do
        "success" ## \n -> do
          x <- runChildren
          flip bindLater n $ \val -> do
            if val then codeGen x else return mempty
        "otherwise" ## \n -> do
          x <- runChildren
          flip bindLater n $ \val -> do
            if val then return mempty else codeGen x
  withSplices runChildren splices `defer` do
    params <- lift $ runMaybeT $ do
      name <- MaybeT $ getParamText "user_name"
      email <- MaybeT $ getParamText "email"
      act <- MaybeT $ getParam "action"
      return (act == "Send me a recovery link", name, email)
    case params of
      Just (True, name, email) -> do
        rand <- liftIO $ randomString 32
        success <- lift $ withTop db $ run $ query (name, email, rand) smt
        case success of
          Right True -> do
            liftIO $ sendMail "localhost" $
              simpleMail (Address Nothing "piperka@piperka.net")
              [Address Nothing email] [] []
              "Your new password for piperka.net"
              [plainTextPart $ L.fromStrict $ mkBody name rand]
          Right False -> return ()
          Left x -> lift $ logError $ "error creating password recovery hash: " <>
                    (C.pack $ show x)
        return True
      _ -> return False
  where
    smt = statement "select * from create_recovery_key($1, $2, $3)"
          encoder (DE.singleRow decoder) True

renderUsePasswordHash :: RuntimeAppHandler a
renderUsePasswordHash _ = do
  let maybeSqlErr = "maybeSqlErr" ##
        deferMany (withSplices (callTemplate "_sqlErr") sqlErrorSplices)
      splices = do
        "success" ##
          deferMany (withSplices runChildren
                     ("password" ## return . yieldRuntimeText)) . fmap hush
        "otherwise" ##
          deferMany (withSplices runChildren maybeSqlErr) .
          fmap (either Just (const $ Nothing))

  withSplices runChildren splices `defer` do
    params <- lift $ runMaybeT $ do
      name <- MaybeT $ getParamText "lost"
      key <- MaybeT $ getParamText "hash"
      return (name, key)
    case params of
      Just (name, key) -> do
        password <- liftIO $ randomString 8
        success <- lift $ withTop db $ run $ query (name, key, password) smt
        return $ case success of
          Right True -> Right password
          Right False -> Left Nothing
          Left x -> Left $ Just x
      Nothing -> return $ Left Nothing
  where
    smt = statement "select * from reset_user_password($1, $2, $3)"
          encoder (DE.singleRow decoder) True
