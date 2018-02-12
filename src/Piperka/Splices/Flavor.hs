{-# LANGUAGE OverloadedStrings #-}

module Piperka.Splices.Flavor where

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Error.Util (hush)
import Control.Monad.Trans
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Hasql.Session (query)
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query
import Heist.Compiled
import Snap
import Snap.Snaplet.Hasql
import System.Exit (ExitCode(..))
import System.Process.ByteString (readProcessWithExitCode)

import Application

renderKaolSubs
  :: Splice AppHandler
renderKaolSubs = return $ yieldRuntimeText $ do
    num <- lift $ withTop db $ do
      num <- run $ query () getKaolSubs
      return $ T.pack $ either (const "") show num
    return num
  where
    getKaolSubs = statement sql EN.unit decode True
    decode = DE.singleRow $ DE.value DE.int8
    sql = "select count(*) from subscriptions join comics using (cid) where uid=3"

renderFortune
  :: Splice AppHandler
renderFortune = do
  err <- runChildren
  return $ yieldRuntime $ do
    (ex, sOut, _) <- liftIO $ readProcessWithExitCode "/usr/games/fortune" [] ""
    case ex of
      ExitSuccess ->
        maybe (codeGen err) (return . fromText) $ hush $ decodeUtf8' sOut
      _ -> codeGen err
