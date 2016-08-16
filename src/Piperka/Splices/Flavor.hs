{-# LANGUAGE OverloadedStrings #-}
module Piperka.Splices.Flavor where

import Heist
import qualified Heist.Compiled as C
import qualified Data.Text as T
import Application
import Snap
import Snap.Snaplet.Hasql
import Hasql.Query
import qualified Hasql.Encoders as EN
import qualified Hasql.Decoders as DE
import Hasql.Session (query)
import Control.Monad.Trans

renderKaolSubs
  :: RuntimeSplice (Handler App App) UserPrefs
  -> C.Splice (Handler App App)
renderKaolSubs _ = do
  return $ C.yieldRuntimeText $ do
    num <- lift $ withTop db $ do
      num <- run $ query () getKaolSubs
      return $ T.pack $ either (const "") show num
    return num
  where
    getKaolSubs = statement sql EN.unit decode True
    decode = DE.singleRow $ DE.value DE.int8
    sql = "select count(*) from subscriptions join comics using (cid) where uid=3"
