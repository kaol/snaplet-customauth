{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import Control.Concurrent.ParallelIO.Local
import Control.Monad.Trans
import           Data.ByteString (ByteString)
--import           Data.Monoid
--import qualified Data.Text as T
--import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist.Compiled
import Snap.Snaplet.Session.Backends.CookieSession
--import           Snap.Util.FileServe
import           Heist
--import qualified Heist.Interpreted as I
--import qualified Heist.Compiled as C
------------------------------------------------------------------------------
import           Application
--import Backend
import           Snap.Snaplet.CustomAuth
import           Snap.Snaplet.Hasql
import           Piperka.Splices
import Control.Lens
import Piperka.ComicInfo.Tag
import Piperka.ComicInfo.External

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes =
  [
  ]

{-
staticRoutes :: [(ByteString, Handler App App ())]
staticRoutes =
  [ ("",          serveDirectory "static")
  ]
-}


data ParLabels a b c = L1 a | L2 b | L3 c

app :: SnapletInit App App
app = makeSnaplet "piperka" "Piperka application." Nothing $ do
  [~(L1 elookup), ~(L2 tlookup), ~(L3 tfp), ~(L3 efp)] <- liftIO $
    (map $ either error id) <$>
    withPool 4 (flip parallel
                [ (fmap L1) <$> generateExternal
                , (fmap L2) <$> generateTag
                , (fmap L3) <$> generateTagFormPart
                , (fmap L3) <$> generateExternalFormPart
                ])
  let initData = AppInit efp tfp
  a <- nestSnaplet "" auth $ authInit "_session" "_login" "_password"
  a' <- nestSnaplet "" apiAuth $ authInit "_session" "_login" "_password"
  m <- nestSnaplet "messages" messages $
       initCookieSessionManager "site_key.txt" "messages" Nothing (Just 3600)
  h <- nestSnaplet "" heist $ heistInit' "templates" $
       emptyHeistConfig
       & hcLoadTimeSplices .~ defaultLoadTimeSplices
       & hcNamespace .~ "h"
       & hcCompiledSplices .~ (piperkaSplices initData)
       & hcTemplateLocations .~ [loadTemplates "templates"]
  d <- nestSnaplet "" db $ hasqlInit "postgresql://kaol@/piperka"
  addRoutes routes
-- TODO: This opens DB handles for things that never need it like
-- static content.  Cull the uses.
  wrapSite (<|> bracketDbOpen heistServe)
  return $ App h a a' d m elookup tlookup
