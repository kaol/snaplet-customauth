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
import Control.Concurrent.ParallelIO.Local
import Control.Monad.Trans
import           Data.ByteString (ByteString)
--import           Data.Monoid
--import qualified Data.Text as T
--import           Snap.Core
import Network.HTTP.Client.TLS
import           Snap.Snaplet
import Snap.Snaplet.Heist
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
import Data.ByteString.UTF8 (fromString)
import qualified Data.Configurator
import Data.Monoid

import System.Directory (listDirectory)
import Piperka.Auth (authHandler, mayCreateAccount)
import Piperka.ComicInfo.Tag
import Piperka.ComicInfo.External
import Piperka.API
import Piperka.Account
import Piperka.OAuth2

------------------------------------------------------------------------------
-- | The application's routes.
routes :: IO [(ByteString, Handler App App ())]
routes = do
  let apiRoutes =
        [ ("/s/cinfo/:cid", comicInfo)
        , ("/s/qsearch", quickSearch)
        , ("/s/tagslist/:tagid", tagList)
        , ("/s/uprefs", userPrefs)
        , ("/s/archive/:cid", dumpArchive)
        , ("/s/profile", profileSubmission)
        , ("/s/attachProvider/:provider", attachProvider)
        , ("newuser", mayCreateAccount (return ()))
        -- Moderator interface
        , ("/s/sinfo/:sid", readSubmit)
        , ("/s/sinfo2/:sid", readSubmit)
        , ("/s/dropsubmit/:sid", dropSubmit)
        , ("/s/viewsubmitbanner/:sid", viewSubmitBanner)
        ]
  let specialTemplates = ["account.html", "newuser.html", "cinfo"]
  templateRoutes <-
    (map fromString .
     filter (not . (`elem` specialTemplates)) .
     filter ((/= "_") . take 1) .
     filter ((/= "_") . take 1 . reverse) .
     map (reverse . drop 4 . reverse) .
     filter ((== ".tpl") . reverse . take 4 . reverse) .
     filter ((> 4) . length)
    ) <$> listDirectory "snaplets/heist/templates"
  return $ mapped._2 %~ bracketDbOpen $ apiRoutes <>
    (map (\x -> (x, authHandler False $ cRender x)) templateRoutes) <>
    [ ("account.html", authHandler False $
        accountUpdateHandler >> cRender "account.html")
    , ("newuser.html", authHandler False $
        mayCreateAccount $ cRender "newuser.html")
    , ("cinfo", authHandler True $ cRender "cinfo")
    ]

{-
templateRoutes :: SnapletInit App [(ByteString, Handler App App ())]
templateRoutes = do
  files <- listDirectory "snaplets/heist/templates"
  mapped._2 %~bra
-}

{-
staticRoutes :: [(ByteString, Handler App App ())]
staticRoutes =
  [ ("",          serveDirectory "static")
  ]
-}


data ParLabels a b c = L1 a | L2 b | L3 c

app :: SnapletInit App App
app = makeSnaplet "piperka" "Piperka application." Nothing $ do
  cfg <- getSnapletUserConfig
  conn <- liftIO $ Data.Configurator.lookupDefault "postgresql://kaol@/piperka" cfg "db"
  mgr <- liftIO newTlsManager
  [~(L1 elookup), ~(L2 tlookup), ~(L3 tfp), ~(L3 efp)] <- liftIO $
    (map $ either error id) <$>
    withPool 4 (flip parallel
                [ (fmap L1) <$> generateExternal
                , (fmap L2) <$> generateTag
                , (fmap L3) <$> generateTagFormPart
                , (fmap L3) <$> generateExternalFormPart
                ])
  let initData = AppInit efp tfp
  let authSettings =  defAuthSettings
       & authSessionCookieName .~ "_session"
       & authUserField .~ "_login"
       & authPasswordField .~ "_password"
  m <- nestSnaplet "messages" messages $
       initCookieSessionManager "site_key.txt" "messages" Nothing (Just 3600)
  let m' = subSnaplet messages
  a <- nestSnaplet "auth" auth $ authInit Nothing $ authSettings
       & authName .~ "auth"
  a' <- nestSnaplet "apiAuth" apiAuth $
        authInit (Just $ piperkaOAuth2 m' mgr) $ authSettings
        & authName .~ "apiAuth"
  h <- nestSnaplet "" heist $ heistInit' "templates" $
       emptyHeistConfig
       & hcLoadTimeSplices .~ defaultLoadTimeSplices
       & hcNamespace .~ "h"
       & hcCompiledSplices .~ (piperkaSplices initData)
       & hcTemplateLocations .~ [loadTemplates "templates"]
  d <- nestSnaplet "" db $ hasqlInit conn
  addRoutes =<< (liftIO routes)
  return $ App h a a' d m elookup tlookup mgr False False Nothing Nothing
