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
import Control.Lens
import Control.Monad.Trans
import Crypto.Hash.MD5
import Data.ByteString (ByteString, readFile)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.UTF8 (fromString)
import qualified Data.Configurator
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Database.Memcache.Client as M
import Heist
import Network.HTTP.Client.TLS
import Prelude hiding (readFile)
import Snap.Core (Cookie(..), ifTop, modifyResponse, setResponseCode)
import Snap.Snaplet
import Snap.Snaplet.CustomAuth hiding (sessionCookieName)
import Snap.Snaplet.Hasql
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
import System.Directory (listDirectory)

import Application
import Piperka.Account
import Piperka.API
import Piperka.Auth (authHandler, mayCreateAccount)
import Piperka.ComicInfo.Tag
import Piperka.ComicInfo.External
import Piperka.OAuth2
import Piperka.Splices
import Piperka.Update.Handlers

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
        , ("/s/submit", receiveSubmit)
        , ("newuser", mayCreateAccount (return ()))
        , ("/s/login", apiLogin)
        -- Moderator interface
        , ("/s/sinfo/:sid", readUserEdit)
        , ("/s/sinfo2/:sid", readSubmit)
        , ("/s/dropsubmit/:sid", dropUserEdit)
        , ("/s/viewsubmitbanner/:sid", viewSubmitBanner)
        ]
  let specialTemplates = [ "account.html"
                         , "newuser.html"
                         , "include/cinfo"
                         , "updates.html"
                         ]
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
    , ("include/cinfo", authHandler True $ cRender "include/cinfo")
    , ("updates.html", authHandler False $
        mayRedir >> cRender "updates.html")
    , ("", ifTop $ authHandler False $ cRender "index")
    , ("index.html", authHandler False $ cRender "index")
    ]

staticRoutes :: [(ByteString, Handler App App ())]
staticRoutes = mapped._2 %~ serveDirectory $
  [ ("d", "files/d")
  , ("blog", "files/blog")
  , ("banners", "files/banners")
  , ("", "static")
  ]

data ParLabels a b c d = L1 a | L2 b | L3 c | L4 d

generateMD5
  :: FilePath
  -> IO (Either String Text)
generateMD5 file = do
  content <- readFile $ "static" ++ file
  return $ Right $ decodeUtf8 $ Base16.encode $ hash $ content

app :: SnapletInit App App
app = makeSnaplet "piperka" "Piperka application." Nothing $ do
  let jsFiles =
        [ "/piperka.js"
        , "/qsearch.js"
        , "/moderate.js"
        , "/piperka.css"
        , "/qsearch.css"
        ]
  cfg <- getSnapletUserConfig
  ads <- liftIO $ Data.Configurator.lookupDefault False cfg "ads"
  conn <- liftIO $ Data.Configurator.lookupDefault "postgresql://kaol@/piperka" cfg "db"
  mgr <- liftIO newTlsManager
  mc <- liftIO $ M.newClient [M.def] M.def
  (~(L1 elookup): ~(L2 tlookup): ~(L3 tfp): ~(L3 efp):jsHash) <- liftIO $
    (map $ either error id) <$>
    withPool (4+length jsFiles) (flip parallel
                ([ (fmap L1) <$> generateExternal
                 , (fmap L2) <$> generateTag
                 , (fmap L3) <$> generateTagFormPart
                 , (fmap L3) <$> generateExternalFormPart
                 ] <> map (\x -> (fmap L4) <$> generateMD5 x) jsFiles))
  let initData = AppInit efp tfp $
        zip (map T.pack jsFiles) $ map (\ ~(L4 x) -> x) jsHash
  let authSettings =  defAuthSettings
       & authSetCookie .~ \x -> Cookie "p_session" x Nothing Nothing (Just "/") False True
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
  addRoutes staticRoutes
  addRoutes [("", bracketDbOpen $ authHandler False $
               modifyResponse (setResponseCode 404) >> cRender "404_")]
  return $ App h a a' d m elookup tlookup mgr mc
    False False Nothing Nothing Nothing ads
