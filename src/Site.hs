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
import Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.Monoid
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import Backend
import		 Snap.Snaplet.CustomAuth
--import Snap.Snaplet.PostgresqlSimple
import qualified Hasql as H
import qualified Hasql.Postgres as HP


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager MyData App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager MyData App) ()
handleLoginSubmit =
    loginUser (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"

------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager MyData App) ()
handleLogout = logoutUser >> redirect "/"


{-
------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App App ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"
-}

handleRecover :: Handler App (AuthManager MyData App) ()
handleRecover = recoverSession

------------------------------------------------------------------------------
-- | The application's routes.
--routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
--         , ("/new_user", with auth handleNewUser)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "piperka" "Piperka application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
--    let postgresSettings = HP.ParamSettings "localhost" 5432 "kaol" "" "piperka"
    let postgresSettings = HP.StringSettings "postgresql:///piperka"
    poolSettings <- liftIO $ maybe (fail "Improper session settings") return $ H.poolSettings 6 30
    pool :: H.Pool HP.Postgres <- liftIO $ H.acquirePool postgresSettings poolSettings
    a <- nestSnaplet "auth" auth $ authInit pool "_session" "_login" "_password"
    addRoutes routes
    addAuthSplices h auth
    wrapSite (\site -> with auth handleRecover >> site)
    return $ App h a
