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
import           Snap.Snaplet.CustomAuth
import           Snap.Snaplet.Hasql


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
routes :: [(ByteString, Handler App App ())]
routes =
  [ ("/login",    with auth handleLoginSubmit)
  , ("/logout",   with auth handleLogout)
--         , ("/new_user", with auth handleNewUser)
  ]

staticRoutes =
  [ ("",          serveDirectory "static")
  ]


app :: SnapletInit App App
app = makeSnaplet "piperka" "Piperka application." Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  a <- nestSnaplet "" auth $ authInit "_session" "_login" "_password"
  d <- nestSnaplet "" db $ hasqlInit "postgresql://kaol@/piperka"
  addRoutes staticRoutes
  addRoutes routes
  addAuthSplices h auth
-- TODO: wrap instead just routes which need DB & auth
  wrapSite (\site -> with auth handleRecover >> site)
  wrapDbOpen
  return $ App h a d
