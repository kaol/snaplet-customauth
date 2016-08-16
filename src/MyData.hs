
module MyData where

import Piperka.Snaplet.Auth
import qualified Control.Exception as E
import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T


instance IAuthBackend MyData (AuthManager MyData App) where
  login AuthManager{..} u pwd = do
    flip E.catch onFailure $ do
      liftPG' backend $ \conn -> do
        cred :: Maybe (Int, String, String, String) <- fmap listToMaybe P.query conn "select o_uid, name, p_session, csrf_ham from auth_login(?, ?) join users on o_uid=uid" (u, pwd)
        let usr = fmap (uncurryN MyData)
        return $ maybe (Left AuthFailure) Right usr
  logout AuthManager{..} u = error "todo"
  recover AuthManager{..} t = error "todo"

{-
instance IAuthBackend MyData x where
  login AuthManager{..} u p = error "todo"
  logout AuthManager{..} u = error "todo"
  recover AuthManager{..} t = error "todo"
-}

instance UserData MyData where
  extractUser MyData{..} = AuthUser
    { name = uname
    , session = usession
    , csrfToken = ucsrfToken
    }

