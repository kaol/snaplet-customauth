{-# LANGUAGE ScopedTypeVariables #-}

module Piperka.Account (renderAccountForm, accountUpdateHandler, getUserEmail) where

import Control.Lens
import Control.Monad.State
import Heist.Compiled
import Snap
import Snap.Snaplet.CustomAuth
import Snap.Snaplet.CustomAuth.User (setUser)
import Snap.Snaplet.CustomAuth.OAuth2

import Application
import Backend ()
import Heist.Compiled.Extra ( eitherDeferMap )
import Piperka.Account.Action
import Piperka.Account.Splices
import Piperka.Account.Types
import Piperka.Account.Query
import Piperka.Error.Splices

renderAccountForm
  :: RuntimeAppHandler MyData
renderAccountForm =
  eitherDeferMap act stdSqlErrorSplice
  (\n' -> withLocalSplices
          (accountSplices n')
          (accountAttrSplices $ (over _1 userAccount . snd) <$> n') runChildren)
  where
    act usr = do
      err <- lift $ withTop' id $ view accountUpdateError
      let upd :: Either AccountUpdateError MyData = maybe (Right usr) Left err
      accs <- lift $ getAccountSettings $ uid usr
      return $ accs >>= \a -> Right $
        either (\e -> (Just e, (a, usr))) (\u' -> (Nothing, (a, u'))) upd

accountUpdateHandler
  :: AppHandler ()
accountUpdateHandler = do
  full <- maybe pass return =<< withTop auth currentUser
  let usr = user full
  upd <- accountUpdates usr
  case upd of
    Left (Right (NeedsValidation p a)) -> withTop apiAuth $ do
      setUser usr
      saveAction p a
      redirectToProvider p
      return ()
    Left (Left e) -> modify $ set accountUpdateError $ Just e
    Right u -> withTop auth (setUser $ full {user = u})
