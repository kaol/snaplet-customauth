{-# LANGUAGE OverloadedStrings #-}

module Piperka.Account (renderAccountForm, getUserEmail, mayCreateAccount) where

import Control.Lens
import Control.Monad.Trans
import Heist.Compiled
import Heist.Compiled.Extra ( eitherDeferMap )

import Application
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
      upd <- lift $ accountUpdates usr
      accs <- lift $ getAccountSettings $ uid usr
      return $ accs >>= \a -> Right $
        either (\e -> (Just e, (a, usr))) (\u' -> (Nothing, (a, u'))) upd

mayCreateAccount
  :: AppHandler ()
mayCreateAccount = do
  return () -- TODO
