{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Piperka.Account (renderAccountForm, getUserEmail) where

import Control.Monad.Trans
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Syntax
import Heist
import Heist.Compiled
import Heist.Compiled.Extra (eitherDeferMap, stdConditionalSplice, IndexedAction(..))
import qualified HTMLEntities.Text as HTML

import Application
import Piperka.Account.Action
import Piperka.Account.Types
import Piperka.Account.Query
import Piperka.Error.Splices
import Piperka.Profile.Types (intToPrivacy, Privacy)
import Piperka.Listing.Types (intToColumns, ViewColumns)

renderAccountForm
  :: RuntimeAppHandler MyData
renderAccountForm =
  eitherDeferMap act stdSqlErrorSplice
  (\n' -> withLocalSplices
          (accountSplices n')
          (accountAttrSplices $ snd <$> n') runChildren)
  where
    act usr = do
      upd <- lift $ accountUpdates usr
      accs <- lift $ getAccountSettings $ uid usr
      return $ accs >>= \a -> Right $
        either (\e -> (Just e, (a, usr))) (\u' -> (Nothing, (a, u'))) upd

accountSplices
  :: RuntimeSplice AppHandler (Maybe AccountUpdateError, (UserAccountSettings, MyData))
  -> Splices (Splice AppHandler)
accountSplices n = mapV ($ n) $ do
  "writeup" ## pureSplice . textSplice $ HTML.text . fromMaybe "" . writeup . fst . snd
  "hasError" ## mayDeferMap (return . fst) accountErrorSplice

accountErrorSplice
  :: RuntimeSplice AppHandler AccountUpdateError
  -> Splice AppHandler
accountErrorSplice = stdConditionalSplice accountError . (fmap (,()))
  where
    accountError (AccountSqlError _) =
      ("maybeSqlError", WithParam (\ ~(AccountSqlError e) _ -> return e)
                        (flip withSplices sqlErrorSplices))
    accountError AccountPasswordMissing = ("passwordMissing", Simple)
    accountError AccountPasswordWrong = ("wrongPassword", Simple)
    accountError AccountNewPasswordMismatch = ("passwordMismatch", Simple)
    accountError (NeedsValidation _) = ("needsValidation", Simple)

accountAttrSplices
  :: RuntimeSplice AppHandler (UserAccountSettings, MyData)
  -> Splices (Text -> RuntimeSplice AppHandler [(Text, Text)])
accountAttrSplices n = mapV ($ n) $ do
  "value" ## valueSplice
  "columns" ## columnSplice . fmap (columns . prefs . snd)
  "privacy" ## privacySplice . fmap (privacy . fst)

valueSplice
  :: RuntimeSplice AppHandler (UserAccountSettings, MyData)
  -> Text
  -> RuntimeSplice AppHandler [(Text, Text)]
valueSplice n "new_windows" = newExternWindows . prefs . snd <$> n >>= \c ->
  return $ if c then [("checked", "")] else []
valueSplice n "rows" = rows . prefs . snd <$> n >>= \r ->
  return $ [("value", T.pack $ show r)]
valueSplice n "email" = email . fst <$> n >>= \e ->
  return $ maybeToList $ e >>= \e' -> return ("value", e')
valueSplice _ _ = error "unknown value"

columnSplice
  :: RuntimeSplice AppHandler ViewColumns
  -> Text
  -> RuntimeSplice AppHandler [(Text, Text)]
columnSplice n t = do
  userCol <- n
  let col = (intToColumns :: Int -> ViewColumns) $ read $ T.unpack t
  return $ if userCol == col then [("checked", "")] else []

privacySplice
  :: RuntimeSplice AppHandler Privacy
  -> Text
  -> RuntimeSplice AppHandler [(Text, Text)]
privacySplice n t = do
  userPriv <- n
  let priv = (intToPrivacy :: Int -> Privacy) $ read $ T.unpack t
  return $ if userPriv == priv then [("checked", "")] else []
