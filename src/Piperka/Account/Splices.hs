{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Piperka.Account.Splices (accountSplices, accountAttrSplices) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Syntax
import Heist
import Heist.Compiled
import Heist.Compiled.Extra ( checkedSplice
                            , runConditionalChildren
                            , stdConditionalSplice
                            , IndexedAction(..))
import qualified HTMLEntities.Text as HTML

import Application
import Piperka.Account.Types
import Piperka.Error.Splices
import Piperka.Listing.Types (ViewColumns, intToColumns)
import Piperka.Profile.Types (Privacy, intToPrivacy)

accountSplices
  :: RuntimeSplice AppHandler (Maybe AccountUpdateError, (AccountData, MyData))
  -> Splices (Splice AppHandler)
accountSplices n = mapV ($ n) $ do
  let runId f = runConditionalChildren . fmap
        (not . null . filter (f . identification) . providers . fst . snd)
  "writeup" ## pureSplice . textSplice $ HTML.text . fromMaybe "" .
    writeup . userAccount . fst . snd
  "haveRemovableProviders" ## runId isJust
  "haveAttachableProviders" ## runId isNothing
  "oauth2Providers" ## deferMany (withSplices runChildren oauth2Splices) . fmap
    (providers . fst . snd)
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

oauth2Splices
  :: Splices (RuntimeSplice AppHandler ProviderData -> Splice AppHandler)
oauth2Splices = do
  "label" ## pureSplice . textSplice $ label
  "name" ## pureSplice . textSplice $ name
  "identification" ## pureSplice . textSplice $ fromMaybe "" . identification
  "hasIdentification" ## checkedSplice $ isJust . identification
