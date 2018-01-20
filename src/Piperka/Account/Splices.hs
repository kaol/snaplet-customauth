{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Piperka.Account.Splices (accountSplices, accountAttrSplices) where

import Control.Monad.Trans
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
import Snap
import Snap.Snaplet.Session
import Text.XmlHtml

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
  "oauth2Providers" ## renderProviders . fmap (providers . fst . snd)
  "hasError" ## const $ callTemplate "_accountError"
  "accountValidationError" ## mayDeferMap (return . fst) accountErrorSplice
  "content" ## const $ runChildren
  "authenticateWith" ## const $ callTemplate "_authenticateWith"
  "providerName" ## const $ return $ yieldRuntimeText $ do
    dat <- lift $ withTop messages $ getFromSession "p_attach_name"
    return $ fromMaybe "" dat

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

accountAttrSplices
  :: RuntimeSplice AppHandler (UserAccountSettings, MyData)
  -> Splices (Text -> RuntimeSplice AppHandler [(Text, Text)])
accountAttrSplices n = mapV ($ n) $ do
  "value" ## valueSplice
  "columns" ## settingAttrSplice (intToColumns :: Int -> ViewColumns) .
    fmap (columns . prefs . snd)
  "privacy" ## settingAttrSplice (intToPrivacy :: Int -> Privacy) .
    fmap (privacy . fst)
  "holdBookmark" ## settingAttrSplice id .
    fmap (holdBookmark . bookmarkSettings . fst)
  "sortBookmark" ## settingAttrSplice id .
    fmap (bookmarkSort . bookmarkSettings . fst)
  "offsetMode" ## settingAttrSplice id .
    fmap (offsetMode . bookmarkSettings . fst)
  "hasNoPassword" ## hasSplice . fmap (not . hasPassword . fst)

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

settingAttrSplice
  :: (Read a, Eq b)
  => (a -> b)
  -> RuntimeSplice AppHandler b
  -> AttrSplice AppHandler
settingAttrSplice f n t = do
  val <- n
  let val' = f $ read $ T.unpack t
  return $ if val == val' then [("checked", "")] else []

hasSplice
  :: RuntimeSplice AppHandler Bool
  -> Text
  -> RuntimeSplice AppHandler [(Text, Text)]
hasSplice n t = do
  checkVal <- n
  return $ if checkVal then [(t, "1")] else []

renderProviders
  :: RuntimeAppHandler [ProviderData]
renderProviders n = do
  filterOwn <- maybe False (read . T.unpack) . getAttribute "filter"
               <$> getParamNode
  manyWithSplices runChildren oauth2Splices $
    (if filterOwn then filter (isJust . identification) else id) <$> n

oauth2Splices
  :: Splices (RuntimeSplice AppHandler ProviderData -> Splice AppHandler)
oauth2Splices = do
  "label" ## pureSplice . textSplice $ label
  "name" ## pureSplice . textSplice $ name
  "identification" ## pureSplice . textSplice $ fromMaybe "" . identification
  "hasIdentification" ## checkedSplice $ isJust . identification
