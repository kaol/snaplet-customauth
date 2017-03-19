{-# LANGUAGE OverloadedStrings #-}

module Piperka.Account.Action (accountUpdates) where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Snap.Core
import Text.HTML.TagSoup

import Application
import Piperka.Account.Types
import Piperka.Account.Query
import Piperka.Listing.Types (intToColumns)
import Piperka.Profile.Types (intToPrivacy)
import Piperka.Util (maybeParseInt, maybeDecodeText)

getAccountUpdateUnpriv
  :: Params
  -> Maybe AccountUpdate
getAccountUpdateUnpriv p = AccountUpdateUnpriv
  <$> (((== "1") . head) <$> M.lookup "new_windows" p)
  <*> (fromIntegral <$> ((maybeParseInt . head) =<< M.lookup "set_rows" p))
  <*> (intToColumns <$> ((maybeParseInt . head) =<< M.lookup "set_columns" p))

getAccountUpdatePriv
  :: Params
  -> Maybe AccountUpdate
getAccountUpdatePriv p = AccountUpdatePriv
  <$> ((maybeDecodeText . head) =<< M.lookup "_password" p)
  <*> (pure $ (maybeDecodeText . head) =<< M.lookup "new_passwd" p)
  <*> (pure $ (maybeDecodeText . head) =<< M.lookup "new_passwd_retype" p)
  <*> (pure $ (maybeDecodeText . head) =<< M.lookup "new_email" p)
  <*> (intToPrivacy <$> ((maybeParseInt . head) =<< M.lookup "privacy" p))
  <*> (pure $ sanitizeUserHTML <$> ((maybeDecodeText . head) =<< M.lookup "writeup" p))

isValidURL
  :: Text
  -> Bool
isValidURL u = T.take 7 u == "http://" || T.take 8 u == "https://"

-- TODO: Doesn't yet work for a tags
sanitizeUserHTML
  :: Text
  -> Text
sanitizeUserHTML txt =
  let tags = parseTags txt
  in renderTags $ concat $ flip evalState False $ forM tags $ \t -> do
        isAOpen <- get
        let maybeClose = if isAOpen then ((TagClose "a"):) else id
        case t of TagOpen "a" attrs -> do
                    let href = listToMaybe $
                          filter (\(k, v) -> k == "href" && isValidURL v) attrs
                    case href of
                      Nothing -> return []
                      Just h -> do
                        put True
                        return $ maybeClose [TagOpen "a" [h]]
                  TagOpen "p" _ -> do
                    put False
                    return $ maybeClose [TagOpen "p" []]
                  TagText t' -> return [TagText t']
                  TagClose "a" -> do
                    put False
                    return $ maybeClose []
                  TagClose "p" -> do
                    put False
                    return $ maybeClose []
                  _ -> return []

tryUpdate
  :: UserPrefs
  -> AccountUpdate
  -> AppHandler (Either AccountUpdateError UserPrefs)
tryUpdate p a@(AccountUpdateUnpriv n r c) = let u = uid $ fromJust $ user p in
  updateUnpriv u a >>=
  return . either (Left . AccountSqlError) (const $ Right updatedPrefs)
  where
    updatedPrefs = p {newExternWindows = n, rows = r, columns = c}

tryUpdate p a@(AccountUpdatePriv _ _ _ _ _ _) = runExceptT $ do
  let u = uid $ fromJust $ user p
  updateErr <- withExceptT AccountSqlError $ ExceptT $ tryUpdatePriv u a
  maybe (return p) throwE updateErr

accountUpdates
  :: UserPrefs
  -> AppHandler (Either AccountUpdateError UserPrefs)
accountUpdates p = do
  params <- getParams
  let update = getAccountUpdateUnpriv params `mplus` getAccountUpdatePriv params
  maybe (return $ Right p) (tryUpdate p) update
