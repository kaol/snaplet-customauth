{-# LANGUAGE OverloadedStrings #-}

module Piperka.Account.Action (accountUpdates, privUpdateConfirmed) where

import Control.Error.Util
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Binary (decodeOrFail)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Map.Lazy as M
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Snap
import Snap.Snaplet.CustomAuth (Provider, parseProvider)
import Snap.Snaplet.Session
import Text.HTML.TagSoup

import Application
import Piperka.Account.Types
import Piperka.Account.Query
import Piperka.Listing.Types (intToColumns)
import Piperka.Profile.Types (intToPrivacy)
import Piperka.Util (maybeParseInt, maybeDecodeText, firstSuccess)

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
  <$> (pure $
       (fromMaybe (Password $ fromMaybe ""
                   (maybeDecodeText =<< listToMaybe =<< M.lookup "_password" p))
        (fmap OAuth2 . parseProvider =<< listToMaybe =<< M.lookup "authenticate_with" p)))
  <*> (pure $ maybeDecodeText =<< listToMaybe =<< M.lookup "new_passwd" p)
  <*> (pure $ maybeDecodeText =<< listToMaybe =<< M.lookup "new_passwd_retype" p)
  <*> (pure $ maybeDecodeText =<< listToMaybe =<< M.lookup "new_email" p)
  <*> (intToPrivacy <$> (maybeParseInt =<< listToMaybe =<< M.lookup "privacy" p))
  <*> (pure $ sanitizeUserHTML <$> (maybeDecodeText =<< listToMaybe =<< M.lookup "writeup" p))
  <*> (pure $ (catMaybes . map parseProvider) (fromMaybe [] $ M.lookup "remove_oauth2" p))
  <*> (pure $ fromMaybe False $ fmap (not . B.null) $ listToMaybe =<< M.lookup "only_oauth2" p)

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
  :: MyData
  -> AccountUpdate
  -> AppHandler (Either AccountUpdateError MyData)
tryUpdate usr a@(AccountUpdateUnpriv n r c) = let u = uid usr in
  updateUnpriv u a >>=
  return . either (Left . AccountSqlError) (const $ Right $ usr {prefs = updatedPrefs})
  where
    updatedPrefs = (prefs usr) {newExternWindows = n, rows = r, columns = c}

tryUpdate usr a@(AccountUpdatePriv _ _ _ _ _ _ _ _) = runExceptT $ do
  let u = uid usr
  ExceptT $ validatePriv u a
  withExceptT AccountSqlError $ ExceptT $ tryUpdatePriv u a
  return usr

accountUpdates
  :: MyData
  -> AppHandler (Either AccountUpdateError MyData)
accountUpdates usr = do
  params <- getParams
  update <- runMaybeT $ firstSuccess $ [
      MaybeT . return $ getAccountUpdateUnpriv params
    , MaybeT . return $ getAccountUpdatePriv params
    , MaybeT $ withTop messages $ runMaybeT $ do
        saved <- encodeUtf8 <$> (MaybeT $ getFromSession "p_priv")
        lift $ deleteFromSession "p_priv" >> commitSession
        MaybeT . return $ (\(_,_,x) -> x) <$>
          (hush . decodeOrFail . fromStrict =<< (hush $ Data.ByteString.Base64.decode saved))
    ]
  maybe (return $ Right usr) (tryUpdate usr) update

privUpdateConfirmed
  :: Provider
  -> Text
  -> ByteString
  -> AppHandler ()
privUpdateConfirmed _ _ d = do
  withTop messages $ do
    setInSession "p_priv" (decodeLatin1 $ Data.ByteString.Base64.encode d)
    commitSession
  redirect' "/account.html" 301
