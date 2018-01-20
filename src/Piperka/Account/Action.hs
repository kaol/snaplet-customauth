{-# LANGUAGE OverloadedStrings #-}

module Piperka.Account.Action (accountUpdates, actionCallback, cancelAttach) where

import Control.Error.Util
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Binary (encode, decodeOrFail)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Map.Lazy as M
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Snap
import Snap.Snaplet.CustomAuth.OAuth2 (Provider, parseProvider)
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
  <$> (pure $ fromMaybe False $
       (== "1") <$> (listToMaybe =<< M.lookup "new_windows" p))
  <*> (fromIntegral <$> (maybeParseInt =<< listToMaybe =<< M.lookup "set_rows" p))
  <*> (intToColumns <$> (maybeParseInt =<< listToMaybe =<< M.lookup "set_columns" p))
  <*> (BookmarkOptions
       <$> (fromIntegral <$> ((\x -> if x >= 0 && x <= 4 then Just x else Nothing) =<<
                              maybeParseInt =<< listToMaybe =<< M.lookup "bookmark_sort" p))
       <*> (pure $ fromMaybe False $
            (== "1") <$> (listToMaybe =<< M.lookup "offset_bookmark_by_one" p))
       <*> ((== "1") <$> (listToMaybe =<< M.lookup "hold_bookmark" p)))

getAccountUpdatePriv
  :: Params
  -> Maybe (PrivData -> AccountUpdate)
getAccountUpdatePriv p = AccountUpdatePriv
  <$> (pure $
       (fromMaybe (Password $ fromMaybe ""
                   (maybeDecodeText =<< listToMaybe =<< M.lookup "_password" p))
        (fmap OAuth2 . parseProvider =<< listToMaybe =<< M.lookup "authenticate_with" p)))

updateProtected
  :: Params
  -> Maybe PrivData
updateProtected p = UpdateAccount
  <$> (pure $ maybeDecodeText =<< listToMaybe =<< M.lookup "new_passwd" p)
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

sanitizeUserHTML
  :: Text
  -> Text
sanitizeUserHTML txt =
  let tags = parseTags txt
      (tags', openA) = flip runState False $ forM tags $ \t -> do
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
  in renderTags $ bool id (++[TagClose "a"]) openA $ concat $ tags'

tryUpdate
  :: MyData
  -> AccountUpdate
  -> AppHandler (Either (Either AccountUpdateError NeedsValidation) MyData)
tryUpdate usr a@(AccountUpdateUnpriv n r c _) = let u = uid usr in
  updateUnpriv u a >>=
  return . either
  (Left . Left . AccountSqlError)
  (const $ Right $ usr {prefs = updatedPrefs})
  where
    updatedPrefs = (prefs usr) {newExternWindows = n, rows = r, columns = c}

tryUpdate usr (AccountUpdatePriv validation upd) = runExceptT $ do
  let u = uid usr
  ExceptT $ validatePriv u upd validation
  withExceptT (Left . AccountSqlError) $ ExceptT $ tryUpdatePriv u upd
  case upd of
    AttachProvider _ _ -> lift cancelAttach
    _ -> return ()
  return usr

accountUpdates
  :: MyData
  -> AppHandler (Either (Either AccountUpdateError NeedsValidation) MyData)
accountUpdates usr = do
  params <- getParams
  let priv = getAccountUpdatePriv params
  update <- runMaybeT $ firstSuccess $ [
      hoistMaybe $ getAccountUpdateUnpriv params
    , hoistMaybe (priv <*> updateProtected params)
    , MaybeT $ withTop messages $ runMaybeT $ do
        guard $ (== (Just "1")) $ listToMaybe =<< M.lookup "attach_provider" params
        f <- hoistMaybe priv
        (provider, token) <- (\(_,_,AttachProvider p t) -> (p,t)) <$>
          (hoistMaybe . hush . decodeOrFail . fromStrict =<<
           hoistMaybe . hush . Data.ByteString.Base64.decode . encodeUtf8 =<<
           (MaybeT $ getFromSession "p_attach"))
        return $ f $ AttachProvider provider token
    , MaybeT $ withTop messages $ runMaybeT $ do
        saved <- encodeUtf8 <$> (MaybeT $ getFromSession "p_priv")
        lift $ deleteFromSession "p_priv" >> commitSession
        hoistMaybe $ (\(_,_,x) -> AccountUpdatePriv Trusted x) <$>
          (hush . decodeOrFail . fromStrict =<< (hush $ Data.ByteString.Base64.decode saved))
    ]
  maybe (return $ Right usr) (tryUpdate usr) update

actionCallback
  :: Provider
  -> Text
  -> ByteString
  -> AppHandler ()
actionCallback _ t d = do
  let decoded = (\(_,_,x) -> x) <$> (hush . decodeOrFail . fromStrict $ d)
  case decoded of
    Just (AccountPayload d') -> privUpdateConfirmed d'
    Just (AttachPayload p') -> prepareAttach p' t
    Nothing -> pass

privUpdateConfirmed
  :: PrivData
  -> AppHandler ()
privUpdateConfirmed d = do
  let payload = decodeLatin1 $ Data.ByteString.Base64.encode $ toStrict $ encode d
  withTop messages $ do
    setInSession "p_priv" payload
    commitSession
  redirect' "/account.html" 301

prepareAttach
  :: Provider
  -> Text
  -> AppHandler ()
prepareAttach p t = do
  let payload = decodeLatin1 $ Data.ByteString.Base64.encode $ toStrict $ encode $
        AttachProvider p t
  withTop messages $ do
    setInSession "p_attach" payload
    setInSession "p_attach_name" $ T.pack $ show p
    commitSession
  redirect' "/account.html" 301

cancelAttach
  :: AppHandler ()
cancelAttach = withTop messages $ do
  deleteFromSession "p_attach"
  deleteFromSession "p_attach_name"
  commitSession
