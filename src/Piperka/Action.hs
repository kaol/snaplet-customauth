{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- Any actions that may be done via the HTML pages, via request
-- parameters.

module Piperka.Action (processAction, encodeAction) where

import Control.Error.Util hiding (err)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Int
import Data.Map.Lazy (lookup)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V
import Data.UUID (toASCIIBytes)
import qualified Hasql.Session
import Heist
import Prelude hiding (lookup)
import Snap
import Snap.Snaplet.CustomAuth.Handlers (logoutUser)

import Application hiding (uid)
import Backend ()
import qualified Application as A
import Piperka.Action.Types
import Piperka.Action.Query
import Piperka.Util (maybeParseInt, if', maybeParseInt)

processAction
  :: UserPrefsWithStats
  -> (RuntimeSplice AppHandler
      (Maybe (Maybe ActionError, Maybe Action), UserPrefsWithStats))
processAction usr = do
  rq <- lift getRequest
  let params = rqParams rq
  let uid = A.uid <$> (user $ prefs usr)
  action <- extractAction rq params uid
  case action of
   Nothing -> return (Nothing, usr)
   Just (Left err) -> return (Just (Just $ SqlError err, Nothing), usr)
   Just (Right act) -> do
     result <- runExceptT $ do
       u <- hoistEither $ note NeedsLogin $ user $ prefs usr
       let csrf = head <$> lookup "csrf_ham" params
       let csrfOk = csrf == (Just $ toASCIIBytes $ ucsrfToken u)
       perform act csrfOk usr $ A.uid u
     return $ (\(e,x) -> (Just (e, Just act), x)) $
       either ((,usr) . Just) (Nothing,) result

extractAction
  :: Request
  -> Params
  -> Maybe Int32
  -> (RuntimeSplice AppHandler
      (Maybe (Either Hasql.Session.Error Action)))
extractAction rq params uid = do
  act <- runExceptT $ sequence_ [ extractSubscribe
                                , extractUnsubscribe
                                , extractBookmark
                                , extractRevert
                                , extractLogout
                                ]
  return $ either Just (const Nothing) act
  where
    lookup' x = head <$> lookup x params
    extract = maybe (return ()) (throwE . Right)
    extractSubscribe = do
      let action = do
            cid <- maybeParseInt =<< lookup' "subscribe"
            let startAtFirst = isJust $ lookup' "start_at_first"
            return $ Subscribe cid startAtFirst
      extract action
    extractUnsubscribe = extract $ Unsubscribe <$>
                         (maybeParseInt =<< lookup' "unsubscribe")
    extractBookmark =
      case lookup' "action" of
       Just "bookmark" -> do
         let act = do
               cid <- maybeParseInt =<< lookup' "cid"
               ord <- maybeParseInt =<< lookup' "ord"
               subord <- maybeParseInt =<< lookup' "subord"
               let latest = lookup' "latest" == Just "True"
               return $ Bookmark [(cid, "TODO", Just (ord, subord, latest))]
         extract act
       _ -> do
         let url = lookup' "bookmark"
         let wantHere = isJust $ lookup' "wantbookmarkhere"
         let addr = rqClientAddr rq
         maybe (return ())
           (\u -> (lift $ getBookmark (decodeUtf8 u) wantHere addr uid)
                  >>= throwE) url
    extractLogout = do
      let logout = Just "logout" == lookup' "action"
      extract $ if' (Just Logout) Nothing logout
    extractRevert = do
      let revert = lookup "revert" params
      extract $ (Revert . V.fromList . map fromIntegral .
                 mapMaybe maybeParseInt) <$> revert

perform
  :: Action
  -> Bool
  -> UserPrefsWithStats
  -> Int32
  -> ExceptT ActionError (RuntimeSplice AppHandler) UserPrefsWithStats
perform Logout True _ _ = do
  lift $ lift $ withTop auth logoutUser
  return defaultUserPrefsWithStats

perform (Bookmark [(cid, _, Just (ord, subord, _))]) True usr uid =
  performSql usr $ setBookmark uid (fromIntegral cid)
  (fromIntegral ord) (fromIntegral subord)

perform (Subscribe cid startAtFirst) True usr uid =
  performSql usr $ subscribe uid (fromIntegral cid) startAtFirst

perform (Unsubscribe cid) True usr uid =
  performSql usr $ unsubscribe uid (fromIntegral cid)

perform (Revert cids) True usr uid =
  performSql usr $ revertUpdates uid cids

perform (Bookmark _) _ usr _ = return usr
perform (Subscribe cid _) False _ _ = csrfFailWithComic cid
perform (Unsubscribe cid) False _ _ = csrfFailWithComic cid
perform _ False _ _ = hoistEither $ Left CsrfFail

-- Get the comic title.  If it fails, give another error
csrfFailWithComic
  :: Int
  -> ExceptT ActionError (RuntimeSplice AppHandler) UserPrefsWithStats
csrfFailWithComic cid = do
  title <- either (throwE . SqlError) return =<< (lift $ getTitle cid)
  hoistEither $ Left $ maybe UnknownAction CsrfFailWithComic title

performSql
  :: UserPrefsWithStats
  -> RuntimeSplice AppHandler (Either Hasql.Session.Error (Int32, Int32))
  -> ExceptT ActionError (RuntimeSplice AppHandler) UserPrefsWithStats
performSql usr act =
  either (\x -> throwE $ SqlError x) return =<<
  (lift $ runExceptT $ do
      (total, count) <- ExceptT act
      return $ usr {unreadCount = (total, count)})

-- In case of CSRF fail and confirmation
encodeAction :: Action -> [(Text, Text)]
encodeAction Logout = [("action", "logout")]
encodeAction (Bookmark [(cid, _, (Just (ord, subord, latest)))]) =
  [ ("action", "bookmark")
  , ("cid", T.pack $ show $ cid)
  , ("ord", T.pack $ show $ ord)
  , ("subord", T.pack $ show $ subord)
  , ("latest", T.pack $ show $ latest)
  ]
encodeAction (Bookmark _) = error "should not happen"
encodeAction (Subscribe cid startAtFirst) =
  [("subscribe", T.pack $ show cid)] <>
  if startAtFirst then [("start_at_first", "True")] else []
encodeAction (Unsubscribe cid) = [("unsubscribe", T.pack $ show $ cid)]
encodeAction (Revert cids) = map (\c -> ("revert", T.pack $ show c)) $ V.toList cids
