{-# LANGUAGE OverloadedStrings #-}

module Piperka.Action.Splices (renderAction) where

import Data.Map.Syntax
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Heist
import Heist.Compiled
import Heist.Compiled.Extra (eitherDeferMap)

import Application
import Piperka.Action.Types
import Piperka.Action
import Piperka.Error.Splices (sqlErrorSplices)

renderAction
  :: Splice AppHandler
  -> RuntimeAppHandler (Maybe (Maybe ActionError, Maybe Action), UserPrefs)
renderAction contentSplice =
  eitherDeferMap (return . maybe (Left ()) Right . fst)
  (const contentSplice)
  (withSplices runChildren (actionSplices contentSplice))

actionSplices
  :: Splice AppHandler
  -> Splices (RuntimeAppHandler (Maybe ActionError, Maybe Action))
actionSplices contentSplice = do
  -- Use to direct user to updates.html page on form submit
  "thisPage" ## const $ return $ yieldPureText "updates.html"
  "bookmark" ## renderBookmark
  "logout" ## renderLogout
  "csrfFail" ## renderCsrfFail
  "sqlErr" ## renderSqlErr contentSplice
  "unknownAction" ## renderUnknown
  where
    renderLogout n = do
      sub <- runChildren
      flip bindLater n $ \(_, act) -> do
        case act of Just Logout -> codeGen sub
                    _ -> return mempty
    renderUnknown n = do
      sub <- runChildren
      flip bindLater n $ \(err, _) -> do
        case err of Just UnknownAction -> codeGen sub
                    _ -> return mempty

itemBinding
  :: Splices (RuntimeAppHandler (Int, Text, Maybe (Int, Int, Bool)))
itemBinding = mapV (pureSplice . textSplice) $ do
  "cid" ## \(c, _, _) -> T.pack $ show c
  "title" ## \(_, t, _) -> t

itemPageBinding
  :: Splices (RuntimeAppHandler (Int, Text, Maybe (Int, Int, Bool)))
itemPageBinding =
  itemBinding <>
  (do
      "newest" ## (newest True)
      "notNewest" ## (newest False)
      "ord" ## pureSplice . textSplice $
        \(_, _, Just (o, s, _)) -> T.pack $
                                   show (o+(if s > 0 then 2 else 1)))
  where
    newest v n = do
      spl <- runChildren
      flip bindLater n $ \(_, _, Just (_, _, x)) ->
        if x == v then codeGen spl else return mempty

renderBookmark
  :: RuntimeAppHandler (Maybe ActionError, Maybe Action)
renderBookmark n = do
  let multipleBinding = "item" ## withSplices runChildren itemBinding

  let success = mayDeferMap successF
                (withSplices runChildren itemPageBinding)
      multiple = mayDeferMap multipleF
                 (manyWithSplices runChildren multipleBinding)
      recognized = mayDeferMap recognizedF (withSplices runChildren itemBinding)
      failed = mayDeferMap failedF (const runChildren)
  mayDeferMap maybeBookmark
    (withSplices runChildren (do "success" ## success
                                 "multiple" ## multiple
                                 "recognized" ## recognized
                                 "failed" ## failed)) n
  where
    maybeBookmark (Nothing, Just (Bookmark x)) = return $ Just x
    maybeBookmark _ = return Nothing
    successF [x@(_, _, Just _)] = return $ Just x
    successF _ = return Nothing
    multipleF xs@(_:_:_) = return $ Just xs
    multipleF _ = return Nothing
    recognizedF [x@(_, _, Nothing)] = return $ Just x
    recognizedF _ = return Nothing
    failedF [] = return $ Just ()
    failedF _ = return Nothing

renderCsrfFail
  :: RuntimeAppHandler (Maybe ActionError, Maybe Action)
renderCsrfFail n = do
  let withTitleSplice = withSplices runChildren
                        (mapV (pureSplice . textSplice) $ do
                            "title" ## fst
                            "cid" ## snd
                        )
  let logout = mayDeferMap logoutF (const runChildren)
      bookmark = mayDeferMap bookmarkF (withSplices runChildren itemPageBinding)
      subscribe = mayDeferMap subscribeF withTitleSplice
      unsubscribe = mayDeferMap unsubscribeF withTitleSplice
      actionInputs n' = manyWithSplices runChildren
                        (mapV (pureSplice . textSplice) $ do
                            "name" ## fst
                            "value" ## snd) `defer` (encodeAction . snd <$> n')
  mayDeferMap maybeCsrfFail
    (withSplices runChildren (do "logout" ## logout
                                 "bookmark" ## bookmark
                                 "subscribe" ## subscribe
                                 "unsubscribe" ## unsubscribe
                                 "actionInputs" ## actionInputs)) n
  where
    logoutF (_, Logout) = return $ Just ()
    logoutF _ = return Nothing
    bookmarkF (_, Bookmark [x]) = return $ Just x
    bookmarkF _ = return Nothing
    subscribeF (Just title, Subscribe cid _) =
      return $ Just (title, T.pack $ show cid)
    subscribeF _ = return Nothing
    unsubscribeF (Just title, Unsubscribe cid) =
      return $ Just (title, T.pack $ show cid)
    unsubscribeF _ = return Nothing

maybeCsrfFail
  :: (Maybe ActionError, Maybe Action)
  -> RuntimeSplice AppHandler (Maybe (Maybe Text, Action))
maybeCsrfFail (Just CsrfFail, Just act) =
  return $ Just (Nothing, act)
maybeCsrfFail (Just (CsrfFailWithComic title), Just act) =
  return $ Just (Just title, act)
maybeCsrfFail _ = return Nothing

renderSqlErr
  :: Splice AppHandler
  -> RuntimeAppHandler (Maybe ActionError, Maybe Action)
renderSqlErr contentSplice =
  eitherDeferMap mapAction
  (withSplices (callTemplate "_sqlErr") sqlErrorSplices)
  (const contentSplice)
  where
    mapAction (Just (SqlError e), _) = return $ Left e
    mapAction _ = return $ Right ()
