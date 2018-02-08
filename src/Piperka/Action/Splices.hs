{-# LANGUAGE OverloadedStrings #-}

module Piperka.Action.Splices (renderAction) where

import Control.Error.Util (note)
import Data.Map.Syntax
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Heist
import Heist.Compiled
import Heist.Compiled.Extra (eitherDeferMap, stdConditionalSplice, IndexedAction(..))

import Application
import Piperka.Action
import Piperka.Action.Types
import Piperka.Error.Splices (sqlErrorSplices)

renderAction
  :: Splice AppHandler
  -> RuntimeAppHandler (Maybe (Maybe ActionError, Maybe Action))
renderAction contentSplice =
  eitherDeferMap (return . note ())
  (const contentSplice)
  (withSplices runChildren (actionSplices contentSplice))

actionSplices
  :: Splice AppHandler
  -> Splices (RuntimeAppHandler (Maybe ActionError, Maybe Action))
actionSplices contentSplice = do
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
renderCsrfFail = mayDeferMap maybeCsrfFail
  (withSplices runChildren $ do
      "describe" ## stdConditionalSplice failCond
      "actionInputs" ## \n' -> manyWithSplices runChildren
                               (mapV (pureSplice . textSplice) $ do
                                   "name" ## fst
                                   "value" ## snd) `defer` (encodeAction . fst <$> n')
  )
  where
    withTitleSplice s = withSplices s
                        (mapV (pureSplice . textSplice) $ do
                            "title" ## fst
                            "cid" ## snd)
    failCond Logout =
      ( "logout", Simple)
    failCond (Bookmark _) =
      ( "bookmark"
      , WithParam (\ ~(Bookmark b) _ ->
                     return $ case b of [b'] -> Just b'
                                        _ -> Nothing)
        (\s -> deferMany (withSplices s $ itemPageBinding)))
    failCond (Subscribe _ _) =
      ( "subscribe"
      , WithParam (\ ~(Subscribe cid _) ~(Just t) ->
                     return (t, T.pack $ show cid))
        withTitleSplice)
    failCond (Unsubscribe _) =
      ( "unsubscribe"
      , WithParam (\ ~(Unsubscribe cid) ~(Just t) ->
                     return (t, T.pack $ show cid))
        withTitleSplice)
    failCond (Revert _) = ( "revert", Simple)

maybeCsrfFail
  :: (Maybe ActionError, Maybe Action)
  -> RuntimeSplice AppHandler (Maybe (Action, Maybe Text))
maybeCsrfFail (Just CsrfFail, Just act) =
  return $ Just (act, Nothing)
maybeCsrfFail (Just (CsrfFailWithComic title), Just act) =
  return $ Just (act, Just title)
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
