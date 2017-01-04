module Piperka.Action.Query where

import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Hasql.Session hiding (run)
import Heist
import Snap.Snaplet
import Snap.Snaplet.Hasql

import Application (db, AppHandler)
import Piperka.Action.Statements
import Piperka.Action.Types

getBookmark
  :: Text
  -> Bool
  -> ByteString
  -> Maybe Int32
  -> RuntimeSplice AppHandler (Either Error Action)
getBookmark url wantHere ip uid =
  (fmap Bookmark) <$>
  (lift $ withTop db $ run $ query (url, wantHere, ip, uid) bookmarkAndLogFetch)

setBookmark
  :: Int32
  -> Int32
  -> Int32
  -> Int32
  -> RuntimeSplice AppHandler (Either Error (Int32, Int32))
setBookmark uid cid ord subord =
  lift $ withTop db $ run $ query (uid, cid, ord, subord) bookmarkSet

subscribe
  :: Int32
  -> Int32
  -> Bool
  -> RuntimeSplice AppHandler (Either Error (Int32, Int32))
subscribe uid cid startAtFirst =
  lift $ withTop db $ run $ query (uid, cid, startAtFirst) subscribeSet

unsubscribe
  :: Int32
  -> Int32
  -> RuntimeSplice AppHandler (Either Error (Int32, Int32))
unsubscribe uid cid =
  lift $ withTop db $ run $ query (uid, cid) unsubscribeSet

getTitle
  :: Integral n
  => n
  -> RuntimeSplice AppHandler (Either Error (Maybe Text))
getTitle cid =
  lift $ withTop db $ run $ query (fromIntegral cid) titleFetch
