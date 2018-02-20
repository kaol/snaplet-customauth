module Piperka.Action.Query where

import Data.Int
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Session hiding (run)
import Network.IP.Addr
import Snap.Snaplet.Hasql (run)

import Application (AppHandler)
import Piperka.Action.Statements
import Piperka.Action.Types

getComicTitle
  :: Int32
  -> AppHandler (Either Error (Maybe Text))
getComicTitle uid =
  run $ query uid comicTitleFetch

getBookmark
  :: Text
  -> Bool
  -> NetAddr IP
  -> Maybe Int32
  -> AppHandler (Either Error Action)
getBookmark url wantHere ip uid =
  (fmap Bookmark) <$>
  (run $ query (url, wantHere, ip, uid) bookmarkAndLogFetch)

setBookmark
  :: Int32
  -> Int32
  -> Int32
  -> Int32
  -> AppHandler (Either Error (Int32, Int32))
setBookmark uid cid ord subord =
  run $ query (uid, cid, ord, subord) bookmarkSet

subscribe
  :: Int32
  -> Int32
  -> Bool
  -> AppHandler (Either Error (Int32, Int32))
subscribe uid cid startAtFirst =
  run $ query (uid, cid, startAtFirst) subscribeSet

unsubscribe
  :: Int32
  -> Int32
  -> AppHandler (Either Error (Int32, Int32))
unsubscribe uid cid =
  run $ query (uid, cid) unsubscribeSet

revertUpdates
  :: Int32
  -> Vector Int32
  -> AppHandler (Either Error (Int32, Int32))
revertUpdates uid cids =
  run $ query (uid, cids) revertUpdatesSet

getTitle
  :: Integral n
  => n
  -> AppHandler (Either Error (Maybe Text))
getTitle cid =
  run $ query (fromIntegral cid) titleFetch
