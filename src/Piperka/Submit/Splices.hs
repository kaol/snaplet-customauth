{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Piperka.Submit.Splices (renderSubmit, renderListOfEdits) where

import Control.Monad.Trans.Class
import Control.Error.Util (hush)
import Data.Int
import Data.Map.Syntax
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock
import qualified Hasql.Decoders as DE
import qualified Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session (Error, query)
import Heist
import Heist.Compiled
import Heist.Compiled.Extra (eitherDeferMap, conditionalChildren)
import qualified HTMLEntities.Text as HTML
import Network.IP.Addr
import Snap (getParam)
import Snap.Snaplet.Hasql
import qualified Text.XmlHtml as X

import Application
import Piperka.Error.Splices
import Piperka.Util (formatTime', getCid)

data SubmitType = Edit | Submit | Moderate
  deriving (Show, Read, Eq)

renderSubmit
  :: AppInit
  -> Splice AppHandler
renderSubmit ini = do
  mode :: SubmitType <- read . T.unpack . fromJust . X.getAttribute "mode" <$>
                        getParamNode
  let
    tpart = extFormPart ini
    epart = tagFormPart ini
    cidSplices = do
      "hasCid" ## deferMany $ \n ->
        withSplices (eitherDeferMap (lift . getTitleHomepage)
                     stdSqlErrorSplice
                     (withSplices runChildren cidSplices') n)
        ("cid" ## pureSplice . textSplice $ T.pack . show) n

      "noCid" ## conditionalChildren (const runChildren) isNothing
      "submitForm" ## const $ callTemplate "_submit"
      (mapV (const . return . yieldPureText) $ do
          "tags" ## tpart
          "epedias" ## epart
        )
      "ifMode" ## const $ do
        match :: [SubmitType] <-
          read . T.unpack . fromJust . X.getAttribute "mode" <$>
          getParamNode
        if mode `elem` match then runChildren else return mempty

    cidSplices' = do
      "found" ## deferMany
        (withSplices runChildren
          (mapV (pureSplice . textSplice) $ do
              "title" ## \(t,_,_) -> t
              "homepage" ## \(_,h,_) -> h
              "description" ## \(_,_,d) -> d
          ))

      "notFound" ## conditionalChildren (const runChildren) isNothing
    submitSplices = if mode == Submit then do
      "preHomepage" ## return $ yieldRuntimeText $
        return . maybe "" HTML.text =<<
        ((hush . decodeUtf8') =<<) <$> (lift $ getParam "pre_homepage")
                    else mempty
  withLocalSplices submitSplices mempty $
    withSplices runChildren cidSplices `defer` getCid'
  where
    getCid' = fmap snd <$> lift getCid

getTitleHomepage
  :: Int
  -> AppHandler (Either Error (Maybe (Text, Text, Text)))
getTitleHomepage = run . flip query stmt . fromIntegral
  where
    stmt = statement sql (EN.value EN.int4) (DE.maybeRow decoder) True
    sql = "SELECT title, homepage, COALESCE(description, '') \
          \FROM comics WHERE cid=$1"
    decoder = (,,)
      <$> (DE.value DE.text)
      <*> (DE.value DE.text)
      <*> (DE.value DE.text)

data EditEntry = EditEntry
  { sid :: Int32
  , cid' :: Int32
  , title :: Text
  , addedOn :: UTCTime
  , fromIP :: Maybe (NetAddr IP)
  , name :: Text
  , subscribed :: Bool
  , diffTags :: Bool
  }

renderListOfEdits
  :: RuntimeAppHandler MyData
renderListOfEdits =
  eitherDeferMap (lift . getUserEdits . uid) stdSqlErrorSplice
  (manyWith runChildren listSplices listAttrSplices)
  where
    listSplices = mapV (pureSplice . textSplice) $ do
      "title" ## title
      "name" ## name
      mapV (fmap T.pack) $ do
        "sidId" ## ("sid-" <>) . show . sid
        "sid" ## show . sid
        "cidId" ## ("cid-" <>) . show . cid'
        "addedOn" ## formatTime' . addedOn
        "fromIP" ## show . fromIP
    listAttrSplices = "class" ## \n -> const $ do
      p <- n
      return $ if subscribed p || diffTags p then [("class", "youCare")] else []
    getUserEdits u = run $ query u stmt
    stmt = statement sql (EN.value EN.int4) (DE.rowsVector decoder) False
    sql = "SELECT sid, cid, title, user_edit.added_on AT TIME ZONE 'utc', \
          \from_ip, coalesce(name, ''), \
          \cid IN (SELECT cid FROM subscriptions WHERE uid=$1) AS subscribed, \
          \sid IN (SELECT sid FROM submit_tag JOIN user_edit USING (sid) WHERE \
          \tagid NOT IN (SELECT tagid FROM comic_tag WHERE cid=user_edit.cid)) OR \
          \sid IN (SELECT sid FROM user_edit JOIN comic_tag USING (cid) WHERE \
          \tagid NOT IN (SELECT tagid FROM submit_tag WHERE sid=user_edit.sid)) AS difftags \
          \FROM user_edit JOIN comics USING (cid) LEFT JOIN users USING (uid) \
          \WHERE $1 in (SELECT uid FROM moderator) ORDER BY sid"
    decoder =
      EditEntry
      <$> DE.value DE.int4
      <*> DE.value DE.int4
      <*> DE.value DE.text
      <*> DE.value DE.timestamptz
      <*> DE.nullableValue DE.inet
      <*> DE.value DE.text
      <*> DE.value DE.bool
      <*> DE.value DE.bool
