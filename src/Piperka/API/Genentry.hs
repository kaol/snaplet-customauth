{-# LANGUAGE OverloadedStrings #-}

module Piperka.API.Genentry (readGenentry) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Int
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import qualified Data.Textual
import Hasql.Decoders as DE
import Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session hiding (run, sql)
import Network.IP.Addr
import Snap
import Snap.Snaplet.Hasql

import Application
import Piperka.API.Common
import Piperka.ComicInfo.Epedia (Epedia, decodeEpedia)
import Piperka.Util (getParamInt)

data Genentry = Genentry
  { sid :: Int
  , title :: Text
  , homepage :: Text
  , firstPage :: Maybe Text
  , description :: Text
  , wantEmail :: Bool
  , email :: Maybe Text
  , newBanner :: Bool
  , fromIP :: NetAddr IP
  , tags :: [Int32]
  , epedias :: [Epedia]
  }

data Output = Output
  { genentry :: Maybe Genentry
  , subject :: Text
  , message :: Text
  }

instance ToJSON Output where
  toJSON x = object $
    (maybe []
     (\g ->
        [ "sid" .= sid g
        , "title" .= title g
        , "homepage" .= homepage g
        , "first_page" .= firstPage g
        -- TODO: check the first page in the updates table and split
        -- first page automatically to url_base and url_tail.
        , "url_base" .= firstPage g
        , "description" .= description g
        , "want_email" .= wantEmail g
        , "email" .= email g
        , "newbanner" .= newBanner g
        , "tags" .= tags g
        , "epedias" .= epedias g
        ]) (genentry x)) <>
    [ "message" .= message x
    , "subject" .= subject x
    ]

decodeGenentry
  :: DE.Row ([Epedia] -> Genentry)
decodeGenentry =
  Genentry
  <$> (liftA fromIntegral $ DE.value DE.int4)
  <*> DE.value DE.text
  <*> DE.value DE.text
  <*> DE.nullableValue DE.text
  <*> DE.value DE.text
  <*> DE.value DE.bool
  <*> DE.nullableValue DE.text
  <*> DE.value DE.bool
  <*> DE.value DE.inet
  <*> DE.value (DE.array $ DE.arrayDimension replicateM $ DE.arrayValue DE.int4)

readGenentry
  :: AppHandler ()
readGenentry = do
  s <- maybe (simpleFail 400 "Required parameter sid missing")
       (return . fromIntegral . snd) =<< getParamInt "sid"
  [msg1, msg2, msg3] <- liftIO $
    map decodeLatin1 . B8.split '$' <$> B.readFile "x/submit_template"
  writeLBS . encode =<<
    (runModQueries $ const $ do
        res1 <- ExceptT $ run $ query s $ statement sql1
                (EN.value EN.int4) (DE.maybeRow decodeGenentry) True
        res <- maybe (return Nothing)
               (\r -> Just . r <$>
                 ((ExceptT $ run $ query s $ statement sql2
                   (EN.value EN.int4) (DE.rowsList decodeEpedia) True))) res1
        let msg = maybe "" msg' res
            msg' r =
              let ip = Data.Textual.toText $ netHost $ fromIP r in
              if wantEmail r
              then msg1 <> ip <> msg2 <> title r <> msg3
              else ""
            subj = maybe "" subj' res
            subj' r = if wantEmail r then title r <> " added to Piperka" else ""
        return $ Output res subj msg
    )
  where
    sql1 = "SELECT sid, title, homepage, first_page, COALESCE(description, ''), \
           \want_email, email, sid IN (SELECT sid FROM submit_banner), from_ip, \
           \COALESCE((SELECT array_agg(tagid) FROM submit_tag WHERE sid=$1), '{}') \
           \FROM submit WHERE sid=$1"
    sql2 = "SELECT epid, entry FROM external_entry_submit WHERE sid=$1"
