{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Piperka.API.Submit.Banner
  ( Banner
  , BannerError(..)
  , receiveBanner
  , submitBanner
  , saveBanner
  , saveFromSubmit
  , deleteBanner
  ) where

import Contravariant.Extras.Contrazip
import Control.Exception
import Control.Monad (join)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (readInt, split)
import Data.Functor.Contravariant
import Data.Int (Int32)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Monoid
import Data.String (IsString)
import qualified Data.Text as T
import Hasql.Decoders as DE
import Hasql.Encoders as EN
import Hasql.Query
import Hasql.Session (Error, query)
import Snap.Snaplet.Hasql
import Snap.Util.FileUploads
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.Posix.Files
import System.Process.ByteString (readProcessWithExitCode)

import Application
import Piperka.API.Submit.Types

data Format = JPG | PNG | GIF

parseFormat
  :: (Eq a, IsString a)
  => a
  -> Maybe Format
parseFormat "image/jpeg" = Just JPG
parseFormat "image/png" = Just PNG
parseFormat "image/gif" = Just GIF
parseFormat _ = Nothing

type Banner = (Format, ByteString)

formatMime
  :: IsString a
  => Format
  -> a
formatMime JPG = "image/jpeg"
formatMime PNG = "image/png"
formatMime GIF = "image/gif"

formatExtension
  :: IsString a
  => Format
  -> a
formatExtension JPG = "jpg"
formatExtension PNG = "png"
formatExtension GIF = "gif"

partHandler
  :: PartInfo
  -> Either PolicyViolationException FilePath
  -> IO (Maybe (Either BannerError Banner))
partHandler _ errFile = do
  let sz = fmap fileSize . getFileStatus
  size <- either (const $ return Nothing) ((Just <$>) . sz) errFile
  if size == (Just 0) then return Nothing else (Just <$>) $ runExceptT $ do
    file <- either (throwE . Violation) return errFile
    (ex, sOut, sErr) <- liftIO $ readProcessWithExitCode "bin/check_banner"
                        [file] B.empty
    case ex of
      ExitSuccess -> do
        format <- maybe (throwE InvalidMime) return $ parseFormat sOut
        content <- lift $ B.readFile file
        return (format, content)
      ExitFailure 10 -> throwE NoImage
      ExitFailure 11 -> throwE AnimatedImage
      ExitFailure 12 -> throwE InvalidMime
      ExitFailure 13 -> do
        let xs = catMaybes $ map (fmap fst . readInt) $ split ' ' sErr
        case xs of
          [w,h] -> throwE $ InvalidDimensions h w
          _ -> throwE UnknownError
      _ -> throwE UnknownError

receiveBanner
  :: AppHandler (Maybe (Either BannerError Banner))
receiveBanner =
  let uploadPolicy = setMaximumNumberOfFormInputs 200 defaultUploadPolicy
      partPolicy part = if partFieldName part == "banner"
        then allowWithMaximumSize (getMaximumFormInputSize uploadPolicy)
        else disallow
  in join . listToMaybe <$> handleFileUploads "tmp" uploadPolicy
     partPolicy partHandler

submitBanner
  :: Int32
  -> Banner
  -> AppHandler (Either Error ())
submitBanner sid (format, content) = run $ query (sid, format, content) $
  statement sql (contrazip3 (EN.value EN.int4)
                 (formatMime >$< EN.value EN.text) (EN.value EN.bytea))
  DE.unit True
  where
    sql = "INSERT INTO submit_banner (sid, mime, banner) VALUES ($1, $2, $3)"

saveBanner
  :: Int32
  -> Banner
  -> AppHandler (Either Error ())
saveBanner c (format, content) = do
  let fileName = show c <> "." <> formatExtension format
  liftIO $ B.writeFile ("/srv/piperka.net/files/banners/" <> fileName) content
  run $ query (c, fileName) $ statement sql
    (contrazip2 (EN.value EN.int4) (T.pack >$< EN.value EN.text)) DE.unit True
  where
    sql = "INSERT INTO banners (cid, file) VALUES ($1, $2) \
          \ON CONFLICT (cid) DO UPDATE SET file = EXCLUDED.file"

saveFromSubmit
  :: Int32
  -> Int32
  -> AppHandler (Either Error ())
saveFromSubmit sid c = runExceptT $ do
  banner <- ((\(f, x) -> f >>= return . (,x)) =<<) <$>
    (ExceptT $ run $ query sid $ statement
     "SELECT mime, banner FROM submit_banner WHERE sid=$1"
     (EN.value EN.int4)
     (DE.maybeRow ((,)
                   <$> (parseFormat <$> DE.value DE.text)
                   <*> DE.value DE.bytea)) True)
  ExceptT $ maybe (return $ Right ()) (saveBanner c) banner

deleteBanner
  :: Int32
  -> AppHandler (Either Error ())
deleteBanner c = runExceptT $ do
  oldFile <- ExceptT $ run $ query c $ statement
    "SELECT file FROM banners WHERE cid=$1"
    (EN.value EN.int4) (DE.maybeRow $ DE.value DE.text) True
  flip (maybe (return ())) oldFile $ \fileName -> do
    liftIO $ catch (removeFile ("banners/" <> (T.unpack fileName)))
      (\e -> let _ = e :: IOException in return ())
    ExceptT $ run $ query c $ statement
      "DELETE FROM file WHERE cid=$1"
      (EN.value EN.int4) DE.unit True
