module Piperka.API.Submit.Types where

import Data.Int
import Data.Text (Text)
import Hasql.Session (Error)
import Snap.Util.FileUploads (PolicyViolationException)

data SubmitResult =
    SValidation SubmitValidation
  | EValidation EditValidation
  | BValidation BannerError
  | Success Success
  | InvalidFormType
  deriving (Show)

data SubmitValidation =
    TitleOrUrl
  | ComicExists (Text, Text)
  | Database Error
  deriving (Show)

data EditValidation = NoComic | Database2 Error
  deriving (Show)

data BannerError =
    Violation PolicyViolationException
  | NoImage
  | AnimatedImage
  | InvalidMime
  | InvalidDimensions Int Int
  | UnknownError
  deriving (Show)

data Success =
    SubmissionAccepted
  | EditSubmitted Int32
  | EditedSuccessfully Int32
  deriving (Show)

resultNumbering :: SubmitResult -> Int
resultNumbering a =
  case a of
    SValidation b -> case b of
      TitleOrUrl -> 0
      ComicExists _ -> 1
      Database _ -> 2
    EValidation b -> case b of
      NoComic -> 3
      Database2 _ -> 4
    BValidation b -> case b of
      Violation _ -> 5
      NoImage -> 6
      AnimatedImage -> 7
      InvalidMime -> 8
      InvalidDimensions _ _ -> 9
      UnknownError -> 10
    Success b -> case b of
      SubmissionAccepted -> 11
      EditSubmitted _ -> 12
      EditedSuccessfully _ -> 13
    InvalidFormType -> 14

isSubmitSuccess :: SubmitResult -> Bool
isSubmitSuccess (Success _) = True
isSubmitSuccess _ = False
