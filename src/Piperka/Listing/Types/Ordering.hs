{-# LANGUAGE OverloadedStrings #-}

module Piperka.Listing.Types.Ordering where

import Prelude hiding (Ordering)
import Data.Text (Text)

data Ordering = NewDesc | UpdateAsc | UpdateDesc | TopDesc | TitleAsc
              | UserUpdates | UserUpdatesDesc
              deriving (Show, Eq, Ord)

allOrderings :: [Ordering]
allOrderings = [ NewDesc, UpdateAsc, UpdateDesc, TopDesc, TitleAsc
               , UserUpdates, UserUpdatesDesc
               ]

orderingToText :: Ordering -> Text
orderingToText NewDesc = "new"
orderingToText UpdateAsc = "updateasc"
orderingToText UpdateDesc = "update"
orderingToText TopDesc = "top"
orderingToText TitleAsc = "name"
orderingToText UserUpdates = "userupdates"
orderingToText UserUpdatesDesc = "userupdatesdesc"

textToOrdering :: Text -> Maybe Ordering
textToOrdering "new" = Just NewDesc
textToOrdering "update" = Just UpdateDesc
textToOrdering "top" = Just TopDesc
textToOrdering "name" = Just TitleAsc
textToOrdering _ = Nothing
