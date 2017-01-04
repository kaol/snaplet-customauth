{-# LANGUAGE OverloadedStrings #-}

module Piperka.Listing.Navigate.Splices (navigateSplices, offsetHref) where

import Heist
import Heist.Compiled as C
import Prelude hiding (Ordering)
import Data.Text (Text)
import Network.HTTP.Types.URI (Query)
import qualified Data.ByteString.Char8 as B
import Data.Map.Syntax

import Application
import Piperka.Util (encodePathToText)

navigateSplices
  :: Bool
  -> Splices (RuntimeAppHandler (([Text], Query), Int, Int, Int))
navigateSplices canSubscribe = do
  "subscribeControl" ## const $
    if canSubscribe then runChildren else return mempty
  "start" ## renderNavigate (const . const . const 0)
  "prev" ## renderNavigate (const (-))
  "next" ## renderNavigate (const (+))
  "end" ## renderNavigate (const . const)

renderNavigate
  :: (Int -> Int -> Int -> Int)
  -> RuntimeAppHandler (([Text], Query), Int, Int, Int)
renderNavigate getOffset runtime = do
  deferMap getThisOffset
    (\runtimeAction -> withLocalSplices mempty
                       (offsetHref runtimeAction)
                       C.runChildren) runtime
  where
    getThisOffset (q, curOffset, blockSize, total) = do
      let total' = total - blockSize
      let offset = getOffset total' curOffset blockSize
      let offset' = if offset < 0 then 0
                    else if offset > total' then total'
                         else offset
      return $ if (offset' == curOffset) then Nothing else Just (q, offset')

offsetHref
  :: RuntimeSplice AppHandler (Maybe (([Text], Query), Int))
  -> Splices (AttrSplice AppHandler)
offsetHref runtime =
  "href" ## \attrName -> do
    val <- runtime
    return $ maybe [] (\((path, q), offset) ->
                        let q' = q ++ [("offset", Just $ B.pack $ show offset)] in
                        [(attrName, encodePathToText path q')]) val
