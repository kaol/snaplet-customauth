{-# LANGUAGE OverloadedStrings #-}

module Piperka.Listing.Navigate.Splices where

import Heist
import Heist.Compiled as C
import Prelude hiding (Ordering)
import Data.Text (Text)
import Network.HTTP.Types.URI (Query, encodePath)
import Blaze.ByteString.Builder (toByteString)
import Data.Text.Encoding (decodeLatin1)
import qualified Data.ByteString.Char8 as B
import Data.Map.Syntax

import Application

navigateSplices
  :: Bool
  -> Splices (RuntimeSplice (AppHandler) (([Text], Query), Int, Int, Int)
              -> C.Splice (AppHandler))
navigateSplices isBottom = do
  "start" ## renderNavigate (const . const . const 0)
  "prev" ## renderNavigate (const (-))
  "next" ## renderNavigate (const (+))
  "end" ## renderNavigate (const . const)
  "bottom" ## const $ return $ yieldPureText $ if isBottom then " bottom" else ""

renderNavigate
  :: (Int -> Int -> Int -> Int)
  -> RuntimeSplice (AppHandler) (([Text], Query), Int, Int, Int)
  -> Splice AppHandler
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
  "href" ## const $ do
    val <- runtime
    return $ maybe [] (\((path, q), offset) ->
                        let q' = q ++ [("offset", Just $ B.pack $ show offset)] in
                        [("href", decodeLatin1 $ toByteString $
                                  encodePath path q')]) val
