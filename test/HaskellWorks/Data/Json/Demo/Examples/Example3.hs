{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Demo.Examples.Example3 where

import Control.Monad
import Data.String
import Data.Word
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Json.Internal.Token.Types
import HaskellWorks.Data.Json.Standard.Cursor.Generic

import qualified Data.ByteString                                as BS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast    as JCF
import qualified HaskellWorks.Data.Json.Standard.Cursor.Generic as C
import qualified HaskellWorks.Data.TreeCursor                   as TC

example :: IO ()
example = do
  let fc = TC.firstChild
  let ns = TC.nextSibling
  let jsonBs  = "[null, {\"field\": 1}]" :: BS.ByteString
  let ibip    = JCF.simdToIbBp jsonBs
  let cursor  = JCF.fromBsIbBp jsonBs ibip
  let !_ = fc cursor
  let !_ = (fc >=> ns) cursor
  return ()
