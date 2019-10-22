{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Demo.Examples.Example2 where

import Control.Monad

import qualified Data.ByteString                             as BS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as JCF
import qualified HaskellWorks.Data.TreeCursor                as TC

example :: IO ()
example = do
  let fc = TC.firstChild
  let ns = TC.nextSibling
  let jsonBs  = "[null, {\"field\": 1}]" :: BS.ByteString
  let ibip    = JCF.simdToIbBp jsonBs                                     --      115
  let cursor  = JCF.fromBsIbBp jsonBs ibip                                --      115

  let _ = fc cursor
  let _ = (fc >=> ns) cursor

  return ()
