{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Json.Demo.Examples.Example1 where

import qualified HaskellWorks.Data.ByteString                as BS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as JCF

example :: IO ()
example = do
  jsonBs <- BS.mmap "corpus/bench/hospitalisation.json"                 --      115
  let !ibip = JCF.simdToIbBp jsonBs                                     --      115
  let !c    = JCF.fromBsIbBp jsonBs ibip                                --      115
  return ()
