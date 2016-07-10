{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Demo where

import qualified Data.ByteString                                  as BS
import qualified Data.Vector.Storable                             as DVS
import           Data.Word
import           GHC.Conc
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Diagnostics.Time
import           System.Mem

readJson :: String -> IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- BS.readFile path
  print "Read file"
  !cursor <- measure (fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  print "Created cursor"
  return cursor

loadJson78 :: IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
loadJson78 = readJson "data/78mb.json"
