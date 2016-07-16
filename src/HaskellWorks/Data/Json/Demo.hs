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
import           HaskellWorks.Data.TreeCursor

import           Control.Monad
import qualified Data.ByteString                                            as BS
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor                     as C
import           HaskellWorks.Data.Json.Succinct.Index
import           HaskellWorks.Data.Json.Value
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import qualified HaskellWorks.Data.TreeCursor                               as TC

newtype QueryJsonCursor = QueryJsonCursor [JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))]

readJson :: String -> IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- BS.readFile path
  print "Read file"
  !cursor <- measure (fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  print "Created cursor"
  return cursor

-- (/@) :: [JsonValue] -> String -> [JsonValue]
-- (/@) js fieldName = js >>= \j -> do
--   case j of
--     JsonObject ks -> ks
--     _ ->
--   return j

loadJson :: String -> IO (Either DecodeError [JsonValue])
loadJson filename = do
  !cursor <- readJson filename
  let !jsonResult = (jsonIndexAt >=> jsonValueAt) cursor
  return $ (:[]) `fmap` jsonResult

loadJson78 :: IO (Either DecodeError [JsonValue])
loadJson78 = do
  !cursor <- readJson "data/78mbs.json"
  let !jsonResult = (jsonIndexAt >=> jsonValueAt) cursor
  return $ (:[]) `fmap` jsonResult
