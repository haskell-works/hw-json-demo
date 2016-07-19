{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.Json.Demo where

import qualified Data.ByteString                                  as BS
import           Data.List
import qualified Data.Vector.Storable                             as DVS
import           Data.Word
import           GHC.Base
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
import qualified Data.DList                                                 as DL
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Json.Succinct.Cursor                     as C
import           HaskellWorks.Data.Json.Succinct.Index
import           HaskellWorks.Data.Json.Succinct.PartialIndex
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

loadJson :: String -> IO (Either DecodeError [JsonValue])
loadJson filename = do
  !cursor <- readJson filename
  let !jsonResult = (jsonIndexAt >=> jsonValueAt) cursor
  return $ (:[]) `fmap` jsonResult

loadJsonPartial :: String -> IO JsonPartialValue
loadJsonPartial filename = do
  !cursor <- readJson filename
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

expandArray :: JsonPartialValue -> QuerySet JsonPartialValue
expandArray jpv = case jpv of
  JsonPartialArray es -> QuerySet $ DL.fromList es
  _                   -> QuerySet   DL.empty

expandObject :: JsonPartialValue -> QuerySet JsonPartialValue
expandObject jpv = case jpv of
  JsonPartialObject fs  -> QuerySet $ DL.fromList (snd `map` fs)
  _                     -> QuerySet   DL.empty

jsonKeys :: JsonPartialValue -> [String]
jsonKeys jpv = case jpv of
  JsonPartialObject fs  -> fst `map` fs
  _                     -> []

hasKey :: String -> JsonPartialValue -> Bool
hasKey fieldName jpv = fieldName `elem` jsonKeys jpv

inArray :: QuerySet JsonPartialValue -> QuerySet JsonPartialValue
inArray jpvs = jpvs >>= expandArray

jsonSize :: JsonPartialValue -> QuerySet JsonPartialValue
jsonSize jpv = case jpv of
  JsonPartialArray  es  -> QuerySet (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  JsonPartialObject es  -> QuerySet (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  _                     -> QuerySet (DL.singleton (JsonPartialNumber 0))

newtype QuerySet a = QuerySet (DL.DList a)

deriving instance Functor     QuerySet
deriving instance Applicative QuerySet
deriving instance Monad       QuerySet
deriving instance Alternative QuerySet
deriving instance MonadPlus   QuerySet

instance Show a => Show (Mini (DL.DList a)) where
  showsPrec _ (Mini dxs) = case DL.toList dxs of
    xs@(_:_:_:_:_:_:_:_:_:_:_:_:_)  -> (("[" ++ intercalate ", " (show `map` take 10 xs) ++ ", ..]") ++)
    []                              -> shows "[]"
    xs                              -> (("[" ++ intercalate ", " (show `map` xs) ++ "]") ++)

instance Show (QuerySet JsonPartialValue) where
  -- show (QuerySet jpvs) = show (Mini (Mini `map` jpvs))
  showsPrec _ (QuerySet das) = shows (Mini (Mini `fmap` das))
