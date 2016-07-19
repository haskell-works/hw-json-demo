{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.Json.Demo where

import           Control.Monad
import qualified Data.ByteString                                  as BS
import qualified Data.DList                                       as DL
import           Data.List
import qualified Data.Vector.Storable                             as DVS
import           Data.Word
import           GHC.Base
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Json.Succinct.Index
import           HaskellWorks.Data.Json.Succinct.PartialIndex
import           HaskellWorks.Data.Json.Value
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Diagnostics.Time

newtype Mini a = Mini a

class MiniShow a where
  miniShows :: a -> String -> String
  miniShow :: a -> String
  miniShow a = miniShows a []

instance MiniShow JsonPartialValue where
  miniShows v = case v of
    JsonPartialString s   -> shows s
    JsonPartialNumber n   -> shows n
    JsonPartialObject []  -> ("{}" ++)
    JsonPartialObject _   -> ("{..}" ++)
    JsonPartialArray []   -> ("[]" ++)
    JsonPartialArray _    -> ("[..]" ++)
    JsonPartialBool w     -> shows w
    JsonPartialNull       -> ("null" ++)
    JsonPartialError s    -> ("<error " ++). shows s . (">" ++)

instance Show (Mini JsonPartialValue) where
  showsPrec _ v = case v of
    Mini (JsonPartialString s ) -> shows s
    Mini (JsonPartialNumber n ) -> shows n
    Mini (JsonPartialObject []) -> ("{}" ++)
    Mini (JsonPartialObject _ ) -> ("{..}" ++)
    Mini (JsonPartialArray [] ) -> ("[]" ++)
    Mini (JsonPartialArray _  ) -> ("[..]" ++)
    Mini (JsonPartialBool w   ) -> shows w
    Mini  JsonPartialNull       -> ("null" ++)
    Mini (JsonPartialError s  ) -> ("<error " ++) . shows s . (">" ++)

instance Show (Mini (String, JsonPartialValue)) where
  showsPrec _ (Mini (fieldName, jpv)) = shows fieldName . (": " ++) . shows (Mini jpv)

instance Show a => Show (Mini [a]) where
  show (Mini xs) = case length xs of
    xsLen | xsLen == 0    -> "[]"
    xsLen | xsLen <= 10   -> "[" ++ intercalate ", " (show `map` xs) ++ "]"
    _                     -> "[" ++ intercalate ", " (show `map` take 10 xs) ++ ", ..]"

newtype QueryJsonCursor = QueryJsonCursor [JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))]

readJson :: String -> IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- BS.readFile path
  putStrLn "Read file"
  !cursor <- measure (fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  putStrLn "Created cursor"
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

expandObject :: JsonPartialValue -> QuerySet (String, JsonPartialValue)
expandObject jpv = case jpv of
  JsonPartialObject fs  -> QuerySet $ DL.fromList fs
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
    []                              -> ("[]" ++)
    xs                              -> (("[" ++ intercalate ", " (show `map` xs) ++ "]") ++)

instance Show (QuerySet JsonPartialValue) where
  showsPrec _ (QuerySet das) = shows (Mini (Mini `fmap` das))

instance Show (QuerySet (String, JsonPartialValue)) where
  showsPrec _ (QuerySet das) = shows (Mini (Mini `fmap` das))
