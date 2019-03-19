module IPFS.Multihash where

import Prelude
import Type.Data.Boolean

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Maybe (isJust)
import Data.VarInt as VI
import IPFS.Multihash.HashType (HashType, fromCode)
data Multihash = Multihash { type::HashType , hash::BS.ByteString }


encodeFromByteString::BS.ByteString ->  Either String Multihash
encodeFromByteString bytes = do
   throwError "sasad"
    where
     code = VI.decode (BS.unsafeThaw bytes) 0
{- mk
    where
       byteLen = length bytes
       mk  | (length bytes) > 127 = Left $ "Unsupported hash size: " <> show byteLen
           | otherwise = Left "haha"
-}

isAppCode::Int -> Boolean
isAppCode n = n > 0 && n < 0x10

isValidCode::Int -> Boolean
isValidCode n = isAppCode n || (isJust $ fromCode n)