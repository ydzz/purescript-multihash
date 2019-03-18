module IPFS.Multihash where

import Data.ByteString as BS
import Data.Either (Either(..))
import Data.VarInt as VI
import IPFS.Multihash.HashType (HashType)

data Multihash = Multihash { type::HashType , hash::BS.ByteString }

encodeFromByteString::BS.ByteString -> Either String Multihash
encodeFromByteString bytes = Left "nmd"
    where
     code = VI.decode (BS.unsafeThaw bytes) 0
{- mk
    where
       byteLen = length bytes
       mk  | (length bytes) > 127 = Left $ "Unsupported hash size: " <> show byteLen
           | otherwise = Left "haha"
-}