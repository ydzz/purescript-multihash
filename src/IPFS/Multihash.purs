module IPFS.Multihash(
  Multihash(..),
  fromByteString,
  fromB58String,
  toByteString,
  toB58String,
  toHexString,
  fromHexString
) where

import Control.Monad.Error.Class (throwError)
import Data.Array (drop)
import Data.Array as A
import Data.Base58 as B58
import Data.ByteString (unsafeFreeze)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Tuple.Nested ((/\))
import Data.VarInt as VI
import Effect.Unsafe (unsafePerformEffect)
import IPFS.Multihash.HashType (HashType, defLength, fromCode, toCode)
import Node.Buffer (fromString, toString)
import Node.Encoding (Encoding(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, bind, map, not, pure, show, when, ($), (&&), (<), (<>), (>), (||))
import Type.Quotient (mkQuotient)
import Unsafe.Coerce (unsafeCoerce)

data Multihash = Multihash { type::HashType , hash::BS.ByteString }

instance multihashShow :: Show Multihash where
  show (Multihash ms) = "Multihash { type:" <> show ms.type <> " , hash:"<> show ms.hash <>"}"


fromByteString::BS.ByteString ->  Either String Multihash
fromByteString bytes =  do
    _ <- when (BS.length bytes < 3) (throwError "multihash too short. must be >= 3 bytes.")
    let code /\ codeLen = VI.decode (BS.unsafeThaw bytes) 0
    _ <- when (not $ isValidCode  code) (throwError $ "multihash unknown function code: 0x"<> show code)
    let unCodeBuff = BS.pack $ drop codeLen $ BS.unpack bytes
    let bufLen /\ codeLen = VI.decode (BS.unsafeThaw unCodeBuff) 0
    _ <- when (bufLen < 1) (throwError $ "multihash invalid length: 0x"<> show bufLen)
    let unCodeUnSizeBuff = BS.pack $ drop codeLen $ BS.unpack unCodeBuff
    let hashType = unsafePartial $ fromJust $ fromCode code
    pure $ Multihash {type:hashType,hash:unCodeUnSizeBuff}

fromB58String::String -> Either String Multihash
fromB58String str = do
  let mayBuffer = B58.decode str
  case mayBuffer of
   Nothing     -> Left "base58 deocde error"
   Just buffer -> fromByteString (BS.pack (map mkQuotient buffer))

toByteString::Multihash -> BS.ByteString
toByteString (Multihash h) = BS.pack $ unsafeCoerce byteArr
  where  
   byteArr = A.concat [VI.encode $ toCode h.type,VI.encode $ defLength h.type,unsafeCoerce $ BS.unpack h.hash] 

toB58String::Multihash -> String
toB58String hash = B58.encode $ unsafeCoerce $ BS.unsafeThaw $ toByteString hash

isValidCode::Int -> Boolean
isValidCode n = n > 0 && n < 0x10 || (isJust $ fromCode n) 

toHexString::Multihash -> String
toHexString h = unsafePerformEffect $ toString Hex $ BS.unsafeThaw $ toByteString h

fromHexString::String -> Either String Multihash
fromHexString str = fromByteString $ unsafeFreeze $ unsafePerformEffect $ fromString str Hex