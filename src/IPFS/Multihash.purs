module IPFS.Multihash where


import Data.ByteString as BS
import IPFS.Multihash.HashType (HashType)


data Multihash = Multihash { type::HashType , hash::BS.ByteString }