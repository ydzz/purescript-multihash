module Data.VarInt(encode,decode) where


import Data.Array (unsafeIndex)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Node.Buffer (Buffer)
import Partial.Unsafe (unsafePartial)
import Prelude (($))

foreign import encode :: Int -> Array Int

foreign import decode_ :: Buffer -> Int -> Array Int

decode::Buffer -> Int -> Tuple Int Int
decode buf n =  (unsafePartial $ unsafeIndex arr 0) /\ (unsafePartial $ unsafeIndex arr 1)
 where 
   arr = decode_ buf n