module Data.VarInt(encode,decode) where

import Node.Buffer (Buffer)

foreign import encode :: Int -> Buffer

foreign import decode :: Buffer -> Int -> Int

