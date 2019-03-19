module Test.Main where

import Prelude

import Data.Either (fromRight)
import Data.VarInt as V
import Effect (Effect)
import Effect.Console (log)
import IPFS.Multihash as MH
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  log "You should add some tests."
  let arr = V.encode 300
  let hash = unsafePartial $ fromRight $ MH.fromB58String "QmPZ3ZfLNSiiQXj75Ft5Qs2qnPCt1Csp1nZBUppfY997Yb"
  log $ show $ hash
  let hexStr = MH.toHexString hash
  log  hexStr
  let hash2 = unsafePartial $ fromRight $ MH.fromHexString hexStr
  log $ MH.toB58String hash2
  