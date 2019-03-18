module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.VarInt as V
main :: Effect Unit
main = do
  log "You should add some tests."
  let arr = V.encode 300
  log $ show $ arr
  log $ show $ V.decode arr 0
