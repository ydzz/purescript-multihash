module Control.Error.Safe where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)



tryAssert::forall  m e.(Monad m) =>  e -> Boolean -> ExceptT e m Unit 
tryAssert errorTip true =  throwError errorTip
tryAssert errorTip false = pure unit

