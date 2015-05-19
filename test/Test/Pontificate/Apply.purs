module Test.Test.Pontificate.Apply where

import Debug.Trace
import Data.Maybe
import Test.QuickCheck
import Test.Pontificate.Apply

checkCheckApply = do
  trace "Apply Maybe String"
  checkApply (<$>) (<$>) (<*>)
    ((<*>) :: Ap Maybe (String -> String) (String -> String) )
    (<*>) (<*>)
  trace "Apply [Number]"
  checkApplyInstance [0] ["foo"] [false]
