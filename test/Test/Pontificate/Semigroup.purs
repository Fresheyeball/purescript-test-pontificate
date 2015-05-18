module Test.Test.Pontificate.Semigroup where

import Debug.Trace
import Test.Pontificate.Semigroup
import Test.Pontificate.Binary
import Test.QuickCheck

checkCheckSemigroup = do
  trace "-"
  checkSemigroup' (/=) ((-) :: Number -> Number -> Number)
  trace "instance Unit"
  checkSemigroupInstance unit
  trace "instance String"
  checkSemigroupInstance ""
  trace "instance [Number]"
  checkSemigroupInstance [0]
