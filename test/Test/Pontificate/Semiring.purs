module Test.Test.Pontificate.Semiring where

import Debug.Trace
import Test.QuickCheck
import Test.Pontificate.Fuzzy
import Test.Pontificate.Semiring
import Test.Pontificate.Binary

checkCheckSemiring = do
  trace "Semiring Number"
  checkSemiring' (=~=) (+) 0 (*) 1
  checkSemiringInstance'
    ((=~=) :: Number -> Number -> Boolean)
  trace "Semiring Boolean"
  checkSemiring (&&) true (||) false
