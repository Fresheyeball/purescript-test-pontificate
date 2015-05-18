module Test.Test.Pontificate.Binary where

import Debug.Trace
import Test.QuickCheck
import Test.Pontificate.Binary
import Test.Pontificate.Fuzzy

checkCheckAssociative = do
    trace "*"
    quickCheck $ associative' (=~=) ((*) :: Number -> Number -> Number)
    trace "+"
    quickCheck $ associative' (=~=) ((+) :: Number -> Number -> Number)
    trace "&&"
    quickCheck $ associative ((&&) :: Boolean -> Boolean -> Boolean)
    trace "||"
    quickCheck $ associative ((||) :: Boolean -> Boolean -> Boolean)

checkCheckCommutiative = do
  trace "=="
  quickCheck $ commutative ((==) :: Boolean -> Boolean -> Boolean)
  trace "&&"
  quickCheck $ commutative ((&&) :: Boolean -> Boolean -> Boolean)
  trace "||"
  quickCheck $ commutative ((||) :: Boolean -> Boolean -> Boolean)
  trace "*"
  quickCheck $ commutative' (=~=) ((*) :: Number -> Number -> Number)
  trace "+"
  quickCheck $ commutative' (=~=) ((+) :: Number -> Number -> Number)

checkCheckDistributive = do
  trace "* over +"
  quickCheck $ distributive' (=~=) (*) ((+) :: Number -> Number -> Number)
  trace "&& over ||"
  quickCheck $ distributive (&&) ((||) :: Boolean -> Boolean -> Boolean)

checkCheckBinary = do
  checkCheckAssociative
  checkCheckCommutiative
  checkCheckDistributive
