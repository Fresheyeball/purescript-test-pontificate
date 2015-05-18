module Test.Test.Pontificate.Binary where

import Debug.Trace
import Test.QuickCheck
import Test.Pontificate.Binary
import Test.Pontificate.Fuzzy

checkCheckAssociative =
  let
    checkFuzzyNum = quickCheck <<< associative' ((=~=) :: Number -> Number -> Boolean)
    checkNotFuzzyNum = quickCheck <<< associative' ((/=~=) :: Number -> Number -> Boolean)
    checkAssoc = quickCheck <<< associative
  in do
    trace "*"
    checkFuzzyNum (*)
    trace "+"
    checkFuzzyNum (+)
    trace "-"
    checkNotFuzzyNum (-)
    trace "&&"
    checkAssoc ((&&) :: Boolean -> Boolean -> Boolean)
    trace "||"
    checkAssoc ((||) :: Boolean -> Boolean -> Boolean)

checkCheckCommutiative = do
  trace "=="
  quickCheck $ commutative ((==) :: Boolean -> Boolean -> Boolean)
  trace "&&"
  quickCheck $ commutative ((&&) :: Boolean -> Boolean -> Boolean)
  trace "||"
  quickCheck $ commutative ((||) :: Boolean -> Boolean -> Boolean)
  trace "+"
  quickCheck $ commutative ((*) :: Number -> Number -> Number)
  trace "*"
  quickCheck $ commutative ((+) :: Number -> Number -> Number)
  trace "-"
  quickCheck $ commutative' (/=) ((-) :: Number -> Number -> Number)

checkCheckDistributive = do
  trace "* over +"
  quickCheck $ distributive' (=~=) (*) ((+) :: Number -> Number -> Number)

checkCheckBinary = do
  checkCheckAssociative
  checkCheckCommutiative
  checkCheckDistributive
