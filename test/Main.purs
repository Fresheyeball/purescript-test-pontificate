module Test.Main where

import Debug.Trace
import Control.Apply
import Test.QuickCheck
import Test.Pontificate.Binary
import Test.Pontificate.Fuzzy

checkCheckAssociative =
  let
    checkFuzzyNum = quickCheck <<< associative' ((=~=) :: Number -> Number -> Boolean)
    checkNotFuzzyNum = quickCheck <<< associative' ((/=~=) :: Number -> Number -> Boolean)
    checkAssoc = quickCheck <<< associative
  in do
    trace "*" *> checkFuzzyNum (*)
    trace "+" *> checkFuzzyNum (+)
    trace "-" *> checkNotFuzzyNum (-)
    trace "&&" *> checkAssoc ((&&) :: Boolean -> Boolean -> Boolean)
    trace "||" *> checkAssoc ((||) :: Boolean -> Boolean -> Boolean)

main = do
  checkCheckAssociative
