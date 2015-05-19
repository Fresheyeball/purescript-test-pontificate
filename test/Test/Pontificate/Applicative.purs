module Test.Test.Pontificate.Applicative where

import Debug.Trace
import Data.Maybe
import Test.QuickCheck
import Test.Pontificate.Applicative

checkCheckApplicative = do
  trace "Applicative Maybe"
  checkApplicativeInstance
    (Just 0) (Just "foo") (Just false)
  trace "Applicative List"
  checkApplicativeInstance
    ["foo"] [0] [true]
