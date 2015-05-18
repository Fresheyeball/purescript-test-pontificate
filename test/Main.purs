module Test.Main where

import Test.Test.Pontificate.Binary
import Test.Test.Pontificate.Semigroup
import Test.Test.Pontificate.Monoid

main = do
  checkCheckBinary
  checkCheckSemigroup
  checkCheckMonoid
