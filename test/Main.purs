module Test.Main where

import Test.Test.Pontificate.Binary
import Test.Test.Pontificate.Semigroup
import Test.Test.Pontificate.Monoid
import Test.Test.Pontificate.Semiring
import Test.Test.Pontificate.Functor
import Test.Test.Pontificate.Apply

main = do
  checkCheckBinary
  checkCheckSemigroup
  checkCheckMonoid
  checkCheckSemiring

  checkCheckFunctor
  checkCheckApply
