module Test.Main where

import Test.Test.Pontificate.Binary
import Test.Test.Pontificate.Semigroup
import Test.Test.Pontificate.Monoid
import Test.Test.Pontificate.Semiring
import Test.Test.Pontificate.Functor
import Test.Test.Pontificate.Apply
import Test.Test.Pontificate.Applicative
import Test.Test.Pontificate.Bind
import Test.Test.Pontificate.Monad

main = do
  checkCheckBinary

  checkCheckSemigroup
  checkCheckMonoid
  checkCheckSemiring

  checkCheckFunctor
  checkCheckApply
  checkCheckApplicative
  checkCheckBind
  checkCheckMonad
  
