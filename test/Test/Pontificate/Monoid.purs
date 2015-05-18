module Test.Test.Pontificate.Monoid where

import Debug.Trace
import Data.Monoid
import Data.Monoid.Additive
import Test.QuickCheck
import Test.Pontificate.Fuzzy
import Test.Pontificate.Monoid

instance arbAdditive :: Arbitrary (Additive Number) where
  arbitrary = Additive <$> arbitrary

instance coarbAdditive :: CoArbitrary (Additive Number) where
  coarbitrary (Additive n) = coarbitrary n

instance fuzzyAdditive :: FuzzyEq (Additive Number) where
  (=~=) (Additive x) (Additive y) = x =~= y

eqArrow :: forall a. (Eq a) => a -> (a -> a) -> (a -> a) -> Boolean
eqArrow a f f' = f a == f' a

instance showNumArrow :: Show (String -> String) where
  show f = let e = "a" in e <> " -> " <> show (f e)

checkCheckMonoid = do
  trace "++ ''"
  checkMonoid (++) ""
  trace "<<< id"
  checkMonoid' (eqArrow "b") (<<<) id
  trace "instance (Additive Number)"
  checkMonoidInstance' ((=~=) :: Additive Number -> Additive Number -> Boolean)
  trace "+ 0"
  checkCommutativeMonoid' (=~=) (+) 0
  trace "* 1"
  checkCommutativeMonoid' (=~=) (*) 1
