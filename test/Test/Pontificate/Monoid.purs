module Test.Test.Pontificate.Monoid where

import Debug.Trace
import Debug.Spy
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

instance showNumArrow :: Show (Number -> Number) where
  show f = let e = 12345 in (show e) <> " -> " <> show (f e)

instance showStringArrow :: Show (String -> String) where
  show f = let e = "12345" in e <> " -> " <> f e

stringSpy :: String -> (String -> String) -> Boolean
stringSpy s ar = f (spy $ s <> " -> " <> ar s)
  where f _ = true

checkCheckMonoid = do
  trace "++ ''"
  checkMonoid (++) ""
  trace "instance (Additive Number)"
  checkMonoidInstance' ((=~=) :: Additive Number -> Additive Number -> Boolean)
  trace "+ 0"
  checkCommutativeMonoid' (=~=) (+) 0
  trace "* 1"
  checkCommutativeMonoid' (=~=) (*) 1
  trace "<<< id"
  -- this should not pass but it does
  checkCommutativeMonoid' (eqArrow "5") (<<<) id
  -- this shows different values for the test set
  quickCheck stringSpy
