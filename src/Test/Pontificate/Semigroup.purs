module Test.Pontificate.Semigroup where

import Debug.Trace
import Test.Pontificate.Binary
import Test.QuickCheck

checkSemigroup' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => CustomEq a -> Binary a -> QC Unit
checkSemigroup' (==) (*) = do
  trace "Semigroup associativity"
  quickCheck $ associative' (==) (*)

checkSemigroup :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => Binary a -> QC Unit
checkSemigroup = checkSemigroup' (==)

checkSemigroupInstance' :: forall a.
  ( Semigroup a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => CustomEq a -> QC Unit
checkSemigroupInstance' (==) = checkSemigroup' (==) (<>)

checkSemigroupInstance :: forall a.
  ( Semigroup a
  , Show a
  , Arbitrary a
  , CoArbitrary a
  , Eq a )
  => a -> QC Unit
checkSemigroupInstance _ = checkSemigroupInstance' ((==) :: CustomEq a)
