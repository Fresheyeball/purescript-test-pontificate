module Test.Pontificate.Semiring where

import Debug.Trace
import Data.Monoid
import Test.QuickCheck
import Test.Pontificate.Binary
import Test.Pontificate.Monoid

checkSemiring' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => CustomEq a -> Binary a -> a -> Binary a -> a -> QC Unit
checkSemiring' (==) (+) zero (*) one = do
  trace "Semiring <= CommutativeMonoid + 0"
  checkCommutativeMonoid' (==) (+) (zero :: a)
  trace "Semiring <= Monoid * 1"
  checkMonoid' (==) (*) (one :: a)
  trace "Semiring annihilate"
  quickCheck annihilate
  trace "Semiring distributive"
  quickCheck $ distributive' (==) (*) ((+) :: a -> a -> a)

  where

  annihilate :: a -> Result
  annihilate a = (zero == (a * zero))
              && (zero == (zero * a))
    <?> "It totally didn't annihilate, when"
    <> "\n a = " <> show a
    <> "\n zero = " <> show (zero :: a)
    <> "\n so..."
    <> "\n a * zero = " <> show (a * zero)
    <> "\n but like"
    <> "\n zero * a = " <> show (zero * a)

checkSemiring :: forall a.
  ( Arbitrary a
  , CoArbitrary a
  , Show a
  , Eq a )
  => Binary a -> a -> Binary a -> a -> QC Unit
checkSemiring = checkSemiring' ((==) :: CustomEq a)

checkSemiringInstance' :: forall a.
  ( Semiring a
  , Arbitrary a
  , CoArbitrary a
  , Show a )
  => CustomEq a -> QC Unit
checkSemiringInstance' (==) = checkSemiring' (==) (+) zero (*) one

checkSemiringInstance :: forall a.
  ( Semiring a
  , Arbitrary a
  , CoArbitrary a
  , Show a
  , Eq a )
  => a -> QC Unit
checkSemiringInstance _ = checkSemiringInstance' ((==) :: CustomEq a)
