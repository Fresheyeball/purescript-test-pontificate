module Test.Pontificate.Monoid where

import Debug.Trace
import Data.Monoid
import Test.QuickCheck
import Test.Pontificate.Binary
import Test.Pontificate.Semigroup

type Id a = a

checkMonoid' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => CustomEq a -> Binary a -> Id a -> QC Unit
checkMonoid' (==) (*) identity' = do
  trace "Monoid identity"
  quickCheck identity
  trace "Semigroup <= Monoid"
  checkSemigroup' (==) (*)

  where

  identity :: a -> Result
  identity a = (a == (a         * identity'))
            && (a == (identity' * a))
    <?> "Identity, it totally didn't hold, when"
    <> "\n a = " <> show a
    <> "\n identity = " <> show identity'
    <> "\n so..."
    <> "\n a * identity = " <> show (a * identity')
    <> "\n but like"
    <> "\n identity * a = " <> show (identity' * a)

checkMonoid :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => Binary a -> Id a -> QC Unit
checkMonoid = checkMonoid' (==)

checkMonoidInstance' :: forall a.
  ( Monoid a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => CustomEq a -> QC Unit
checkMonoidInstance' (==) = checkMonoid' (==) (<>) mempty

checkMonoidInstance :: forall a.
  ( Monoid a
  , Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => a -> QC Unit
checkMonoidInstance _ = checkMonoid'
  ((==) :: CustomEq a) (<>) mempty

checkCommutativeMonoid' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => CustomEq a -> Binary a -> Id a -> QC Unit
checkCommutativeMonoid' (==) (+) identity = do
  trace "CommutativeMonoid <= Monoid"
  checkMonoid' (==) (+) identity
  trace "CommutativeMonoid <= Commutative"
  quickCheck $ commutative' (==) (+)

checkCommutativeMonoid :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => Binary a -> Id a -> QC Unit
checkCommutativeMonoid = checkCommutativeMonoid' (==)
