module Test.Pontificate.Functor where

import Debug.Trace
import Test.QuickCheck
import Test.Pontificate.Binary

type Fmap f a b = (a -> b) -> f a -> f b

checkFunctor' :: forall f a.
  ( Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a)
  , Show (f a) )
  => CustomEq (f a) -> Fmap f a a -> QC Unit
checkFunctor' (==) (<$>) = do
  trace "Functor identity"
  quickCheck identity
  trace "Functor associativity"
  quickCheck associativity

  where

  identity :: f a -> Result
  identity f = id <$> f == id f
    <?> "oh no bro! functor identity isn't cool when"
    <> "\n f a = " <> show f
    <> "\n so..."
    <> "\n id <$> f = " <> show (id <$> f)
    <> "\n except"
    <> "\n id f = " <> show (id f)

  associativity :: f a -> (a -> a) -> (a -> a) -> Result
  associativity f p q = (p <<< q) <$> f == ((<$>) p <<< (<$>) q) f
    <?> "no way, functor associativity won't work when"
    <> "\n f = " <> show f
    <> "\n cuz like"
    <> "\n (p <<< q) <$> f = " <> show ((p <<< q) <$> f)
    <> "\n but then"
    <> "\n ((<$>) p <<< (<$>) q) f = " <> show (((<$>) p <<< (<$>) q) f)

checkFunctor :: forall f a.
  ( Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a)
  , Eq (f a)
  , Show (f a) )
  => Fmap f a a -> QC Unit
checkFunctor = checkFunctor' (==)

checkFunctorInstance' :: forall f a.
  ( Functor f
  , Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a)
  , Show (f a) )
  => CustomEq (f a) -> QC Unit
checkFunctorInstance' (==) = checkFunctor' (==) (<$>)

checkFunctorInstance :: forall f a.
  ( Functor f
  , Eq (f a)
  , Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a)
  , Show (f a) )
  => f a -> QC Unit
checkFunctorInstance _ = checkFunctorInstance' ((==) :: CustomEq (f a))
