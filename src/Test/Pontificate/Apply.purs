module Test.Pontificate.Apply where

import Debug.Trace
import Test.QuickCheck
import Test.Pontificate.Functor
import Test.Pontificate.Binary

type Ap f a b = f (a -> b) -> f a -> f b

checkApply' :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show (f a), Show (f b), Show (f c) )
  => CustomEq (f c)
  -> Fmap f c c
  -> Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c
  -> QC Unit
checkApply' (==) (<$>) leftFMAPu vAPw uAPv _vAPw_ uAP_v = do

  trace "Functor <= Apply"
  checkFunctor' (==) (<$>)
  trace "Apply composition"
  quickCheck composition

  where

  composition :: f (b -> c) -> f (a -> b) -> f a -> Result
  composition u v w = ((<<<) `leftFMAPu` u `uAPv` v `vAPw` w) == (u `uAP_v` (v `_vAPw_` w))
    <?> "Apply composition, it, did, not, hold, cuz like"
    <> "\n w = " <> show w
    <> "\n and if you think about it"
    <> "\n (<<<) <$> u <*> v <*> w = " <> show (((<<<) `leftFMAPu` u `uAPv` v `vAPw` w))
    <> "\n but,"
    <> "\n u <*> (v <*> w) = " <> show (u `uAP_v` (v `_vAPw_` w))

checkApply :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show (f a), Show (f b), Show (f c)
  , Eq (f c) )
  => Fmap f c c
  -> Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c
  -> QC Unit
checkApply = checkApply' (==)

checkApplyInstance' :: forall f a b c.
  ( Apply f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show (f a), Show (f b), Show (f c) )
  => CustomEq (f c) -> f a -> f b -> QC Unit
checkApplyInstance' (==) _ _ = checkApply' (==)
  ((<$>) :: Fmap f c c)
  ((<$>) :: Fmap f (b -> c) ((a -> b) -> a -> c))
  ((<*>) :: Ap f a c)
  ((<*>) :: Ap f (a -> b) (a -> c))
  ((<*>) :: Ap f a b)
  ((<*>) :: Ap f b c)

checkApplyInstance :: forall f a b c.
  ( Apply f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show (f a), Show (f b), Show (f c)
  , Eq (f c) )
  => f c -> f a -> f b -> QC Unit
checkApplyInstance _ = checkApplyInstance' ((==) :: CustomEq (f c))
