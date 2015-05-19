module Test.Pontificate.Applicative where

import Debug.Trace
import Test.QuickCheck
import Test.Pontificate.Functor
import Test.Pontificate.Apply
import Test.Pontificate.Binary

type Pure  f a   = a -> f a
type Category f a b c = (f b c) -> (f a b) -> a -> c

checkApplicative' :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary a, CoArbitrary a
  , Arbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show a, Show (f a), Show (f b), Show (f c) )
  => CustomEq (f a) -> CustomEq (f b) -> CustomEq (f c)
  -> Fmap f c c
  -> Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c
  -> Ap f a a
  -> Ap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a b
  -> Ap f (a -> b) b
  -> Pure f (a -> a)
  -> Pure f (Category (->) a b c)
  -> Pure f b
  -> Pure f a
  -> Pure f (a -> b)
  -> Pure f ((a -> b) -> b)
  -> QC Unit
checkApplicative' (==) (===) (====)
  (<$>) leftFMAPu
  vAPw uAPv _vAPw_ uAP_v idAPv pureleftAPu fAPpurea y_APu
  pureId pureleft pureb purea pureAB pure_X = do

  trace "Applicative <= Apply"
  checkApply' (====) (<$>) leftFMAPu vAPw uAPv _vAPw_ uAP_v
  trace "Applicative composition"
  quickCheck composition
  trace "Applicative identity"
  quickCheck identity
  trace "Applicative homomorphism"
  quickCheck homomorphism
  trace "Applicative interchange"
  quickCheck interchange

  where

  composition :: f (b -> c) -> f (a -> b) -> f a -> Result
  composition u v w = (pureleft (<<<) `pureleftAPu` u `uAPv` v `vAPw` w) ==== (u `uAP_v` (v `_vAPw_` w))
    <?> "Applicative composition, it, did, not, hold, cuz like"
    <> "\n h = " <> show w
    <> "\n pure (<<<) <*> u <*> v <*> w) = "
    <> show (pureleft (<<<) `pureleftAPu` u `uAPv` v `vAPw` w)
    <> "\n but"
    <> "\n u <*> (v <*> w) = " <> show (u `uAP_v` (v `_vAPw_` w))

  identity :: f a -> Result
  identity v = (pureId id `idAPv` v) == (v :: f a)
    <?> "Applicative idenity, its just not true dude, when"
    <> "\n v = " <> show v
    <> "\n cuz"
    <> "\n pure id <*> v = " <> show (pureId id `idAPv` v)

  homomorphism :: (a -> b) -> a -> Result
  homomorphism f x = (pureAB f `fAPpurea` purea x) === ((pureb (f x)) :: f b)
    <?> "Dude, it not an Applicative Homomorphism when"
    <> "\n x = " <> show x
    <> "\n so..."
    <> "\n pure f <*> pure x = " <> show (pureAB f `fAPpurea` purea x)
    <> "\n but like"
    <> "\n pure (f x) = " <> show ((pureb (f x)) :: f b)
    <> "\n no homo"

  interchange :: a -> f (a -> b) -> Result
  interchange y u = (u `fAPpurea` purea y) === (pure_X (\x -> x y) `y_APu` u)
    <?> "Ugh, bro thats totally not an Applicative interchange."
    <> "\n y = " <> show y
    <> "\n and like"
    <> "\n u <*> (pure y) = " <> show (u `fAPpurea` purea y)
    <> "\n except"
    <> "\n (pure ($ y)) <*> u = " <> show (pure_X (\x -> x y) `y_APu` u)

checkApplicative :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary a, CoArbitrary a
  , Arbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show a, Show (f a), Show (f b), Show (f c)
  , Eq (f a), Eq (f b), Eq (f c) )
  => Fmap f c c
  -> Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c
  -> Ap f a a
  -> Ap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a b
  -> Ap f (a -> b) b
  -> Pure f (a -> a)
  -> Pure f (Category (->) a b c)
  -> Pure f b
  -> Pure f a
  -> Pure f (a -> b)
  -> Pure f ((a -> b) -> b)
  -> QC Unit
checkApplicative = checkApplicative' (==) (==) (==)

checkApplicativeInstance' :: forall f a b c fn ap.
  ( Applicative f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary a, CoArbitrary a
  , Arbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show a, Show (f a), Show (f b), Show (f c) )
  => CustomEq (f a) -> CustomEq (f b) -> CustomEq (f c) -> QC Unit
checkApplicativeInstance' (==) (===) (====) = checkApplicative' (==) (===) (====)
  (<$>) (<$>)
  (<*>) (<*>) (<*>) (<*>) (<*>) (<*>) (<*>) (<*>)
  pure pure pure pure pure pure

checkApplicativeInstance :: forall f a b c.
  ( Applicative f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary a, CoArbitrary a
  , Arbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show a, Show (f a), Show (f b), Show (f c)
  , Eq (f a), Eq (f b), Eq (f c) ) => f a -> f b -> f c -> QC Unit
checkApplicativeInstance _ _ _ = checkApplicativeInstance'
  ((==) :: CustomEq (f a)) ((==) :: CustomEq (f b)) ((==) :: CustomEq (f c))
