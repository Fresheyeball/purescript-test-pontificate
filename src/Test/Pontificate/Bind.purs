module Test.Pontificate.Bind where

import Debug.Trace
import Test.QuickCheck
import Test.Pontificate.Functor
import Test.Pontificate.Binary

type Bind m a b = m a -> (a -> m b) -> m b

checkBind' :: forall m a b c.
  ( Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , CoArbitrary b
  , Show (m a), Show (m c) )
  => CustomEq (m c)
  -> Bind m a b
  -> Bind m b c
  -> Bind m a c
  -> QC Unit
checkBind' (==) bind1 bind2 bind3 = do

  trace "Bind associativity"
  quickCheck associativity

  where
  associativity :: m a -> (a -> m b) -> (b -> m c) -> Result
  associativity x f g = ((x `bind1` f) `bind2` g) == (x `bind3` (\k -> f k `bind2` g))
    <?> "bind associativity, dude, it didn't pass. cuz when"
    <> "\n x = " <> show x
    <> "\n so..."
    <> "\n (x >>= f) >>= g = " <> show ((x `bind1` f) `bind2` g)
    <> "\n but then"
    <> "\n x >>= (λk -> f k >>= g) = " <> show (x `bind3` (\k -> f k `bind2` g))

checkBind :: forall m a b c.
  ( Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , CoArbitrary b
  , Eq (m c)
  , Show (m a), Show (m c) )
  => Bind m a b
  -> Bind m b c
  -> Bind m a c
  -> QC Unit
checkBind = checkBind' (==)

checkBindInstance' :: forall m a b c.
  ( Bind m
  , Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , CoArbitrary b
  , Show (m a), Show (m c) )
  => CustomEq (m c) -> m a -> m b -> QC Unit
checkBindInstance' (==) _ _ = checkBind' (==)
  ((>>=) :: Bind m a b) ((>>=) :: Bind m b c) ((>>=) :: Bind m a c)

checkBindInstance :: forall m a b c.
  ( Bind m
  , Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , CoArbitrary b
  , Eq (m c)
  , Show (m a), Show (m c) )
  => m a -> m b -> m c -> QC Unit
checkBindInstance ma mb _ = checkBindInstance' ((==) :: CustomEq (m c)) ma mb
