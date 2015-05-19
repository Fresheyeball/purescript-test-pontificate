module Test.Pontificate.Monad where

import Test.Pontificate.Binary
import Test.Pontificate.Functor
import Test.Pontificate.Apply
import Test.Pontificate.Applicative
import Test.Pontificate.Bind
import Test.QuickCheck
import Debug.Trace

checkMonad' :: forall m a b c.
  ( Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , Arbitrary b, CoArbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (m (a -> b))
  , Arbitrary (m (b -> c))
  , Show a, Show c, Show (m a), Show (m b), Show (m c) )
  => CustomEq (m a) -> CustomEq (m b) -> CustomEq (m c)
  -> Fmap m c c
  -> Fmap m (b -> c) ((a -> b) -> a -> c)
  -> Ap m a c
  -> Ap m (a -> b) (a -> c)
  -> Ap m a b
  -> Ap m b c
  -> Ap m a a
  -> Ap m (b -> c) ((a -> b) -> a -> c)
  -> Ap m a b
  -> Ap m (a -> b) b
  -> Pure m (a -> a)
  -> Pure m (Category (->) a b c)
  -> Pure m a
  -> Pure m b
  -> Pure m c
  -> Pure m (a -> b)
  -> Pure m ((a -> b) -> b)
  -> Bind m a a
  -> Bind m a b
  -> Bind m a c
  -> Bind m b c
  -> Bind m c c
  -> QC Unit
checkMonad'
  (==) (===) (====)
  (<$>) leftFMAPu
  vAPw uAPv _vAPw_ uAP_v idAPv pureleftAPu fAPpurea y_APu
  pureId pureleft purea pureb purec pureAB pure_X
  bindaa bindab bindac bindbc bindcc = do

  trace "Monad <= Applicative"
  checkApplicative'
    (==) (===) (====)
    (<$>) leftFMAPu
    vAPw uAPv _vAPw_ uAP_v idAPv pureleftAPu fAPpurea y_APu
    pureId pureleft pureb purea pureAB pure_X

  trace "Monad <= Bind"
  checkBind' (====) bindab bindbc bindac

  trace "Monad leftIdentity"
  quickCheck leftIdentity
  trace "Monad rightIdentity"
  quickCheck rightIdentity

  where

  leftIdentity :: c -> (c -> m c) -> Result
  leftIdentity x f = (purec x `bindcc` f) ==== (f x)
    <?> "woah, no left id on Monad bro, when"
    <> "\n x = " <> show x
    <> "\n and like"
    <> "\n return x >>= f = " <> show (purec x `bindcc` f)
    <> "\n but totally"
    <> "\n f x = " <> show (f x)

  rightIdentity :: m a -> Result
  rightIdentity m = (m `bindaa` purea) == m
    <?> "woah, no right id on Monad bro, when"
    <> "\n m = " <> show m
    <> "\n but like"
    <> "\n m >>= return = " <> show (m `bindaa` purea)

checkMonad :: forall m a b c.
  ( Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , Arbitrary b, CoArbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (m (a -> b))
  , Arbitrary (m (b -> c))
  , Show a, Show c, Show (m a), Show (m b), Show (m c)
  , Eq (m a), Eq (m b), Eq (m c) )
  => Fmap m c c
  -> Fmap m (b -> c) ((a -> b) -> a -> c)
  -> Ap m a c
  -> Ap m (a -> b) (a -> c)
  -> Ap m a b
  -> Ap m b c
  -> Ap m a a
  -> Ap m (b -> c) ((a -> b) -> a -> c)
  -> Ap m a b
  -> Ap m (a -> b) b
  -> Pure m (a -> a)
  -> Pure m (Category (->) a b c)
  -> Pure m a
  -> Pure m b
  -> Pure m c
  -> Pure m (a -> b)
  -> Pure m ((a -> b) -> b)
  -> Bind m a a
  -> Bind m a b
  -> Bind m a c
  -> Bind m b c
  -> Bind m c c
  -> QC Unit
checkMonad = checkMonad' (==) (==) (==)

checkMonadInstance' :: forall m a b c.
  ( Monad m
  , Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , Arbitrary b, CoArbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (m (a -> b))
  , Arbitrary (m (b -> c))
  , Show a, Show c, Show (m a), Show (m b), Show (m c) )
  => CustomEq (m a) -> CustomEq (m b) -> CustomEq (m c) -> QC Unit
checkMonadInstance' (==) (===) (====) = checkMonad'
  (==) (===) (====)
  (<$>) (<$>)
  (<*>) (<*>) (<*>) (<*>) (<*>) (<*>) (<*>) (<*>)
  pure pure pure pure pure pure pure
  (>>=) (>>=) (>>=) (>>=) (>>=)

checkMonadInstance :: forall m a b c.
  ( Monad m
  , Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , Arbitrary b, CoArbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (m (a -> b))
  , Arbitrary (m (b -> c))
  , Show a, Show c, Show (m a), Show (m b), Show (m c)
  , Eq (m a), Eq (m b), Eq (m c) )
  => m a -> m b -> m c -> QC Unit
checkMonadInstance _ _ _ = checkMonadInstance'
  ((==) :: CustomEq (m a))
  ((==) :: CustomEq (m b))
  ((==) :: CustomEq (m c))
