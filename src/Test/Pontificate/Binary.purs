module Test.Pontificate.Binary where

import Test.QuickCheck
import Debug.Trace
import Debug.Spy

type CustomEq a = (a -> a -> Boolean)
type Binary a = (a -> a -> a)

commutative' :: forall a.
  ( Show a )
  => CustomEq a -> Binary a
  -> a -> a -> a -> Result
commutative' (==) (*) a b c =
              -- this shows the same value for a complete test set
  let   x =  (spy a * b * c)
  in   (x == (a * c * b))
    && (x == (b * c * a))
    && (x == (c * b * a))
    && (x == (c * a * b))
    <?> "some things are just not commutative bro, when"
    <> "\n a = " <> show a
    <> "\n b = " <> show b
    <> "\n c = " <> show c
    <> "\n so..."
    <> "\n a * b * c = " <> show (a * b * c)
    <> "\n but like"
    <> "\n a * c * b = " <> show (a * c * b)
    <> "\n b * c * a = " <> show (b * c * a)
    <> "\n c * b * a = " <> show (c * b * a)
    <> "\n c * a * b = " <> show (c * a * b)

commutative :: forall a.
  ( Eq a
  , Show a )
  => Binary a -> a -> a -> a -> Result
commutative = commutative' (==)

associative' :: forall a.
  ( Show a )
  => CustomEq a -> Binary a -> a -> a -> a -> Result
associative' (==) (*) a b c = (((a * b) * c) == (a * (b * c)))
  <?> "its not associative bro, when"
  <>  "\n a = " <> show a
  <>  "\n b = " <> show b
  <>  "\n c = " <> show c
  <>  "\n so..."
  <>  "\n (a * b) * c  = " <> show ( (a * b) * c  )
  <>  "\n  a * (b * c) = " <> show (  a * (b * c) )

associative :: forall a.
  ( Show a
  , Eq a )
  => Binary a -> a -> a -> a -> Result
associative = associative' ((==) :: CustomEq a)

distributive' :: forall a.
  ( Show a )
  => CustomEq a -> Binary a -> Binary a -> a -> a -> a -> Result
distributive' (==) (*) (+) a b c = ( ( a * (b + c)) == ((a * b) + (a * c)) )
                                && ( ((a + b) * c ) == ((a * c) + (b * c)) )
  <?> "Dude, multiplication just won't distribute over addition, when"
  <> "\n a = " <> show a
  <> "\n b = " <> show b
  <> "\n c = " <> show c
  <> "\n so..."
  <> "\n  a * (b + c) = " <> show (a * (b + c))
  <> "\n but like"
  <> "\n (a * b) + (a * c) = " <> show ((a * b) + (a * c))
  <> "\n and so..."
  <> "\n (a + b) * c = " <> show ((a + b) * c)
  <> "\n but then like,"
  <> "\n (a * c) + (b * c) = " <> show ((a * c) + (b * c))

distributive :: forall a.
  ( Show a
  , Eq a )
  => Binary a -> Binary a -> a -> a -> a -> Result
distributive = distributive' (==)
