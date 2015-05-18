module Test.Fuzzy where

infix 4 =~=
infix 4 /=~=

(/=~=) :: forall a. (FuzzyEq a) => a -> a -> Boolean
(/=~=) x y = not $ x =~= y

class FuzzyEq a where
  (=~=) :: a -> a -> Boolean

instance fuzzyNumbers :: FuzzyEq Number where
  (=~=) x y = (y - x) <= epsilon && (y - x) >= (-epsilon)
    where
    epsilon = 0.00000001
