module Test.Pontificate.Fuzzy where

import Math

infix 4 =~=
infix 4 /=~=

foreign import fuzzyEqNumber
"""
var fuzzyEqNumber = function(x){
  return function(y){
    return x.toFixed(6) == y.toFixed(6);
  };
};
""" :: Number -> Number -> Boolean

(/=~=) :: forall a. (FuzzyEq a) => a -> a -> Boolean
(/=~=) x y = not $ x =~= y

class FuzzyEq a where
  (=~=) :: a -> a -> Boolean

instance fuzzyNumbers :: FuzzyEq Number where
  (=~=) = fuzzyEqNumber
