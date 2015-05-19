module Test.Test.Pontificate.Functor where

import Debug.Trace
import Data.Maybe
import Test.QuickCheck
import Test.Pontificate.Functor

checkCheckFunctor = do
  trace "Functor [Number]"
  checkFunctor ((<$>) ::
    (Number -> Number) -> [Number] -> [Number])
  trace "Functor Maybe"
  checkFunctorInstance $ Just 0
