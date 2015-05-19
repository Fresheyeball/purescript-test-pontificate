module Test.Test.Pontificate.Monad where

import Debug.Trace
import Data.Maybe
import Test.QuickCheck
import Test.Pontificate.Monad

checkCheckMonad = do
  trace "Monad Maybe"
  checkMonadInstance (Just 0) (Just "foo") (Just false)
  trace "Monad List"
  checkMonadInstance ["foo"] [true] [0]
  trace "Monad Maybe the hard way"
  checkMonad'
    ((==) :: Maybe Number -> Maybe Number -> Boolean)
    ((==) :: Maybe String -> Maybe String -> Boolean)
    ((==) :: Maybe Boolean -> Maybe Boolean -> Boolean)
    (<$>) (<$>)
    (<*>) (<*>) (<*>) (<*>) (<*>) (<*>) (<*>) (<*>)
    pure pure pure pure pure pure pure
    (>>=) (>>=) (>>=) (>>=) (>>=)
