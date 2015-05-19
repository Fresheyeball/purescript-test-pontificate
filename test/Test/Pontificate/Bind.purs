module Test.Test.Pontificate.Bind where

import Debug.Trace
import Data.Maybe
import Test.QuickCheck
import Test.Pontificate.Bind

checkCheckBind = do
  trace "Bind Maybe"
  checkBindInstance (Just 0) (Just "foo") (Just false)
