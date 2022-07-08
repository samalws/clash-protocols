module Tests.Protocols (tests, main) where

import Prelude
import Test.Tasty

import qualified Tests.Protocols.Df
import qualified Tests.Protocols.DfLikeAlternative

tests :: TestTree
tests = testGroup "Protocols"
  [ Tests.Protocols.Df.tests
  , Tests.Protocols.DfLikeAlternative.tests
  ]

main :: IO ()
main = defaultMain tests
