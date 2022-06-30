{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MonomorphismRestriction #-}

module Tests.Protocols.AvalonMM where

-- base
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (mapAccumL)
import GHC.Stack (HasCallStack)
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=))

-- containers
import qualified Data.HashMap.Strict as HashMap

-- extra
import Data.List (transpose, partition)

-- deepseq
import Control.DeepSeq (NFData)

-- hashable
import Data.Hashable (Hashable)

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit(HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
import qualified Protocols.Avalon.MemMap.AvalonMemMap as MM
import Protocols.Hedgehog

-- tests
import Util
import qualified Tests.Protocols.Df as DfTest

---------------------------------------------------------------
---------------------------- TESTS ----------------------------
---------------------------------------------------------------

prop_avalon_fabric_id :: Property
prop_avalon_fabric_id = propWithModelSingleDomain
                        @C.System
                        defExpectOptions
                        (DfTest.genData DfTest.genSmallInt)
                        (C.exposeClockResetEnable id)
                        (C.exposeClockResetEnable @C.System ckt')
                        (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit
    (C.Vec 1 (MM.AvalonMMMaster dom
      ('MM.AvalonMMMasterConfig 'True 1 1
        ('MM.AvalonMMSharedConfig 1 'True 'True 1 1 'True 'True 'True))
      () Int))
    (C.Vec 1 (MM.AvalonMMSlave dom 0
      ('MM.AvalonMMSlaveConfig 1 'True 'True 'True 'True 'True 'True
        ('MM.AvalonMMSharedConfig 1 'True 'True 1 1 'True 'True 'True))
      () Int))
  ckt = MM.avalonInterconnectFabric (const True C.:> C.Nil) (0 C.:> C.Nil) (C.SNat @0)

  ckt' :: (C.HiddenClockResetEnable dom) => Circuit
    (MM.AvalonMMMaster dom
      ('MM.AvalonMMMasterConfig 'True 1 1
        ('MM.AvalonMMSharedConfig 1 'True 'True 1 1 'True 'True 'True))
      () Int)
    (MM.AvalonMMSlave dom 0
      ('MM.AvalonMMSlaveConfig 1 'True 'True 'True 'True 'True 'True
        ('MM.AvalonMMSharedConfig 1 'True 'True 1 1 'True 'True 'True))
      () Int)
  ckt' = Circuit (\(a,b) -> let Circuit cf = ckt in let (c C.:> C.Nil, d C.:> C.Nil) = cf (a C.:> C.Nil, b C.:> C.Nil) in (c,d))


tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
