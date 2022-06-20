{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Hashable (Index n)

module Tests.Protocols.Fifo where

-- base
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=))

-- extra
import Data.Proxy (Proxy(..))

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
import Protocols.Hedgehog
import Protocols.Fifo (fifo)
import qualified Protocols.Axi4.Stream.Axi4Stream as Stream

-- tests
import Util
import qualified Tests.Protocols.Df as DfTest

genAxi4StreamByte :: Gen Stream.Axi4StreamByte
genAxi4StreamByte = Gen.choice [pure Stream.NullByte, pure Stream.PositionByte, Stream.DataByte <$> Gen.integral (Range.linear 0 255)]

instance (Hashable t, C.KnownNat n, 1 <= n, 1 <= (n C.- 1), 1 <= ((n C.- 1) C.- 1)) => Hashable (C.Vec n t)

---------------------------------------------------------------
---------------------------- TESTS ----------------------------
---------------------------------------------------------------
prop_df_fifo_id :: Property
prop_df_fifo_id = propWithModelSingleDomain
               @C.System
               defExpectOptions
               (DfTest.genData DfTest.genSmallInt)
               (C.exposeClockResetEnable id)
               (C.exposeClockResetEnable @C.System ckt)
               (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int)
  ckt = Circuit (fifo (Proxy @(_, _, Int)) Proxy (C.SNat @10) () ())

prop_stream_fifo_id :: Property
prop_stream_fifo_id = propWithModelSingleDomain
                      @C.System
                      defExpectOptions
                      (DfTest.genData (genVec genAxi4StreamByte))
                      (C.exposeClockResetEnable id)
                      (C.exposeClockResetEnable @C.System ckt)
                      (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Stream.Axi4Stream dom 1 1 () (C.Vec 10 Stream.Axi4StreamByte)) (Stream.Axi4Stream dom 1 1 (C.Index 11) (C.Vec 10 Stream.Axi4StreamByte))
  ckt = Circuit (fifo (Proxy @(_,_,(C.Vec 10 Stream.Axi4StreamByte))) Proxy (C.SNat @10) () 0)

prop_df_stream_fifo_id :: Property
prop_df_stream_fifo_id = propWithModelSingleDomain
                      @C.System
                      defExpectOptions
                      (DfTest.genData (genVec genAxi4StreamByte))
                      (C.exposeClockResetEnable id)
                      (C.exposeClockResetEnable @C.System ckt)
                      (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom (C.Vec 10 Stream.Axi4StreamByte)) (Stream.Axi4Stream dom 1 1 (C.Index 11) (C.Vec 10 Stream.Axi4StreamByte))
  ckt = Circuit (fifo (Proxy @(_,_,(C.Vec 10 Stream.Axi4StreamByte))) Proxy (C.SNat @10) () 0)

prop_stream_df_fifo_id :: Property
prop_stream_df_fifo_id = propWithModelSingleDomain
                      @C.System
                      defExpectOptions
                      (DfTest.genData (genVec genAxi4StreamByte))
                      (C.exposeClockResetEnable id)
                      (C.exposeClockResetEnable @C.System ckt)
                      (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Stream.Axi4Stream dom 1 1 () (C.Vec 10 Stream.Axi4StreamByte)) (Df dom (C.Vec 10 Stream.Axi4StreamByte))
  ckt = Circuit (fifo (Proxy @(_,_,(C.Vec 10 Stream.Axi4StreamByte))) Proxy (C.SNat @10) () ())


tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
