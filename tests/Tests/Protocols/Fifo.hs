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
import qualified Protocols.Axi4.Stream.Axi4Stream as AxStream
import qualified Protocols.Avalon.Stream.AvalonStream as AvStream
import qualified Protocols.Avalon.MemMap.AvalonMemMap as MM

-- tests
import Util
import qualified Tests.Protocols.Df as DfTest

genAxi4StreamByte :: Gen AxStream.Axi4StreamByte
genAxi4StreamByte = Gen.choice [pure AxStream.NullByte, pure AxStream.PositionByte, AxStream.DataByte <$> Gen.integral (Range.linear 0 255)]

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

prop_axi4stream_fifo_id :: Property
prop_axi4stream_fifo_id = propWithModelSingleDomain
                      @C.System
                      defExpectOptions
                      (DfTest.genData (genVec genAxi4StreamByte))
                      (C.exposeClockResetEnable id)
                      (C.exposeClockResetEnable @C.System ckt)
                      (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (AxStream.Axi4Stream dom 1 1 () (C.Vec 10 AxStream.Axi4StreamByte)) (AxStream.Axi4Stream dom 1 1 (C.Index 11) (C.Vec 10 AxStream.Axi4StreamByte))
  ckt = Circuit (fifo (Proxy @(_,_,(C.Vec 10 AxStream.Axi4StreamByte))) Proxy (C.SNat @10) () 0)

prop_df_axi4stream_fifo_id :: Property
prop_df_axi4stream_fifo_id = propWithModelSingleDomain
                      @C.System
                      defExpectOptions
                      (DfTest.genData (genVec genAxi4StreamByte))
                      (C.exposeClockResetEnable id)
                      (C.exposeClockResetEnable @C.System ckt)
                      (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom (C.Vec 10 AxStream.Axi4StreamByte)) (AxStream.Axi4Stream dom 1 1 (C.Index 11) (C.Vec 10 AxStream.Axi4StreamByte))
  ckt = Circuit (fifo (Proxy @(_,_,(C.Vec 10 AxStream.Axi4StreamByte))) Proxy (C.SNat @10) () 0)

prop_axi4stream_df_fifo_id :: Property
prop_axi4stream_df_fifo_id = propWithModelSingleDomain
                      @C.System
                      defExpectOptions
                      (DfTest.genData (genVec genAxi4StreamByte))
                      (C.exposeClockResetEnable id)
                      (C.exposeClockResetEnable @C.System ckt)
                      (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (AxStream.Axi4Stream dom 1 1 () (C.Vec 10 AxStream.Axi4StreamByte)) (Df dom (C.Vec 10 AxStream.Axi4StreamByte))
  ckt = Circuit (fifo (Proxy @(_,_,(C.Vec 10 AxStream.Axi4StreamByte))) Proxy (C.SNat @10) () ())

prop_avalonstream_fifo_id :: Property
prop_avalonstream_fifo_id = propWithModelSingleDomain
                            @C.System
                            defExpectOptions
                            (DfTest.genData DfTest.genSmallInt)
                            (C.exposeClockResetEnable id)
                            (C.exposeClockResetEnable @C.System ckt)
                            (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (AvStream.AvalonStream dom 0 1 1 1 Int) (AvStream.AvalonStream dom 0 1 1 1 Int)
  ckt = Circuit (fifo (Proxy @(_,_,Int)) Proxy (C.SNat @10) () 0)

prop_df_avalonstream_fifo_id :: Property
prop_df_avalonstream_fifo_id = propWithModelSingleDomain
                               @C.System
                               defExpectOptions
                               (DfTest.genData DfTest.genSmallInt)
                               (C.exposeClockResetEnable id)
                               (C.exposeClockResetEnable @C.System ckt)
                               (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (AvStream.AvalonStream dom 0 1 1 1 Int)
  ckt = Circuit (fifo (Proxy @(_,_,Int)) Proxy (C.SNat @10) () 0)

prop_avalonstream_df_fifo_id :: Property
prop_avalonstream_df_fifo_id = propWithModelSingleDomain
                               @C.System
                               defExpectOptions
                               (DfTest.genData DfTest.genSmallInt)
                               (C.exposeClockResetEnable id)
                               (C.exposeClockResetEnable @C.System ckt)
                               (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (AvStream.AvalonStream dom 0 1 1 1 Int) (Df dom Int)
  ckt = Circuit (fifo (Proxy @(_,_,Int)) Proxy (C.SNat @10) () ())

prop_avalonmm_fifo_id :: Property
prop_avalonmm_fifo_id = propWithModelSingleDomain
                        @C.System
                        defExpectOptions
                        (DfTest.genData DfTest.genSmallInt)
                        (C.exposeClockResetEnable id)
                        (C.exposeClockResetEnable @C.System ckt)
                        (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit
                                           (MM.AvalonMMSlave dom 0
                                             ('MM.AvalonMMSlaveConfig 1 'True 'True 'True 'True 'True 'True
                                               ('MM.AvalonMMSharedConfig 1 'True 'True 1 1 'True 'True 'True))
                                             () Int)
                                           (MM.AvalonMMMaster dom
                                             ('MM.AvalonMMMasterConfig 'True 1 1
                                               ('MM.AvalonMMSharedConfig 1 'True 'True 1 1 'True 'True 'True))
                                             () Int)
  ckt = Circuit (fifo (Proxy @(_,_,Int)) Proxy (C.SNat @10) () ())

prop_avalonmm_avalonstream_fifo_id :: Property
prop_avalonmm_avalonstream_fifo_id = propWithModelSingleDomain
                                     @C.System
                                     defExpectOptions
                                     (DfTest.genData DfTest.genSmallInt)
                                     (C.exposeClockResetEnable id)
                                     (C.exposeClockResetEnable @C.System ckt)
                                     (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit
                                           (MM.AvalonMMSlave dom 0
                                             ('MM.AvalonMMSlaveConfig 1 'True 'True 'True 'True 'True 'True
                                               ('MM.AvalonMMSharedConfig 1 'True 'True 1 1 'True 'True 'True))
                                             () Int)
                                           (AvStream.AvalonStream dom 0 1 1 1 Int)
  ckt = Circuit (fifo (Proxy @(_,_,Int)) Proxy (C.SNat @10) () 0)

prop_avalonstream_avalonmm_fifo_id :: Property
prop_avalonstream_avalonmm_fifo_id = propWithModelSingleDomain
                                     @C.System
                                     defExpectOptions
                                     (DfTest.genData DfTest.genSmallInt)
                                     (C.exposeClockResetEnable id)
                                     (C.exposeClockResetEnable @C.System ckt)
                                     (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit
                                           (AvStream.AvalonStream dom 0 1 1 1 Int)
                                           (MM.AvalonMMMaster dom
                                             ('MM.AvalonMMMasterConfig 'True 1 1
                                               ('MM.AvalonMMSharedConfig 1 'True 'True 1 1 'True 'True 'True))
                                             () Int)
  ckt = Circuit (fifo (Proxy @(_,_,Int)) Proxy (C.SNat @10) () ())


tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
