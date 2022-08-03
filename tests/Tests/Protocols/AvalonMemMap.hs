{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-} -- TODO remove

module Tests.Protocols.AvalonMemMap where

-- base
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C

-- extra
import Data.Proxy (Proxy(..))

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit(HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
import Protocols.Internal
import qualified Protocols.Df as Df
import Protocols.Hedgehog
import qualified Protocols.DfConv as DfConv
import Protocols.Avalon.MemMap.AvalonMemMap

-- tests
import qualified Tests.Protocols.Df as DfTest

---------------------------------------------------------------
---------------------------- TESTS ----------------------------
---------------------------------------------------------------

type SharedConfig = 'AvalonMMSharedConfig 2 'True 'True 2 'True 'True 2 'True 2 'True 'True 'True
type ManagerConfig = 'AvalonMMManagerConfig 'False 'False 'False SharedConfig
type SubordinateConfig = 'AvalonMMSubordinateConfig 'True 'True 'True 'False 'True 'False 'False 'False 'False SharedConfig

genWriteImpt :: Gen (AvalonWriteImpt 'True SharedConfig)
genWriteImpt =
  AvalonWriteImpt
    <$> (toKeepType <$> Gen.enumBounded)
    <*> (toKeepType <$> Gen.enumBounded)
    <*> (toKeepType <$> Gen.enumBounded)
    <*> pure (toKeepType 1)

genReadReqImpt :: Gen (AvalonReadReqImpt 'True SharedConfig)
genReadReqImpt =
  AvalonReadReqImpt
    <$> (toKeepType <$> Gen.enumBounded)
    <*> (toKeepType <$> Gen.enumBounded)
    <*> pure (toKeepType 1)

readImpt :: AvalonReadImpt SharedConfig
readImpt
  = AvalonReadImpt
  { ri_readData = toKeepType 0
  , ri_endOfPacket = toKeepType False
  }

prop_avalon_idc_id_manager :: Property
prop_avalon_idc_id_manager =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData $ (Left <$> genReadReqImpt) C.<|> (Right <$> genWriteImpt))
    id
    (C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $ DfConv.dfConvTestBench Proxy Proxy (repeat True) (repeat (Df.Data readImpt)) ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit
    (AvalonMMManager dom ManagerConfig)
    (AvalonMMManager dom ManagerConfig)
  ckt = DfConv.convert Proxy Proxy

-- TODO also test reverse

prop_avalon_convert_manager_subordinate :: Property
prop_avalon_convert_manager_subordinate =
  DfTest.idWithModelDf
    defExpectOptions
    -- (DfTest.genData $ (Left <$> genReadReqImpt) C.<|> (Right <$> genWriteImpt))
    -- (DfTest.genData $ Left <$> genReadReqImpt)
    (DfTest.genData $ Right <$> genWriteImpt)
    id
    (C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $ DfConv.dfConvTestBench Proxy Proxy (repeat True) (repeat (Df.Data readImpt)) ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit
    (AvalonMMManager dom ManagerConfig)
    (AvalonMMSubordinate dom 0 SubordinateConfig)
  ckt = DfConv.convert Proxy Proxy


tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
