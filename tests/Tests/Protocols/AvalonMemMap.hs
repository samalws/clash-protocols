{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-} -- TODO remove

module Tests.Protocols.AvalonMemMap where

-- base
import Prelude
import Data.Maybe (fromMaybe)

-- clash-prelude
import qualified Clash.Prelude as C

-- list
import Data.List (partition, transpose, mapAccumL)

-- containers
import qualified Data.HashMap.Strict as HashMap

-- extra
import Data.Proxy (Proxy(..))

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
import Protocols.Internal
import qualified Protocols.Df as Df
import Protocols.Hedgehog
import qualified Protocols.DfConv as DfConv
import Protocols.Avalon.MemMap.AvalonMemMap

-- tests
import Util
import qualified Tests.Protocols.Df as DfTest

---------------------------------------------------------------
---------------------------- TESTS ----------------------------
---------------------------------------------------------------

type SharedConfig = 'AvalonMMSharedConfig 2 'True 'True 2 'True 'True 2 'True 2 'True 'True 'True
type ManagerConfig = 'AvalonMMManagerConfig 'True 'False 'False SharedConfig
type SubordinateConfig = 'AvalonMMSubordinateConfig 'True 'True 'True 'True 'True 'True 'False 'False 'False SharedConfig

genManagerWriteImpt :: Gen (AvalonManagerWriteImpt ManagerConfig)
genManagerWriteImpt =
  AvalonManagerWriteImpt
    <$> (toKeepType <$> Gen.enumBounded)
    <*> Gen.enumBounded
    <*> (toKeepType <$> Gen.enumBounded)
    <*> pure (toKeepType 1)
    <*> (toKeepType <$> Gen.enumBounded)

genManagerReadReqImpt :: Gen (AvalonManagerReadReqImpt ManagerConfig)
genManagerReadReqImpt =
  AvalonManagerReadReqImpt
    <$> Gen.enumBounded
    <*> (toKeepType <$> Gen.enumBounded)
    <*> pure (toKeepType 1)
    <*> (toKeepType <$> Gen.enumBounded)

convWriteImpt (Right AvalonManagerWriteImpt{..})
  = Right AvalonSubordinateWriteImpt
  { swi_writeData          = mwi_writeData
  , swi_addr               = toKeepType mwi_addr
  , swi_byteEnable         = mwi_byteEnable
  , swi_burstCount         = mwi_burstCount
  }

managerReadImpt :: AvalonManagerReadImpt ManagerConfig
managerReadImpt
  = AvalonManagerReadImpt
  { mri_readData = toKeepType 0
  , mri_endOfPacket = toKeepType False
  }

subordinateReadImpt :: AvalonSubordinateReadImpt SubordinateConfig
subordinateReadImpt
  = AvalonSubordinateReadImpt
  { sri_readData = toKeepType 0
  , sri_endOfPacket = toKeepType False
  }

prop_avalon_idc_id_manager :: Property
prop_avalon_idc_id_manager =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData $ (Left <$> genManagerReadReqImpt) C.<|> (Right <$> genManagerWriteImpt))
    -- (DfTest.genData $ Left <$> genManagerReadReqImpt)
    -- (DfTest.genData $ Right <$> genManagerWriteImpt)
    id
    (C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $ DfConv.dfConvTestBench Proxy Proxy (repeat True) (repeat (Df.Data managerReadImpt)) ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit
    (AvalonMMManager dom ManagerConfig)
    (AvalonMMManager dom ManagerConfig)
  ckt = idC

-- TODO also test reverse

prop_avalon_map_manager_subordinate :: Property
prop_avalon_map_manager_subordinate =
  DfTest.idWithModelDf
    defExpectOptions
    -- (DfTest.genData $ (Left <$> genManagerReadReqImpt) C.<|> (Right <$> genManagerWriteImpt))
    -- (DfTest.genData $ Left <$> genManagerReadReqImpt)
    (DfTest.genData $ Right <$> genManagerWriteImpt)
    (fmap convWriteImpt)
    (C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $ DfConv.dfConvTestBench Proxy Proxy (repeat True) (repeat (Df.Data subordinateReadImpt)) ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit
    (AvalonMMManager dom ManagerConfig)
    (AvalonMMSubordinate dom 0 SubordinateConfig)
  ckt = DfConv.mapBoth Proxy Proxy convWriteImpt undefined

prop_avalon_1_fabric_id :: Property
prop_avalon_1_fabric_id =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData $ Right <$> genManagerWriteImpt)
    (fmap convWriteImpt)
    (C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $ DfConv.dfConvTestBench Proxy Proxy (repeat True) (repeat Df.NoData) ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit
    (AvalonMMManager dom ManagerConfig)
    (AvalonMMSubordinate dom 0 SubordinateConfig)
  ckt = interconnectFabricSingleMember (C.const True) 0 (C.SNat @0)


tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
