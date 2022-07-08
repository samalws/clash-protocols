{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Hashable (Index n)

module Tests.Protocols.DfLikeAlternative where

-- base
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C

-- extra
import Data.Proxy (Proxy(..))

-- hedgehog
import Hedgehog

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit(HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
import Protocols.Internal
import Protocols.Hedgehog
import qualified Protocols.DfLikeAlternative as DfLikeAlt

-- tests
import Util
import qualified Tests.Protocols.Df as DfTest

undoDoubleReverseInp :: Circuit (Reverse (Reverse a)) b -> Circuit a b
undoDoubleReverseInp = coerceCircuit

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
  ckt = undoDoubleReverseInp $ DfLikeAlt.fifo (Proxy, ()) (Proxy, ()) (C.SNat @10)

prop_df_map_inc :: Property
prop_df_map_inc = DfTest.idWithModelDf' (fmap (+ 1)) (C.withClockResetEnable C.clockGen C.resetGen C.enableGen ckt) where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int)
  ckt = undoDoubleReverseInp $ DfLikeAlt.map (Proxy, ()) (Proxy, ()) (+1)

prop_df_filter_over_5 :: Property
prop_df_filter_over_5 = DfTest.idWithModelDf' (filter (> 5)) (C.withClockResetEnable C.clockGen C.resetGen C.enableGen ckt) where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int)
  ckt = undoDoubleReverseInp $ DfLikeAlt.filter (Proxy, ()) (Proxy, ()) (> 5)

prop_df_mapmaybe_inc_over_5 :: Property
prop_df_mapmaybe_inc_over_5 = DfTest.idWithModelDf' (map (+ 1) . filter (> 5)) (C.withClockResetEnable C.clockGen C.resetGen C.enableGen ckt) where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int)
  ckt = undoDoubleReverseInp $ DfLikeAlt.mapMaybe (Proxy, ()) (Proxy, ()) (\n -> if n > 5 then Just (n+1) else Nothing)

prop_df_zipwith_add :: Property
prop_df_zipwith_add =
  idWithModel
    defExpectOptions
    ( do
        as <- DfTest.genData DfTest.genSmallInt
        bs <- DfTest.genData DfTest.genSmallInt
        let n = min (length as) (length bs)
        pure (take n as, take n bs) )
    (uncurry (zipWith (+)))
    (C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen ckt)
  where
  ckt_ :: (C.HiddenClockResetEnable dom) => Circuit (Reverse (Reverse (Df dom Int), Reverse (Df dom Int))) (Df dom Int)
  ckt_ = DfLikeAlt.zipWith ((Proxy, ()), (Proxy, ())) (Proxy, ()) (+)
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int, Df dom Int) (Df dom Int)
  ckt = let Circuit f = ckt_ in Circuit f

prop_fanout1 :: Property
prop_fanout1 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genData DfTest.genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable ckt)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (C.Vec 1 (Df dom Int))
  ckt = undoDoubleReverseInp $ DfLikeAlt.fanout (Proxy, ()) (C.repeat (Proxy, ()))

prop_fanout2 :: Property
prop_fanout2 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genData DfTest.genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable ckt)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (C.Vec 2 (Df dom Int))
  ckt = undoDoubleReverseInp $ DfLikeAlt.fanout (Proxy, ()) (C.repeat (Proxy, ()))

prop_fanout7 :: Property
prop_fanout7 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genData DfTest.genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable ckt)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (C.Vec 7 (Df dom Int))
  ckt = undoDoubleReverseInp $ DfLikeAlt.fanout (Proxy, ()) (C.repeat (Proxy, ()))


tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
