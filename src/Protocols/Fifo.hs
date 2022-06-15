{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Protocols.Fifo where

import           Prelude hiding (replicate)
import           Data.Maybe (isJust)
import           Control.Monad.State (State, runState)
import           Clash.Prelude

-- me
import           Protocols.Internal



-- | Generalized fifo
-- * Uses blockram to store data
generalizedFifo ::
  HiddenClockResetEnable dom =>
  KnownNat depth =>
  NFDataX dat =>
  NFDataX sA =>
  NFDataX sB =>
  Protocol tA =>
  Protocol tB =>
  Fwd tA ~ Signal dom fwdA =>
  Bwd tA ~ Signal dom bwdA =>
  Fwd tB ~ Signal dom fwdB =>
  Bwd tB ~ Signal dom bwdB =>
  (fwdA -> (Index (depth+1)) -> State sA (bwdA, Maybe dat)) ->
  sA -> bwdA ->
  (bwdB -> (Index (depth+1)) -> dat -> State sB (fwdB, Bool)) ->
  sB -> fwdB ->
  -- | Depth of the fifo
  SNat depth ->
  -- |
  Circuit tA tB
-- fA :: iA -> space left -> State sA (oA,Maybe push)
-- fB :: iB -> space left -> pop -> State sB (oB,popped)
generalizedFifo fA sA0 defA fB sB0 defB fifoDepth = Circuit (hideReset circuitFunction) where

  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset (inpA, inpB) = (otpA, otpB) where
    brRead = readNew (blockRam (replicate fifoDepth $ errorX "generalizedFifo: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpA, otpB) = unbundle $ mealy machineAsFunction (s0 sA0 sB0 fifoDepth) $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB)

  machineAsFunction _ (_, True, _, _) = (s0 sA0 sB0 fifoDepth, (0, Nothing, defA, defB))
  machineAsFunction (sA,sB,rAddr,wAddr,amtLeft) (brRead, False, iA, iB) =
    let ((oA, maybePush), sA') = runState (fA iA amtLeft) sA
        (wAddr', amtLeft') = if (isJust maybePush) then (incIdxLooping wAddr, amtLeft-1) else (wAddr, amtLeft)
        brWrite = (wAddr,) <$> maybePush
        ((oB, popped), sB') = runState (fB iB amtLeft' brRead) sB
        (rAddr', amtLeft'') = if popped then (incIdxLooping rAddr, amtLeft+1) else (rAddr, amtLeft')
        brReadAddr = rAddr'
    in  ((sA', sB', rAddr', wAddr', amtLeft''), (brReadAddr, brWrite, oA, oB))

  s0 :: (KnownNat n) => sA -> sB -> SNat n -> (sA, sB, Index n, Index n, Index (n+1))
  s0 _sA0 _sB0 _ = (_sA0, _sB0, 0, 0, maxBound)

  incIdxLooping idx = if idx == maxBound then 0 else idx+1
