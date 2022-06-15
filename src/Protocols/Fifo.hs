{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Protocols.Fifo where

import           Prelude hiding (replicate)
import           Control.Monad.State (State, runState)
import           Clash.Prelude
import           Data.Maybe (isJust)
import           Data.Proxy (Proxy(..))

-- me
import           Protocols.Internal


class (NFDataX (FifoInpState fwd bwd), NFDataX (FifoInpDat fwd bwd)) => FifoInput fwd bwd where
  type FifoInpState fwd bwd
  type FifoInpDat fwd bwd
  fifoInpFn :: (KnownNat depth) => fwd -> Index (depth+1) -> State (FifoInpState fwd bwd) (bwd, Maybe (FifoInpDat fwd bwd))
  fifoInpS0 :: (KnownNat depth) => Proxy (fwd,bwd) -> SNat depth -> FifoInpState fwd bwd
  fifoInpBlank :: (KnownNat depth) => Proxy (fwd,bwd) -> SNat depth -> bwd

class (NFDataX (FifoOtpState fwd bwd), NFDataX (FifoOtpDat fwd bwd)) => FifoOutput fwd bwd where
  type FifoOtpState fwd bwd
  type FifoOtpDat fwd bwd
  fifoOtpFn :: (KnownNat depth) => bwd -> Index (depth+1) -> FifoOtpDat fwd bwd -> State (FifoOtpState fwd bwd) (fwd, Bool)
  fifoOtpS0 :: (KnownNat depth) => Proxy (fwd,bwd) -> SNat depth -> FifoOtpState fwd bwd
  fifoOtpBlank :: (KnownNat depth) => Proxy (fwd,bwd) -> SNat depth -> fwd


-- | Generalized fifo
-- * Uses blockram to store data
fifo ::
  HiddenClockResetEnable dom =>
  KnownNat fifoDepth =>
  NFDataX dat =>
  FifoInput fwdA bwdA =>
  FifoOutput fwdB bwdB =>
  FifoInpDat fwdA bwdA ~ dat =>
  FifoInpState fwdA bwdA ~ sA =>
  FifoOtpDat fwdB bwdB ~ dat =>
  FifoOtpState fwdB bwdB ~ sB =>
  Protocol tA =>
  Protocol tB =>
  Fwd tA ~ Signal dom fwdA =>
  Bwd tA ~ Signal dom bwdA =>
  Fwd tB ~ Signal dom fwdB =>
  Bwd tB ~ Signal dom bwdB =>
  Proxy (fwdA,bwdA) ->
  Proxy (fwdB,bwdB) ->
  SNat fifoDepth ->
  Circuit tA tB
fifo pxyA pxyB fifoDepth = Circuit (hideReset circuitFunction) where

  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset (inpA, inpB) = (otpA, otpB) where
    brRead = readNew (blockRam (replicate fifoDepth $ errorX "generalizedFifo: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpA, otpB) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB)

  machineAsFunction _ (_, True, _, _) = (s0, (0, Nothing, fifoInpBlank pxyA fifoDepth, fifoOtpBlank pxyB fifoDepth))
  machineAsFunction (sA,sB,rAddr,wAddr,amtLeft) (brRead, False, iA, iB) =
    let ((oA, maybePush), sA') = runState (fifoInpFn iA amtLeft) sA
        (wAddr', amtLeft') = if (isJust maybePush) then (incIdxLooping wAddr, amtLeft-1) else (wAddr, amtLeft)
        brWrite = (wAddr,) <$> maybePush
        ((oB, popped), sB') = runState (fifoOtpFn iB amtLeft' brRead) sB
        (rAddr', amtLeft'') = if popped then (incIdxLooping rAddr, amtLeft+1) else (rAddr, amtLeft')
        brReadAddr = rAddr'
    in  ((sA', sB', rAddr', wAddr', amtLeft''), (brReadAddr, brWrite, oA, oB))

  s0 = (fifoInpS0 pxyA fifoDepth, fifoOtpS0 pxyB fifoDepth, _0 fifoDepth, _0 fifoDepth, _maxBound fifoDepth)

  _0 :: (KnownNat n) => SNat n -> Index n
  _0 = const 0

  _maxBound :: (KnownNat n) => SNat n -> Index (n+1)
  _maxBound = const maxBound

  incIdxLooping idx = if idx == maxBound then 0 else idx+1
