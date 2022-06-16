{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, UndecidableInstances #-}

module Protocols.Fifo where

import           Prelude hiding (replicate)
import           Control.Monad (when)
import           Control.Monad.State (State, runState, get, put)
import           Clash.Prelude hiding ((&&),(||))
import           Data.Maybe (isJust)
import           Data.Proxy (Proxy(..))

-- me
import           Protocols.Internal
import           Protocols.Df (Data(..))
import           Protocols.Axi4.Common (KeepBurst(..), KeepSize(..), KeepBurstLength(..), KeepResponse(..), Width, BurstMode(..), Resp(..))
import           Protocols.Axi4.ReadAddress (M2S_ReadAddress(..), S2M_ReadAddress(..))
import           Protocols.Axi4.ReadData (M2S_ReadData(..), S2M_ReadData(..))


class (NFDataX (FifoInpState fwd bwd depth), NFDataX (FifoInpDat fwd bwd depth), KnownNat depth) => FifoInput fwd bwd depth where
  type FifoInpState fwd bwd depth
  type FifoInpDat fwd bwd depth
  type FifoInpParam fwd bwd depth
  fifoInpFn :: FifoInpParam fwd bwd depth -> fwd -> Index (depth+1) -> State (FifoInpState fwd bwd depth) (bwd, Maybe (FifoInpDat fwd bwd depth))
  fifoInpS0 :: Proxy (fwd,bwd) -> SNat depth -> FifoInpParam fwd bwd depth -> FifoInpState fwd bwd depth
  fifoInpBlank :: Proxy (fwd,bwd) -> SNat depth -> FifoInpParam fwd bwd depth -> bwd

class (NFDataX (FifoOtpState fwd bwd depth), NFDataX (FifoOtpDat fwd bwd depth), KnownNat depth) => FifoOutput fwd bwd depth where
  type FifoOtpState fwd bwd depth
  type FifoOtpDat fwd bwd depth
  type FifoOtpParam fwd bwd depth
  fifoOtpFn :: FifoOtpParam fwd bwd depth -> bwd -> Index (depth+1) -> FifoOtpDat fwd bwd depth -> State (FifoOtpState fwd bwd depth) (fwd, Bool)
  fifoOtpS0 :: Proxy (fwd,bwd) -> SNat depth -> FifoOtpParam fwd bwd depth -> FifoOtpState fwd bwd depth
  fifoOtpBlank :: Proxy (fwd,bwd) -> SNat depth -> FifoOtpParam fwd bwd depth -> fwd


-- | Generalized fifo
-- * Uses blockram to store data
fifo ::
  HiddenClockResetEnable dom =>
  KnownNat depth =>
  NFDataX dat =>
  FifoInput fwdA bwdA depth =>
  FifoOutput fwdB bwdB depth =>
  FifoInpDat fwdA bwdA depth ~ dat =>
  FifoInpState fwdA bwdA depth ~ sA =>
  FifoOtpDat fwdB bwdB depth ~ dat =>
  FifoOtpState fwdB bwdB depth ~ sB =>
  Proxy (fwdA,bwdA) ->
  Proxy (fwdB,bwdB) ->
  SNat depth ->
  FifoInpParam fwdA bwdA depth ->
  FifoOtpParam fwdB bwdB depth ->
  (Signal dom fwdA, Signal dom bwdB) ->
  (Signal dom bwdA, Signal dom fwdB)
fifo pxyA pxyB fifoDepth paramA paramB = hideReset circuitFunction where

  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset (inpA, inpB) = (otpA, otpB) where
    brRead = readNew (blockRam (replicate fifoDepth $ errorX "fifo: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpA, otpB) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB)

  machineAsFunction _ (_, True, _, _) = (s0, (0, Nothing, fifoInpBlank pxyA fifoDepth paramA, fifoOtpBlank pxyB fifoDepth paramB))
  machineAsFunction (sA,sB,rAddr,wAddr,amtLeft) (brRead, False, iA, iB) =
    let ((oA, maybePush), sA') = runState (fifoInpFn paramA iA amtLeft) sA
        (wAddr', amtLeft') = if (isJust maybePush) then (incIdxLooping wAddr, amtLeft-1) else (wAddr, amtLeft)
        brWrite = (wAddr,) <$> maybePush
        ((oB, popped), sB') = runState (fifoOtpFn paramB iB amtLeft' brRead) sB
        (rAddr', amtLeft'') = if popped then (incIdxLooping rAddr, amtLeft+1) else (rAddr, amtLeft')
        brReadAddr = rAddr'
    in  ((sA', sB', rAddr', wAddr', amtLeft''), (brReadAddr, brWrite, oA, oB))
  -- TODO send otp immediately if we just pushed onto an empty queue

  s0 = (fifoInpS0 pxyA fifoDepth paramA, fifoOtpS0 pxyB fifoDepth paramB, _0 fifoDepth, _0 fifoDepth, _maxBound fifoDepth)

  _0 :: (KnownNat n) => SNat n -> Index n
  _0 = const 0

  _maxBound :: (KnownNat n) => SNat n -> Index (n+1)
  _maxBound = const maxBound

  incIdxLooping idx = if idx == maxBound then 0 else idx+1


instance (NFDataX dat, KnownNat depth) => FifoInput (Data dat) Ack depth where
  type FifoInpState (Data dat) Ack depth = ()
  type FifoInpDat (Data dat) Ack depth = dat
  type FifoInpParam (Data dat) Ack depth = ()
  fifoInpFn _ (Data inp) n | n > 0 = pure (Ack True, Just inp)
  fifoInpFn _ _ _ = pure (Ack False, Nothing)
  fifoInpS0 _ _ _ = ()
  fifoInpBlank _ _ _ = Ack False

instance (NFDataX dat, KnownNat depth) => FifoOutput (Data dat) Ack depth where
  type FifoOtpState (Data dat) Ack depth = Maybe dat
  type FifoOtpDat (Data dat) Ack depth = dat
  type FifoOtpParam (Data dat) Ack depth = ()
  fifoOtpFn _ (Ack ack) amtLeft queueItem = do
    sending <- get
    retVal <- case (sending, amtLeft == maxBound) of
      (Just toSend, _) -> pure (Data toSend, False)
      (Nothing, False) -> put (Just queueItem) >> pure (Data queueItem, True)
      (Nothing, True) -> pure (NoData, False)
    when ack $ put Nothing
    pure retVal
  fifoOtpS0 _ _ _ = Nothing
  fifoOtpBlank _ _ _ = NoData


instance (NFDataX dat, NFDataX rdUser, KnownNat dp1, dp1 ~ (depth + 1), KnownNat (Width aw), KnownNat (Width iw)) =>
  FifoOutput
    (S2M_ReadAddress,
     S2M_ReadData 'KeepResponse iw rdUser (Either (Index dp1) dat))
    (M2S_ReadAddress 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData,
     M2S_ReadData)
    depth
    where
  type FifoOtpState
    (S2M_ReadAddress,
     S2M_ReadData 'KeepResponse iw rdUser (Either (Index dp1) dat))
    (M2S_ReadAddress 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData,
     M2S_ReadData)
    depth
    = (Bool, Index (2^8), BitVector (Width iw), S2M_ReadData 'KeepResponse iw rdUser (Either (Index dp1) dat))
    -- (what we're sending: status (false) or data (true), burst length left, read id, read data currently sending)
  type FifoOtpDat
    (S2M_ReadAddress,
     S2M_ReadData 'KeepResponse iw rdUser (Either (Index dp1) dat))
    (M2S_ReadAddress 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData,
     M2S_ReadData)
    depth
    = dat
  type FifoOtpParam
    (S2M_ReadAddress,
     S2M_ReadData 'KeepResponse iw rdUser (Either (Index dp1) dat))
    (M2S_ReadAddress 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData,
     M2S_ReadData)
    depth
    = (BitVector (Width aw), Maybe (BitVector (Width aw)), rdUser, rdUser, rdUser)
    -- data address, status address, user responses for: fifo item, fifo status, error
  fifoOtpS0 _ _ _ =
    (errorX "FifoOutput for Axi4: No initial value for status vs data",
     0,
     errorX "FifoOutput for Axi4: No initial value for read id",
     S2M_NoReadData)
  fifoOtpBlank _ _ _ = (S2M_ReadAddress { _arready = False }, S2M_NoReadData)
  fifoOtpFn (dataAddr,statusAddr,usrA,usrB,usrC) (addrVal, dataAck) amtLeft popVal = do
    addrAck <- processAddr addrVal
    (dataVal,popped) <- sendData
    processDataAck dataAck
    pure ((addrAck,dataVal),popped)
    where
      processAddr M2S_NoReadAddress = pure (S2M_ReadAddress { _arready = False })
      processAddr M2S_ReadAddress{_arburst} | _arburst /= BmFixed = pure (S2M_ReadAddress{ _arready = True })
      processAddr M2S_ReadAddress{_araddr,_arlen,_arid} = do
        (_,burstLenLeft,_,d) <- get
        when (burstLenLeft == 0 && (_araddr == dataAddr || Just _araddr == statusAddr)) $ put (_araddr == dataAddr, _arlen, _arid, d)
        pure (S2M_ReadAddress{ _arready = burstLenLeft == 0 })

      sendData = do
        (isData,burstLenLeft,readId,currOtp) <- get
        popped <- case (currOtp, isData, burstLenLeft == 0, amtLeft == maxBound) of
          (S2M_NoReadData, True, False, False) -> do
            put (isData, burstLenLeft, readId, S2M_ReadData { _rid = readId, _rdata = Right popVal, _rresp = ROkay, _rlast = burstLenLeft == 1, _ruser = usrA })
            pure True
          (S2M_NoReadData, True, False, True) -> do
            put (isData, burstLenLeft, readId, S2M_ReadData { _rid = readId, _rdata = Left amtLeft, _rresp = RSlaveError, _rlast = burstLenLeft == 1, _ruser = usrC })
            pure False
          (S2M_NoReadData, False, False, _) -> do
            put (isData, burstLenLeft, readId, S2M_ReadData { _rid = readId, _rdata = Left amtLeft, _rresp = ROkay, _rlast = burstLenLeft == 1, _ruser = usrB })
            pure False
          _ -> pure False
        (_,_,_,currOtp') <- get
        pure (currOtp', popped)

      processDataAck M2S_ReadData{_rready} = when _rready $ do
        (a,b,c,_) <- get
        put (a,b,c,S2M_NoReadData)
