{-|
TODO change this stuff
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.Fifo where

-- base
import Data.Coerce
import Data.Kind (Type)
import Data.Proxy
import GHC.Generics (Generic)

-- clash-prelude
-- import qualified Clash.Prelude as C
import Clash.Prelude hiding (pure, not, (||), (&&))

-- me
import Protocols.Axi4.Common hiding (Data)
import Protocols.Internal
import Protocols.DfLike hiding (pure)

-- ADDED:
import Protocols.Axi4.ReadAddress
import Protocols.Axi4.ReadData hiding (pure)
import Protocols.Axi4.WriteAddress
import Protocols.Axi4.WriteData
import Protocols.Axi4.WriteResponse
import Protocols.Df hiding (pure)
import Control.Monad.State

import Prelude hiding (replicate)
import qualified Clash.Explicit.Prelude     as E

-- TODO change all above


-- | Axi4 to Df source
-- TODO change comment
-- * Reading from the given address, pops an item from the fifo
-- * Writes are acknowledged, but ignored
-- * Reading from statusAddress returns how much free space is left in the buffer
-- * Reading/writing to any other address is acknowledged, but the fifo element is not popped.
-- * Asserts err when the FIFO is empty
axi4Source ::
  (1 + n) ~ depth =>
  (depth+1) ~ (1+depth) =>
  HiddenClockResetEnable dom =>
  KnownNat depth =>
  KnownNat (BitSize dat) =>
  KnownNat (Width aw) =>
  KnownNat (Width iw) =>
  NFDataX dat =>
  BitPack dat =>
  NFDataX rdUser =>
  -- | Address to respond to
  BitVector (Width aw) ->
  -- | TODO
  Maybe (BitVector (Width aw)) ->
  -- | Depth of the FIFO
  SNat depth ->
  -- | TODO
  rdUser ->
  -- |
  Circuit
    (Axi4WriteAddress dom waKeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser,
     Axi4WriteData dom 'NoStrobe ((BitSize dat + 7) `Div` 8) wdUser, -- TODO strobe
     Axi4ReadAddress dom raKeepBurst raKeepSize lw iw aw raKeepRegion 'KeepBurstLength raKeepLock raKeepCache raKeepPermissions raKeepQos raUser, -- TODO len
     Reverse (Axi4ReadData dom 'KeepResponse iw rdUser (Index (depth+1))))
    (Df dom dat)
-- TODO keepSize verify the size
-- TODO error if exclusive access, weird burst type, etc is tried
axi4Source respondAddress statusAddress fifoDepth rdUser = Circuit (hideReset circuitFunction) where

  circuitFunction reset ((inpA, inpB, inpC, inpD), inpE) = ((otpA, otpB, otpC, otpD), otpE) where
    brRead = readNew (blockRam (E.replicate fifoDepth $ errorX "axi4Source: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpA, otpB, otpC, otpD, otpE) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB, inpC, inpD, inpE)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  -- (last write address given was respondAddress, number of times left to send status output, status output, data output, amount of space left, next place to read from, next place to write to)
  s0 = let numFree = maxBound
           nextRW = numFree * 0
           -- extremely janky;
           --   forces nextRead and nextWrite to have the same type (that is, Index (depth+1)) as numFree
           --   (since ghc wouldn't be able to tell their type otherwise)
           -- nextRW is initialized to 0, although it could take on any value and the buffer would still work
       in
           (False, 0, S2M_NoReadData, NoData, numFree, nextRW, nextRW)

  fullStateMachine (_,True,_,_,_,_,_) = pure (0, Nothing, S2M_WriteAddress{_awready = False}, S2M_WriteData{_wready = False}, S2M_ReadAddress{_arready = False}, S2M_NoReadData, NoData)
  fullStateMachine (brRead,False,addrM2S,dataM2S,statusAddrM2S,statusAckM2S,Ack ack) = do
    sendData brRead
    (_, _, _, dataOut, _, brReadAddr, _) <- get
    ackA <- processWriteAddress addrM2S
    (brWrite, ackB) <- pushInpData dataM2S
    ackC <- processReadAddress statusAddrM2S
    sendStatus
    (_, _, statusOut, _, _, _, _) <- get
    clearStatus statusAckM2S
    clearData ack
    pure (brReadAddr, brWrite, ackA, ackB, ackC, statusOut, dataOut)

  processWriteAddress M2S_NoWriteAddress = pure (S2M_WriteAddress{_awready = False})
  processWriteAddress addrM2S = do
    (_,b,c,d,e,f,g) <- get
    put (_awaddr addrM2S == respondAddress, b, c, d, e, f, g)
    pure (S2M_WriteAddress{_awready = True})

  pushInpData M2S_NoWriteData = pure (Nothing, S2M_WriteData{_wready = False})
  pushInpData inpDat = do
    (shouldRead,b,c,d,numFree,f,nextWrite) <- get
    if (not shouldRead || numFree == 0) then pure (Nothing, S2M_WriteData{_wready = not shouldRead}) {- TODO comment -} else do
      put (False,b,c,d,numFree-1,f,incIdxLooping nextWrite)
      pure (Just (nextWrite, unpack $ resize $ _wdata inpDat), S2M_WriteData{_wready = True})

  processReadAddress M2S_NoReadAddress = pure (S2M_ReadAddress { _arready = False })
  processReadAddress addrM2S = do
    (a,_,c,d,e,f,g) <- get
    when (Just (_araddr addrM2S) == statusAddress) $ put (a,_arlen addrM2S,c,d,e,f,g)
    pure (S2M_ReadAddress { _arready = True })

  sendStatus = do
    (a,burstLenLeft,statusOut,d,numFree,f,g) <- get
    case (burstLenLeft == 0, statusOut) of
      (False, S2M_NoReadData) -> put (a,burstLenLeft-1,S2M_ReadData { _rdata = numFree, _rid = 0, _rresp = ROkay, _rlast = burstLenLeft == 1, _ruser = rdUser },d,numFree,f,g)
      _ -> pure ()

  sendData brRead = do
    (a,b,c,currOtp,numFree,nextRead,g) <- get
    case (currOtp,numFree == maxBound) of
      (NoData, False) -> put (a, b, c, Data brRead, numFree+1, incIdxLooping nextRead, g)
      _ -> pure ()

  clearStatus ack = when (_rready ack) $ do
    (a,b,_,d,e,f,g) <- get
    put (a, b, S2M_NoReadData, d, e, f, g)

  clearData ack = when ack $ do
    (a,b,c,_,e,f,g) <- get
    put (a, b, c, NoData, e, f, g)

  incIdxLooping idx = if idx == (maxBound-1) then 0 else idx+1

-- | Axi4 to Df sink
-- TODO change comment
-- * Reading from the given address, pops an item from the fifo
-- * Writes are acknowledged, but ignored
-- * Reading from statusAddress returns how much free space is left in the buffer
-- * Reading/writing to any other address is acknowledged, but the fifo element is not popped.
-- * Asserts err when the FIFO is empty
axi4Sink ::
  (1 + n) ~ depth =>
  HiddenClockResetEnable dom =>
  KnownNat depth =>
  KnownNat (BitSize dat) =>
  KnownNat (Width aw) =>
  KnownNat (Width iw) =>
  NFDataX dat =>
  NFDataX rdUser =>
  -- | Address to respond to
  BitVector (Width aw) ->
  -- | TODO
  Maybe (BitVector (Width aw)) ->
  -- | Depth of the FIFO
  SNat depth ->
  -- | TODO
  rdUser -> rdUser -> rdUser ->
  -- |
  Circuit
    (Df dom dat)
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData), -- TODO size
     Axi4ReadData dom 'KeepResponse iw rdUser (Either (Index (depth+1)) dat))
-- TODO error if exclusive access, etc is tried
axi4Sink respondAddress statusAddress fifoDepth rdUserRead rdUserStatus rdUserErr = Circuit (hideReset circuitFunction) where

  circuitFunction reset (inpA, (inpB, inpC)) = (otpA, (otpB, otpC)) where
    brRead = readNew (blockRam (E.replicate fifoDepth $ errorX "axi4Sink: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpA, otpB, otpC) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB, inpC)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  -- (amount left in read burst, axi read data (amt left or data), amount of space left, next place to read from, next place to write to, fifo)
  s0 = let numFree = maxBound
           nextRW = numFree * 0
           -- extremely janky;
           --   forces nextRead and nextWrite to have the same type (that is, Index (depth+1)) as numFree
           --   (since ghc wouldn't be able to tell their type otherwise)
           -- nextRW is initialized to 0, although it could take on any value and the buffer would still work
       in
           (Left 0, S2M_NoReadData, numFree, nextRW, nextRW)

  fullStateMachine (_,True,_,_,_) = pure (0, Nothing, Ack False, S2M_ReadAddress{_arready = False}, S2M_NoReadData{})
  fullStateMachine (brRead,False,inpDat,addrM2S,dataM2S) = do
    ackB <- processAddr addrM2S
    sendData brRead
    (_, dataS2M, _, brReadAddr, _) <- get
    clearData dataM2S
    (brWrite, ackA) <- pushInpData inpDat
    pure (brReadAddr, brWrite, ackA, ackB, dataS2M)

  pushInpData NoData = pure (Nothing, Ack False)
  pushInpData (Data inpDat) = do
    (a,b,numFree,d,nextWrite) <- get
    if numFree == 0 then pure (Nothing, Ack False) else do
      put (a,b,numFree-1,d,incIdxLooping nextWrite)
      pure (Just (nextWrite, inpDat), Ack True)

  processAddr M2S_NoReadAddress = pure (S2M_ReadAddress{_arready = False})
  processAddr addrM2S | _arburst addrM2S /= BmFixed = do
    (_,_,c,d,e) <- get --TODO what if we're sending data?
    put (Left 0, S2M_NoReadData, c, d, e)
    pure (S2M_ReadAddress{_arready = True}) -- TODO what if it's our address? this is fine?
  processAddr addrM2S = do
    (_,_,c,d,e) <- get --TODO what if we're sending data?
    put (pureProcessAddr (_araddr addrM2S) (_arlen addrM2S), S2M_NoReadData, c, d, e)
    pure (S2M_ReadAddress{_arready = True})

  pureProcessAddr addr n | addr == respondAddress = Right n
  pureProcessAddr addr n | Just addr == statusAddress = Left n
  pureProcessAddr _ _ = Left 0

  sendData brRead = do
    (burstLenLeft,currOtp,numFree,nextRead,e) <- get
    case (burstLenLeft, currOtp, numFree == maxBound) of
      (Left 0, _, _) -> pure ()
      (Right 0, _, _) -> pure ()
      (Left n, S2M_NoReadData, _) ->
        put (Left (n-1),
             S2M_ReadData { _rid = 0, _rdata = Left numFree, _rresp = ROkay, _rlast = n == 1, _ruser = rdUserStatus },
             numFree,
             nextRead,
             e)
      (Right n, S2M_NoReadData, False) ->
        put (Right (n-1),
             S2M_ReadData { _rid = 0, _rdata = Right brRead, _rresp = ROkay, _rlast = n == 1, _ruser = rdUserRead },
             numFree+1,
             incIdxLooping nextRead,
             e)
      (Right n, S2M_NoReadData, True) ->
        put (Right (n-1),
             S2M_ReadData { _rid = 0, _rdata = Left numFree, _rresp = RSlaveError, _rlast = n == 1, _ruser = rdUserErr },
             numFree,
             nextRead,
             e)
      _ -> pure ()

  clearData dataM2S = when (_rready dataM2S) $ do
    (a,_,c,d,e) <- get
    put (a,S2M_NoReadData,c,d,e)

  incIdxLooping idx = if idx >= (maxBound-1) then 0 else idx+1
