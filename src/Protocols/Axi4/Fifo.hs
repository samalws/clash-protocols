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
import Clash.Prelude hiding (pure, not, (||))

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
  -- | Address to respond to
  BitVector (Width aw) ->
  -- | TODO
  Maybe (BitVector (Width aw)) ->
  -- | Depth of the FIFO
  SNat depth ->
  -- |
  Circuit
    (Axi4WriteAddress dom 'NoBurst 'NoSize lw iw aw 'NoRegion 'NoBurstLength 'NoLock 'NoCache 'NoPermissions 'NoQos (),
     Axi4WriteData dom 'NoStrobe ((BitSize dat + 7) `Div` 8) (),
     Axi4ReadAddress dom 'NoBurst 'NoSize lw iw aw 'NoRegion 'NoBurstLength 'NoLock 'NoCache 'NoPermissions 'NoQos (),
     Reverse (Axi4ReadData dom 'NoResponse iw () (Index (depth+1))))
    (Df dom dat)
axi4Source respondAddress statusAddress fifoDepth = Circuit (hideReset circuitFunction) where

  circuitFunction reset ((inpA, inpB, inpC, inpD), inpE) = ((otpA, otpB, otpC, otpD), otpE) where
    brRead = readNew (blockRam (E.replicate fifoDepth $ errorX "axi4Source: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpA, otpB, otpC, otpD, otpE) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB, inpC, inpD, inpE)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  -- (last write address given was respondAddress, status output, data output, amount of space left, next place to read from, next place to write to)
  s0 = let numFree = maxBound
           nextRW = numFree * 0
           -- extremely janky;
           --   forces nextRead and nextWrite to have the same type (that is, Index (depth+1)) as numFree
           --   (since ghc wouldn't be able to tell their type otherwise)
           -- nextRW is initialized to 0, although it could take on any value and the buffer would still work
       in
           (False, S2M_NoReadData, NoData, numFree, nextRW, nextRW)

  fullStateMachine (_,True,_,_,_,_,_) = pure (0, Nothing, S2M_WriteAddress{_awready = False}, S2M_WriteData{_wready = False}, S2M_ReadAddress{_arready = False}, S2M_NoReadData, NoData)
  fullStateMachine (brRead,False,addrM2S,dataM2S,statusAddrM2S,statusAckM2S,Ack ack) = do
    sendData brRead
    (_, statusOut, dataOut, _, brReadAddr, _) <- get
    ackA <- processAddress addrM2S
    (brWrite, ackB) <- pushInpData dataM2S
    ackC <- sendStatus statusAddrM2S
    clearStatus statusAckM2S
    clearData ack
    pure (brReadAddr, brWrite, ackA, ackB, ackC, statusOut, dataOut)

  processAddress M2S_NoWriteAddress = pure (S2M_WriteAddress{_awready = False})
  processAddress addrM2S = do
    (_,b,c,d,e,f) <- get
    put (_awaddr addrM2S == respondAddress, b, c, d, e, f)
    pure (S2M_WriteAddress{_awready = True})

  pushInpData M2S_NoWriteData = pure (Nothing, S2M_WriteData{_wready = False})
  pushInpData inpDat = do
    (shouldRead,b,c,numFree,e,nextWrite) <- get
    if (not shouldRead || numFree == 0) then pure (Nothing, S2M_WriteData{_wready = not shouldRead}) {- TODO comment -} else do
      put (False,b,c,numFree-1,e,incIdxLooping nextWrite)
      pure (Just (nextWrite, unpack $ resize $ _wdata inpDat), S2M_WriteData{_wready = True})

  sendStatus M2S_NoReadAddress = pure (S2M_ReadAddress { _arready = False })
  sendStatus ack = do
    (a,statusOut,c,numFree,e,f) <- get
    case (Just (_araddr ack) == statusAddress, statusOut) of
      (True, S2M_NoReadData) -> put (a,S2M_ReadData { _rdata = numFree, _rid = 0, _rresp = (), _rlast = True, _ruser = () },c,numFree,e,f)
      _ -> pure ()
    pure (S2M_ReadAddress { _arready = True })

  sendData brRead = do
    (a,b,currOtp,numFree,nextRead,f) <- get
    case (currOtp,numFree == maxBound) of
      (NoData, False) -> put (a, b, Data brRead, numFree+1, incIdxLooping nextRead, f)
      _ -> pure ()

  clearStatus ack = when (_rready ack) $ do
    (a,_,c,d,e,f) <- get
    put (a, S2M_NoReadData, c, d, e, f)

  clearData ack = when ack $ do
    (a,b,_,d,e,f) <- get
    put (a, b, NoData, d, e, f)

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
  -- | Address to respond to
  BitVector (Width aw) ->
  -- | TODO
  Maybe (BitVector (Width aw)) ->
  -- | Depth of the FIFO
  SNat depth ->
  -- |
  Circuit
    (Df dom dat)
    (Reverse (Axi4ReadAddress dom 'NoBurst 'NoSize lw iw aw 'NoRegion 'NoBurstLength 'NoLock 'NoCache 'NoPermissions 'NoQos ()),
     Axi4ReadData dom 'NoResponse iw () (Either (Index (depth+1)) dat))
axi4Sink respondAddress statusAddress fifoDepth = Circuit (hideReset circuitFunction) where

  circuitFunction reset (inpA, (inpB, inpC)) = (otpA, (otpB, otpC)) where
    brRead = readNew (blockRam (E.replicate fifoDepth $ errorX "axi4Sink: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpA, otpB, otpC) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB, inpC)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  -- (axi read data (amt left or data), amount of space left, next place to read from, next place to write to, fifo)
  s0 = let numFree = maxBound
           nextRW = numFree * 0
           -- extremely janky;
           --   forces nextRead and nextWrite to have the same type (that is, Index (depth+1)) as numFree
           --   (since ghc wouldn't be able to tell their type otherwise)
           -- nextRW is initialized to 0, although it could take on any value and the buffer would still work
       in
           (S2M_NoReadData, numFree, nextRW, nextRW)

  fullStateMachine (_,True,_,_,_) = pure (0, Nothing, Ack False, S2M_ReadAddress{_arready = False}, S2M_NoReadData{})
  fullStateMachine (brRead,False,inpDat,addrM2S,dataM2S) = do
    ackB <- sendData addrM2S brRead
    (dataS2M, _, brReadAddr, _) <- get
    clearData dataM2S
    (brWrite, ackA) <- pushInpData inpDat
    pure (brReadAddr, brWrite, ackA, ackB, dataS2M)

  pushInpData NoData = pure (Nothing, Ack False)
  pushInpData (Data inpDat) = do
    (a,numFree,c,nextWrite) <- get
    if numFree == 0 then pure (Nothing, Ack False) else do
      put (a,numFree-1,c,incIdxLooping nextWrite)
      pure (Just (nextWrite, inpDat), Ack True)

  sendData M2S_NoReadAddress _ = pure (S2M_ReadAddress{_arready = False})
  sendData addrM2S brRead = do
    (currOtp,numFree,nextRead,d) <- get
    case (_araddr addrM2S == respondAddress, Just (_araddr addrM2S) == statusAddress, currOtp, numFree == maxBound) of
      (True, _, S2M_NoReadData, False) ->
        put (S2M_ReadData { _rid = 0, _rdata = Right brRead, _rresp = (), _rlast = True, _ruser = () }, numFree+1, incIdxLooping nextRead, d)
      (_, True, S2M_NoReadData, _) ->
        put (S2M_ReadData { _rid = 0, _rdata = Left numFree, _rresp = (), _rlast = True, _ruser = () }, numFree, nextRead, d)
      _ -> pure ()
    pure (S2M_ReadAddress{_arready = True})

  clearData dataM2S = when (_rready dataM2S) $ do
    (_,b,c,d) <- get
    put (S2M_NoReadData,b,c,d)

  incIdxLooping idx = if idx == (maxBound-1) then 0 else idx+1
