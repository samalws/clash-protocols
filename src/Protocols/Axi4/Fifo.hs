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
import Clash.Prelude hiding (pure, not, (&&))

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
    (otpA, otpB, otpC, otpD, otpE) = unbundle $ mealy machineAsFunction s0 $ bundle (unsafeToHighPolarity reset, inpA, inpB, inpC, inpD, inpE)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  s0 = (False, S2M_NoReadData, NoData, maxBound, replicate fifoDepth NoData)

  fullStateMachine (True,_,_,_,_,_) = pure (S2M_WriteAddress{_awready = False}, S2M_WriteData{_wready = False}, S2M_ReadAddress{_arready = False}, S2M_NoReadData, NoData)
  fullStateMachine (False,addrM2S,dataM2S,statusAddrM2S,statusAckM2S,Ack ack) = do
    ackA <- processAddress addrM2S
    ackB <- pushInpData dataM2S
    ackC <- sendStatus statusAddrM2S
    sendData
    (_, statusOut, dataOut, _, _) <- get
    clearStatus statusAckM2S
    clearData ack
    pure (ackA, ackB, ackC, statusOut, dataOut)

  processAddress M2S_NoWriteAddress = pure (S2M_WriteAddress{_awready = False})
  processAddress addrM2S = do
    (_,b,c,d,e) <- get
    put (_awaddr addrM2S == respondAddress, b, c, d, e)
    pure (S2M_WriteAddress{_awready = True})

  pushInpData M2S_NoWriteData = pure (S2M_WriteData{_wready = False})
  pushInpData inpDat = do
    (shouldRead,b,c,numFree,buf) <- get
    when (shouldRead && numFree > 0) $ put (False,b,c,numFree-1,Data (unpack $ resize $ _wdata inpDat) +>> buf)
    pure (S2M_WriteData{_wready = numFree > 0})

  sendStatus M2S_NoReadAddress = pure (S2M_ReadAddress { _arready = False })
  sendStatus ack = do
    (a,statusOut,c,numFree,e) <- get
    case (Just (_araddr ack) == statusAddress, statusOut) of
      (True, S2M_NoReadData) -> put (a,S2M_ReadData { _rdata = numFree, _rid = 0, _rresp = (), _rlast = True, _ruser = () },c,numFree,e)
      _ -> pure ()
    pure (S2M_ReadAddress { _arready = True })

  sendData = do
    (a,b,currOtp,numFree,buf) <- get
    case (currOtp,numFree == maxBound) of
      (NoData, False) -> put (a, b, E.head buf, numFree+1, buf <<+ NoData)
      _ -> pure ()

  clearStatus ack = when (_rready ack) $ do
    (a,_,c,d,e) <- get
    put (a, S2M_NoReadData, c, d, e)

  clearData ack = when ack $ do
    (a,b,_,d,e) <- get
    put (a, b, NoData, d, e)

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
    (otpA, otpB, otpC) = unbundle $ mealy machineAsFunction s0 $ bundle (unsafeToHighPolarity reset, inpA, inpB, inpC)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  s0 = (S2M_NoReadData, maxBound, replicate fifoDepth NoData)

  fullStateMachine (True,_,_,_) = pure (Ack False, S2M_ReadAddress{_arready = False}, S2M_NoReadData{})
  fullStateMachine (False,inpDat,addrM2S,dataM2S) = do
    ackA <- pushInpData inpDat
    ackB <- sendData addrM2S
    (dataS2M, _, _) <- get
    clearData dataM2S
    pure (ackA, ackB, dataS2M)

  pushInpData NoData = pure (Ack False)
  pushInpData inpDat@(Data _) = do
    (a,numFree,buf) <- get
    if numFree == 0 then pure (Ack False) else do
      put (a,numFree-1,inpDat +>> buf)
      pure (Ack True)

  sendData M2S_NoReadAddress = pure (S2M_ReadAddress{_arready = False})
  sendData addrM2S = do
    (currOtp,numFree,buf) <- get
    case (_araddr addrM2S == respondAddress, Just (_araddr addrM2S) == statusAddress, currOtp, E.head buf) of
      (True, _, S2M_NoReadData, Data toSend) ->
        put (S2M_ReadData { _rid = 0, _rdata = Right toSend, _rresp = (), _rlast = True, _ruser = () }, numFree+1, buf <<+ NoData)
      (_, True, S2M_NoReadData, _) ->
        put (S2M_ReadData { _rid = 0, _rdata = Left numFree, _rresp = (), _rlast = True, _ruser = () }, numFree, buf)
      _ -> pure ()
    pure (S2M_ReadAddress{_arready = True})

  clearData dataM2S = when (_rready dataM2S) $ do
    (_,b,c) <- get
    put (S2M_NoReadData,b,c)
