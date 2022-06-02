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
import Clash.Prelude hiding (pure, not)

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
  HiddenClockResetEnable dom =>
  KnownNat depth =>
  KnownNat (BitSize dat) =>
  KnownNat (Width aw) =>
  KnownNat (Width iw) =>
  NFDataX dat =>
  BitPack dat =>
  -- | Address to respond to
  BitVector (Width aw) ->
  -- | Depth of the FIFO
  SNat depth ->
  -- |
  Circuit
    (Axi4WriteAddress dom 'NoBurst 'NoSize lw iw aw 'NoRegion 'NoBurstLength 'NoLock 'NoCache 'NoPermissions 'NoQos (),
     Axi4WriteData dom 'NoStrobe ((BitSize dat + 7) `Div` 8) ())
    (Df dom dat)
axi4Source respondAddress fifoDepth = Circuit (hideReset circuitFunction) where

  circuitFunction reset ((inpA, inpB), inpC) = ((otpA, otpB), otpC) where
    (otpA, otpB, otpC) = unbundle $ mealy machineAsFunction s0 $ bundle (unsafeToHighPolarity reset, inpA, inpB, inpC)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  s0 = (NoData, replicate fifoDepth NoData)

  fullStateMachine (True,_,_,_) = pure (S2M_WriteAddress{_awready = False}, S2M_WriteData{_wready = False}, NoData)
  fullStateMachine (False,addrM2S,dataM2S,Ack ack) = do
    ackA <- processAddress addrM2S
    ackB <- pushInpData dataM2S
    sendData
    (dataOut, _) <- get
    clearData ack
    pure (ackA, ackB, dataOut)

  processAddress M2S_NoWriteAddress = pure (S2M_WriteAddress{_awready = False})
  processAddress addrM2S = do
    (_,b,c) <- get
    put (_awaddr addrM2S == respondAddress, b, c)
    pure (S2M_WriteAddress{_awready = True})

  pushInpData M2S_NoWriteData = pure (S2M_WriteData{_wready = False})
  pushInpData inpDat = do
    (shouldRead,b,buf) <- get
    when shouldRead $ put (False,b,Data (unpack $ resize $ _wdata inpDat) +>> buf)
    pure (S2M_WriteData{_wready = True})

  sendData = do
    (a,currOtp,buf) <- get
    case currOtp of
      NoData -> put (a, E.head buf, buf <<+ NoData)
      _ -> pure ()

  clearData ack = when ack $ do
    (a,_,c) <- get
    put (a, NoData, c)

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
  -- | Depth of the FIFO
  SNat depth ->
  -- |
  Circuit
    (Df dom dat)
    (Reverse (Axi4ReadAddress dom 'NoBurst 'NoSize lw iw aw 'NoRegion 'NoBurstLength 'NoLock 'NoCache 'NoPermissions 'NoQos ()),
     Axi4ReadData dom 'NoResponse iw () dat)
axi4Sink respondAddress fifoDepth = Circuit (hideReset circuitFunction) where

  circuitFunction reset (inpA, (inpB, inpC)) = (otpA, (otpB, otpC)) where
    (otpA, otpB, otpC) = unbundle $ mealy machineAsFunction s0 $ bundle (unsafeToHighPolarity reset, inpA, inpB, inpC)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  s0 = (S2M_NoReadData, replicate fifoDepth NoData)

  fullStateMachine (True,_,_,_) = pure (Ack False, S2M_ReadAddress{_arready = False}, S2M_NoReadData{})
  fullStateMachine (False,inpDat,addrM2S,dataM2S) = do
    ackA <- pushInpData inpDat
    ackB <- sendData addrM2S
    (dataS2M, _) <- get
    clearData dataM2S
    pure (ackA, ackB, dataS2M)

  pushInpData NoData = pure (Ack False)
  pushInpData inpDat@(Data _) = do
    (a,buf) <- get
    put (a,inpDat +>> buf)
    pure (Ack True)

  sendData M2S_NoReadAddress = pure (S2M_ReadAddress{_arready = False})
  sendData addrM2S = do
    (currOtp,buf) <- get
    case (_araddr addrM2S == respondAddress, currOtp, E.head buf) of
      (True, S2M_NoReadData, Data toSend) -> put (S2M_ReadData { _rid = 0, _rdata = toSend, _rresp = (), _rlast = True, _ruser = () }, buf <<+ NoData)
      _ -> pure ()
    pure (S2M_ReadAddress{_arready = True})

  clearData dataM2S = when (_rready dataM2S) $ do
    (_,b) <- get
    put (S2M_NoReadData, b)
