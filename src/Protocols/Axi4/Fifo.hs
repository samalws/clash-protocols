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


-- | Wishbone to Df sink
--
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
    (Reverse (Axi4ReadAddress dom 'NoBurst 'NoSize lw iw aw 'NoRegion 'NoBurstLength 'NoLock 'NoCache 'NoPermissions 'NoQos dat),
     Axi4ReadData dom 'NoResponse iw () dat)
axi4Sink respondAddress fifoDepth = Circuit (hideReset circuitFunction) where

  circuitFunction reset (inpA, (inpB, inpC)) = (otpA, (otpB, otpC)) where
    (otpA, otpB, otpC) = unbundle $ mealy machineAsFunction s0 $ bundle (unsafeToHighPolarity reset, inpA, inpB, inpC)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  s0 = (False, S2M_NoReadData, replicate fifoDepth NoData)

  fullStateMachine (True,_,_,_) = pure (Ack False, S2M_ReadAddress{_arready = True}, S2M_NoReadData{})
  fullStateMachine (False,inpDat,addrM2S,dataM2S) = do
    ackA <- pushInpData inpDat
    ackB <- processReadAddress addrM2S
    sendDataS2M dataM2S
    (_, dataS2M, _) <- get
    pure (ackA, ackB, dataS2M)

  pushInpData NoData = pure (Ack False)
  pushInpData inpDat@(Data _) = do
    (a,b,buf) <- get
    put (a,b,inpDat +>> buf)
    pure (Ack True)

  processReadAddress M2S_NoReadAddress = pure (S2M_ReadAddress{_arready = False})
  processReadAddress addrM2S = do
    (_,b,c) <- get
    put (_araddr addrM2S == respondAddress, b, c)
    pure (S2M_ReadAddress{_arready = True})

  sendDataS2M dataM2S | _rready dataM2S = do
    (_,_,c) <- get
    put (False, S2M_NoReadData, c)
  sendDataS2M _ = do
    (shouldSend,currOtp,buf) <- get
    case (shouldSend, currOtp, E.head buf) of
      (True, S2M_NoReadData, Data toSend) -> put (False, S2M_ReadData { _rid = 0, _rdata = toSend, _rresp = (), _rlast = True, _ruser = () }, buf <<+ NoData)
      _ -> pure ()
