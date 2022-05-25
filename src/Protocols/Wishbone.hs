{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

module Protocols.Wishbone where

import           Clash.Prelude hiding ((&&), not)

import           Protocols.Internal

import           Protocols.Df (Data, Data(..))

import qualified Clash.Explicit.Prelude as E
import Control.Monad.State (get, gets, modify, put, runState)
import Control.Monad (when)

-- | Data communicated from a Wishbone Master to a Wishbone Slave
data WishboneM2S bytes addressWidth
  = WishboneM2S
  { -- | ADR
    addr                :: "ADR" ::: BitVector addressWidth
    -- | DAT
  , writeData           :: "DAT_MOSI" ::: BitVector (8 * bytes)
    -- | SEL
  , busSelect           :: "SEL" ::: BitVector bytes
    -- | CYC
  , busCycle            :: "CYC" ::: Bool
    -- | STB
  , strobe              :: "STB" ::: Bool
    -- | WE
  , writeEnable         :: "WE" ::: Bool
    -- | CTI
  , cycleTypeIdentifier :: "CTI" ::: CycleTypeIdentifier
    -- | BTE
  , burstTypeExtension  :: "BTE" ::: BurstTypeExtension
  } deriving (Generic, NFDataX, Show, Eq)


-- | Data communicated from a Wishbone Slave to a Wishbone Master
data WishboneS2M bytes
  = WishboneS2M
  { -- | DAT
    readData    :: "DAT_MISO" ::: BitVector (8 * bytes)
    -- | ACK
  , acknowledge :: "ACK"   ::: Bool
    -- | ERR
  , err         :: "ERR"   ::: Bool

    -- | STALL
  , stall       :: "STALL" ::: Bool

    -- | RTY
  , retry       :: "RTY"   ::: Bool
  } deriving (Generic, NFDataX, Show, Eq)

newtype CycleTypeIdentifier = CycleTypeIdentifier (BitVector 3) deriving (Generic, NFDataX, Show, Eq)

pattern Classic, ConstantAddressBurst, IncrementingBurst, EndOfBurst :: CycleTypeIdentifier
pattern Classic = CycleTypeIdentifier 0
pattern ConstantAddressBurst = CycleTypeIdentifier 1
pattern IncrementingBurst = CycleTypeIdentifier 2
pattern EndOfBurst = CycleTypeIdentifier 7

data BurstTypeExtension
  = LinearBurst
  | Beat4Burst
  | Beat8Burst
  | Beat16Burst
  deriving (Generic, NFDataX, Show, Eq)

data WishboneMode
  = Standard
  | Pipelined
  deriving (Generic, Show, Eq)

data Wishbone (dom :: Domain) (mode :: WishboneMode) (bytes :: Nat) (addressWidth :: Nat)

instance Protocol (Wishbone dom mode bytes addressWidth) where
  type Fwd (Wishbone dom mode bytes addressWidth) = Signal dom (WishboneM2S bytes addressWidth)
  type Bwd (Wishbone dom mode bytes addressWidth) = Signal dom (WishboneS2M bytes)


instance (KnownNat bytes) => Backpressure (Wishbone dom mode bytes addressWidth) where
  boolsToBwd _ =  fromList_lazy . Prelude.map (\b -> (wishboneS2M SNat) { acknowledge = b })


wishboneM2S :: SNat bytes -> SNat addressWidth -> WishboneM2S bytes addressWidth
wishboneM2S SNat SNat
  = WishboneM2S
  { addr = Clash.Prelude.undefined
  , writeData = Clash.Prelude.undefined
  , busSelect = Clash.Prelude.undefined
  , busCycle = False
  , strobe = False
  , writeEnable = False
  , cycleTypeIdentifier = Classic
  , burstTypeExtension = LinearBurst
  }

wishboneS2M :: SNat bytes -> WishboneS2M bytes
wishboneS2M SNat
  = WishboneS2M
  { readData = 0
  , acknowledge = False
  , err = False
  , retry = False
  , stall = False
  }

-- | Wishbone to Df source
--
-- * Writing to the given address, pushes an item onto the fifo
-- * Reading always returns zero
-- * Writing to other addresses are acknowledged, but ignored
-- * Asserts stall when the FIFO is full
wishboneSource ::
  (1 + n) ~ depth =>
  HiddenClockResetEnable dom =>
  KnownNat bytes =>
  KnownNat addressWidth =>
  KnownNat depth =>
  -- | Bytes of data
  SNat bytes ->
  -- | Address to respond to
  BitVector addressWidth ->
  -- | Depth of the FIFO
  SNat depth ->
  -- |
  Signal dom (WishboneM2S bytes addressWidth, Ack) ->
  -- |
  Signal dom (WishboneS2M bytes, Data (BitVector (8 * bytes)))
wishboneSource bytes respondAddress fifoDepth = mealy machineAsFunction s0 where

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  s0 = E.replicate fifoDepth NoData

  fullStateMachine (m2s, (Ack ack)) = do
    when ack $ modify (<<+ NoData)
    leftOtp <- leftStateMachine m2s
    rightOtp <- gets E.head
    pure (leftOtp, rightOtp)

  leftStateMachine m2s
    | busCycle m2s && addr m2s == respondAddress && writeEnable m2s = pushInput (writeData m2s)
    | busCycle m2s = pure ((wishboneS2M bytes) { acknowledge = True })
    | otherwise = pure (wishboneS2M bytes)

  pushInput inpData = do
    buf <- get
    if (E.last buf /= NoData) then pure ((wishboneS2M bytes) { stall = True }) else do
      put (Data inpData +>> buf)
      pure ((wishboneS2M bytes) { acknowledge = True })

-- | Wishbone to Df sink
--
-- * Reading from the given address, pops an item from the fifo
-- * Writes are acknowledged, but ignored
-- * Reads from any other address are acknowledged, but the fifo element is not popped.
-- * Asserts stall when the FIFO is empty
wishboneSink ::
  (1 + n) ~ depth =>
  HiddenClockResetEnable dom =>
  KnownNat bytes =>
  KnownNat addressWidth =>
  KnownNat depth =>
  -- | Bytes of data
  SNat bytes ->
  -- | Address to respond to
  BitVector addressWidth ->
  -- | Depth of the FIFO
  SNat depth ->
  -- |
  Signal dom (WishboneM2S bytes addressWidth, Data (BitVector (8 * bytes))) ->
  -- |
  Signal dom (WishboneS2M bytes, Ack)
wishboneSink bytes respondAddress fifoDepth = mealy machineAsFunction s0 where

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  s0 = E.replicate fifoDepth NoData

  fullStateMachine (m2s,inpData) = (,) <$> leftStateMachine m2s <*> rightStateMachine inpData

  leftStateMachine m2s
    | busCycle m2s && addr m2s == respondAddress && not (writeEnable m2s) = popOutput
    | busCycle m2s = pure ((wishboneS2M bytes) { acknowledge = True })
    | otherwise = pure (wishboneS2M bytes)

  rightStateMachine NoData = pure (Ack False)
  rightStateMachine inpData@(Data _) = do
    buf <- get
    if (E.last buf /= NoData) then pure (Ack False) else do
      put (inpData +>> buf)
      pure (Ack True)

  popOutput = do
    buf <- get
    case (E.head buf) of
      NoData -> pure ((wishboneS2M bytes) { stall = True })
      (Data toSend) -> do
        put (buf <<+ NoData)
        pure ((wishboneS2M bytes) { acknowledge = True, readData = toSend })
