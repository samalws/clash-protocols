{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Protocols.Axi4.Stream.Axi4Stream where

-- base
import           Control.DeepSeq (NFData)
import           Prelude hiding ()

import qualified Data.Maybe as Maybe
import           Data.Proxy
import qualified Prelude as P

-- clash-prelude
import           Clash.Prelude
import qualified Clash.Prelude as C

-- me
import           Protocols.Internal
import           Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

-- | Each byte sent along an AXI4 Stream can either be
-- a data byte, a position byte, or a null byte.
-- The value of position and null bytes should be ignored.
-- Additionally, null bytes can be added or dropped.
data Axi4StreamByte = DataByte (Unsigned 8)
                    | PositionByte
                    | NullByte
                    deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

-- | Data sent from master to slave.
-- dataType should only ever be @Vec n Axi4StreamByte@
-- for some n, but is left open-ended in order to
-- be able to implement DfLike.
-- The tvalid field is left out: messages with
-- @tvalid = False@ should be sent as a @NoAxi4StreamM2S@.
-- The tdata, tstrb, and tkeep fields are all grouped
-- in the @_tdata@ field in this datatype (see @Axi4StreamByte@).
data Axi4StreamM2S idWidth destWidth userType dataType
  = NoAxi4StreamM2S
  | Axi4StreamM2S
  {
    _tdata :: dataType,
    _tlast :: Bool,
    _tid   :: Unsigned idWidth,
    _tdest :: Unsigned destWidth,
    _tuser :: userType
  }
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

-- | Data sent from slave to master.
-- A simple acknowledge message.
data Axi4StreamS2M = Axi4StreamS2M { tReady :: Bool } deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

-- | Type for AXI4 Stream protocol.
data Axi4Stream (dom :: Domain) (idWidth :: Nat) (destWidth :: Nat) (userType :: Type) (dataType :: Type)

-- | @dataType = Vec n Axi4StreamByte@ is enforced here
instance (dataType ~ Vec dataLen Axi4StreamByte) => Protocol (Axi4Stream dom idWidth destWidth userType dataType) where
  type Fwd (Axi4Stream dom idWidth destWidth userType dataType) = Signal dom (Axi4StreamM2S idWidth destWidth userType dataType)
  type Bwd (Axi4Stream dom idWidth destWidth userType dataType) = Signal dom Axi4StreamS2M

instance (dataType ~ Vec dataLen Axi4StreamByte) => Backpressure (Axi4Stream dom idWidth destWidth userType dataType) where
  boolsToBwd _ = C.fromList_lazy . fmap Axi4StreamS2M

instance (dataType ~ Vec dataLen Axi4StreamByte, C.KnownDomain dom, KnownNat idWidth, KnownNat destWidth) => Simulate (Axi4Stream dom idWidth destWidth userType dataType) where
  type SimulateFwdType (Axi4Stream dom idWidth destWidth userType dataType) = [Axi4StreamM2S idWidth destWidth userType dataType]
  type SimulateBwdType (Axi4Stream dom idWidth destWidth userType dataType) = [Axi4StreamS2M]
  type SimulateChannels (Axi4Stream dom idWidth destWidth userType dataType) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = C.sample_lazy
  sigToSimBwd _ = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) = DfLike.stall Proxy conf stallAck stalls

-- | Grab the data from a master-to-slave message, if there is any
streamM2SToMaybe :: Axi4StreamM2S idWidth destWidth userType dataType -> Maybe dataType
streamM2SToMaybe NoAxi4StreamM2S = Nothing
streamM2SToMaybe m2s = Just (_tdata m2s)

instance (dataType ~ Vec dataLen Axi4StreamByte, C.KnownDomain dom, KnownNat idWidth, KnownNat destWidth) => Drivable (Axi4Stream dom idWidth destWidth userType dataType) where
  type ExpectType (Axi4Stream dom idWidth destWidth userType dataType) = [dataType]

  -- | All the fields aside from @_tdata@ are left at zero/default values.
  toSimulateType Proxy = fmap (\dat -> (Axi4StreamM2S { _tdata = dat, _tlast = False, _tid = 0, _tdest = 0, _tuser = P.undefined }))
  fromSimulateType Proxy = Maybe.mapMaybe streamM2SToMaybe

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy

instance (dataType ~ Vec dataLen Axi4StreamByte, KnownNat idWidth, KnownNat destWidth) => DfLike dom (Axi4Stream dom idWidth destWidth userType) dataType where
  type Data (Axi4Stream dom idWidth destWidth userType) dataType = Axi4StreamM2S idWidth destWidth userType dataType
  type Payload dataType = dataType
  type Ack (Axi4Stream dom idWidth destWidth userType) dataType = Axi4StreamS2M

  getPayload = const $ streamM2SToMaybe

  -- | If we try to give data to a @NoAxi4StreamM2S@, all the fields aside from @_tdata@ are left at zero/default values.
  setPayload _ _ NoAxi4StreamM2S (Just b) = Axi4StreamM2S { _tdata = b, _tlast = False, _tid = 0, _tdest = 0, _tuser = P.undefined }
  setPayload _ _ m2s (Just b) = m2s { _tdata = b }
  setPayload _ _ _ Nothing = NoAxi4StreamM2S

  noData _ = NoAxi4StreamM2S

  boolToAck _ = Axi4StreamS2M
  ackToBool _ = tReady
