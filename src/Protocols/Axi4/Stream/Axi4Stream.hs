{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Protocols.Axi4.Stream.Axi4Stream where -- TODO bad module name

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

data Axi4StreamByte = DataByte (Unsigned 8) | PositionByte | NullByte  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

data Axi4StreamM2S idWidth destWidth userType dataType
  = NoAxi4StreamM2S
  | Axi4StreamM2S
  {
    tData :: dataType,
    tLast :: Bool,
    tId   :: Unsigned idWidth,
    tDest :: Unsigned destWidth,
    tUser :: userType
  }
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

data Axi4StreamS2M = Axi4StreamS2M { tReady :: Bool } deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

data Axi4Stream (dom :: Domain) (idWidth :: Nat) (destWidth :: Nat) (userType :: Type) (dataType :: Type)

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

streamM2SToMaybe :: Axi4StreamM2S idWidth destWidth userType dataType -> Maybe dataType
streamM2SToMaybe NoAxi4StreamM2S = Nothing
streamM2SToMaybe m2s = Just (tData m2s)

instance (dataType ~ Vec dataLen Axi4StreamByte, C.KnownDomain dom, KnownNat idWidth, KnownNat destWidth) => Drivable (Axi4Stream dom idWidth destWidth userType dataType) where
  type ExpectType (Axi4Stream dom idWidth destWidth userType dataType) = [dataType]

  toSimulateType Proxy = fmap (\dat -> (Axi4StreamM2S { tData = dat, tLast = False, tId = 0, tDest = 0, tUser = P.undefined }))
  fromSimulateType Proxy = Maybe.mapMaybe streamM2SToMaybe

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy

instance (dataType ~ Vec dataLen Axi4StreamByte, KnownNat idWidth, KnownNat destWidth) => DfLike dom (Axi4Stream dom idWidth destWidth userType) dataType where
  type Data (Axi4Stream dom idWidth destWidth userType) dataType = Axi4StreamM2S idWidth destWidth userType dataType
  type Payload dataType = dataType
  type Ack (Axi4Stream dom idWidth destWidth userType) dataType = Axi4StreamS2M

  getPayload = const $ streamM2SToMaybe

  setPayload _ _ NoAxi4StreamM2S (Just b) = Axi4StreamM2S { tData = b, tLast = False, tId = 0, tDest = 0, tUser = P.undefined }
  setPayload _ _ m2s (Just b) = m2s { tData = b }
  setPayload _ _ _ Nothing = NoAxi4StreamM2S

  noData _ = NoAxi4StreamM2S

  boolToAck _ = Axi4StreamS2M
  ackToBool _ = tReady
