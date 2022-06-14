{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Protocols.Axi4.Stream.Axi4Stream where -- TODO bad module name

-- base
import           GHC.Stack (HasCallStack)
import           Control.DeepSeq (NFData)
import           Prelude hiding ()

import qualified Data.Maybe as Maybe
import           Data.Proxy
import qualified Prelude as P

-- clash-prelude
import           Clash.Prelude
import           Clash.Signal.Internal (Signal)
import qualified Clash.Prelude as C

-- me
import           Protocols.Internal
import           Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

data Axi4StreamByte = DataByte (Unsigned 8) | PositionByte {- TODO no data? -} | NullByte  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

-- TODO dataType should *only* ever be Vec n Axi4StreamByte; how do I enforce this?
data Axi4StreamM2S idType destType userType dataType
  = Axi4StreamM2S
  {
    streamBytes :: dataType, -- TODO what to name it?
    tValid      :: Bool,
    tLast       :: Bool,
    tId         :: idType,
    tDest       :: destType,
    tUser       :: userType
  }
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

data Axi4StreamS2M = Axi4StreamS2M { tReady :: Bool } deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

data Axi4Stream (dom :: Domain) (idType :: Type) (destType :: Type) (userType :: Type) (dataType :: Type)

instance Protocol (Axi4Stream dom idType destType userType dataType) where
  type Fwd (Axi4Stream dom idType destType userType dataType) = Signal dom (Axi4StreamM2S idType destType userType dataType)
  type Bwd (Axi4Stream dom idType destType userType dataType) = Signal dom Axi4StreamS2M

streamM2SToMaybe :: Axi4StreamM2S idType destType userType dataType -> Maybe dataType
streamM2SToMaybe m2s = if tValid m2s then Just (streamBytes m2s) else Nothing

instance Backpressure (Axi4Stream dom idType destType userType dataType) where
  boolsToBwd _ = C.fromList_lazy . fmap Axi4StreamS2M

instance (C.KnownDomain dom{-, C.NFDataX a, C.ShowX a, Show a-}) => Simulate (Axi4Stream dom idType destType userType dataType) where
  type SimulateFwdType (Axi4Stream dom idType destType userType dataType) = [Axi4StreamM2S idType destType userType dataType]
  type SimulateBwdType (Axi4Stream dom idType destType userType dataType) = [Axi4StreamS2M]
  type SimulateChannels (Axi4Stream dom idType destType userType dataType) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = C.sample_lazy
  sigToSimBwd _ = C.sample_lazy

  -- TODO???
  -- stallC conf (C.head -> (stallAck, stalls)) = stall conf stallAck stalls

instance (C.KnownDomain dom) => Drivable (Axi4Stream dom idType destType userType dataType) where
  type ExpectType (Axi4Stream dom idType destType userType dataType) = [dataType]

  -- TODO
  -- toSimulateType Proxy = P.map Data
  fromSimulateType Proxy = Maybe.mapMaybe streamM2SToMaybe

  -- driveC = drive
  -- sampleC = sample


instance DfLike dom (Axi4Stream dom idType destType userType) dataType where
  type Data (Axi4Stream dom idType destType userType) dataType = Axi4StreamM2S idType destType userType dataType
  type Payload dataType = dataType
  type Ack (Axi4Stream dom idType destType userType) dataType = Axi4StreamS2M

  getPayload = const $ streamM2SToMaybe

  setPayload _ _ m2s (Just b) = m2s { tValid = True, streamBytes = b }
  setPayload _ _ m2s Nothing = m2s { tValid = False, streamBytes = Prelude.undefined }

  noData _ = Axi4StreamM2S { tValid = False }

  boolToAck _ = Axi4StreamS2M
  ackToBool _ = tReady
