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

data Axi4StreamM2S idType destType userType (busWidth :: Nat)
  = Axi4StreamM2S
  {
    streamBytes :: Vec busWidth Axi4StreamByte, -- TODO what to name it?
    tValid      :: Bool,
    tLast       :: Bool,
    tId         :: idType,
    tDest       :: destType,
    tUser       :: userType
  }
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

data Axi4StreamS2M = Axi4StreamS2M { tReady :: Bool } deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

data Axi4Stream (dom :: Domain) (idType :: Type) (destType :: Type) (userType :: Type) (busWidth :: Nat)

instance Protocol (Axi4Stream dom idType destType userType busWidth) where
  type Fwd (Axi4Stream dom idType destType userType busWidth) = Signal dom (Axi4StreamM2S idType destType userType busWidth)
  type Bwd (Axi4Stream dom idType destType userType busWidth) = Signal dom Axi4StreamS2M

streamM2SToMaybe :: Axi4StreamM2S idType destType userType busWidth -> Maybe (Vec busWidth Axi4StreamByte)
streamM2SToMaybe m2s = if tValid m2s then Just (streamBytes m2s) else Nothing

-- TODO
instance Backpressure (Axi4Stream dom idType destType userType busWidth) where
  boolsToBwd _ = Prelude.undefined -- C.fromList_lazy . coerce

instance (C.KnownDomain dom{-, C.NFDataX a, C.ShowX a, Show a-}) => Simulate (Axi4Stream dom idType destType userType busWidth) where
  type SimulateFwdType (Axi4Stream dom idType destType userType busWidth) = [Axi4StreamM2S idType destType userType busWidth]
  type SimulateBwdType (Axi4Stream dom idType destType userType busWidth) = [Axi4StreamS2M]
  type SimulateChannels (Axi4Stream dom idType destType userType busWidth) = 1

  -- TODO??
  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = C.sample_lazy
  sigToSimBwd _ = C.sample_lazy

  -- TODO???
  -- stallC conf (C.head -> (stallAck, stalls)) = stall conf stallAck stalls

instance (C.KnownDomain dom) => Drivable (Axi4Stream dom idType destType userType busWidth) where
  type ExpectType (Axi4Stream dom idType destType userType busWidth) = [Vec busWidth Axi4StreamByte]

  -- TODO
  -- toSimulateType Proxy = P.map Data
  fromSimulateType Proxy = Maybe.mapMaybe streamM2SToMaybe

  -- driveC = drive
  -- sampleC = sample


-- TODO busWidth is a Nat but should be a type
{-
instance DfLike dom (Axi4Stream dom idType destType userType) (Vec busWidth Axi4StreamByte) where
  type Data (Axi4Stream dom idType destType userType) (Vec busWidth Axi4StreamByte) = Axi4StreamM2S idType destType userType busWidth
  type Payload (Vec busWidth Axi4StreamByte) = (Vec busWidth Axi4StreamByte)
  type Ack (Axi4Stream dom idType destType userType) (Vec busWidth Axi4StreamByte) = Axi4StreamS2M

  getPayload = const $ streamM2SToMaybe

  setPayload _ _ m2s (Just b) = m2s { tValid = True, streamBytes = b }
  setPayload _ _ m2s Nothing = m2s { tValid = False }

  noData _ = Axi4StreamM2S { tValid = False }

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}
-}
