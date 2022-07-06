{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Hashable (Unsigned n)

module Protocols.Axi4.Stream.Axi4Stream where

-- base
import           Control.DeepSeq (NFData)
import           Prelude hiding ()

import           Data.Hashable (Hashable)
import qualified Data.Maybe as Maybe
import           Data.Proxy
import qualified Prelude as P

-- clash-prelude
import           Clash.Prelude hiding (take, concat, length)
import qualified Clash.Prelude as C

-- testing related
import           Hedgehog.Internal.Property (failWith)
import           Text.Show.Pretty (ppShow)

-- me
import           Protocols.Internal
import           Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike
import           Protocols.Hedgehog.Internal


instance (KnownNat n) => Hashable (Unsigned n)

-- | Each byte sent along an AXI4 Stream can either be
-- a data byte, a position byte, or a null byte.
-- The value of position and null bytes should be ignored.
-- Additionally, null bytes can be added or dropped.
data Axi4StreamByte = DataByte (Unsigned 8)
                    | PositionByte
                    | NullByte
                    deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show, Hashable)

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
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show, C.Bundle)

-- | Data sent from slave to master.
-- A simple acknowledge message.
data Axi4StreamS2M = Axi4StreamS2M { _tready :: Bool } deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show, C.Bundle)

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
  ackToBool _ = _tready

instance (dataType ~ Vec dataLen Axi4StreamByte, C.KnownDomain dom, KnownNat idWidth, KnownNat destWidth, KnownNat dataLen, NFData userType, NFDataX userType, Show userType, ShowX userType, Eq userType) => Test (Axi4Stream dom idWidth destWidth userType dataType) where
  expectToLengths Proxy = pure . length

  -- directly copied from Df instance, with minor changes made
  expectN Proxy (ExpectOptions{eoEmptyTail, eoTimeout}) (C.head -> nExpected) sampled = do
    go (Maybe.fromMaybe maxBound eoTimeout) nExpected sampled
   where
    catDatas [] = []
    catDatas (NoAxi4StreamM2S:xs) = catDatas xs
    catDatas (Axi4StreamM2S{_tdata=x}:xs) = x:catDatas xs

    go _timeout _n [] =
      -- This really should not happen, protocols should produce data indefinitely
      error "unexpected end of signal"
    go _timeout 0 rest = do
      -- Check for superfluous output from protocol
      case catDatas (take eoEmptyTail rest) of
        [] -> pure (take nExpected (catDatas sampled))
        superfluous ->
          let err = "Circuit produced more output than expected:" in
          failWith Nothing (err <> "\n\n" <> ppShow superfluous)
    go timeout n _ | timeout <= 0 =
      failWith Nothing $ concat
        [ "Circuit did not produce enough output. Expected "
        , show n, " more values. Sampled only " <> show (nExpected - n) <> ":\n\n"
        , ppShow (take (nExpected - n) (catDatas sampled)) ]

    go timeout n (NoAxi4StreamM2S:as) = do
      -- Circuit did not output valid cycle, just continue
      go (pred timeout) n as
    go _ n (Axi4StreamM2S{}:as) =
      -- Circuit produced a valid cycle, reset timeout
      go (Maybe.fromMaybe maxBound eoTimeout) (pred n) as
