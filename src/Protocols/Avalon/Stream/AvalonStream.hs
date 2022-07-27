{-|
Types and instance declarations for the Avalon-stream protocol.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Hashable (Unsigned n)

module Protocols.Avalon.Stream.AvalonStream where

-- base
import           Control.DeepSeq (NFData)
import           Control.Monad (when)
import           Control.Monad.State (put, get, modify, gets)
import           Data.Hashable (Hashable)
import qualified Data.Maybe as Maybe
import           Data.Proxy
import qualified Prelude as P

-- clash-prelude
import           Clash.Prelude hiding (take, concat, length)
import qualified Clash.Prelude as C

-- me
import           Protocols.Internal
import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import           Protocols.Hedgehog.Internal


instance Hashable (C.Unsigned n)


-- | Configuration for AXI4 Stream protocol. Determines the width of some
-- fields in 'AvalonStreamM2S', and toggles some others. Also sets the ready
-- latency (see specs for more info on this).
data AvalonStreamConfig
  = AvalonStreamConfig
  { _channelWidth      :: Nat
  , _errorWidth        :: Nat
  , _keepStartOfPacket :: Bool
  , _keepEndOfPacket   :: Bool
  , _emptyWidth        :: Nat
  , _readyLatency      :: Nat
  }

-- | Grab '_channelWidth' at the type level.
type family ChannelWidth (conf :: AvalonStreamConfig) where
  ChannelWidth ('AvalonStreamConfig a _ _ _ _ _) = a

-- | Grab '_errorWidth' at the type level.
type family ErrorWidth (conf :: AvalonStreamConfig) where
  ErrorWidth ('AvalonStreamConfig _ a _ _ _ _) = a

-- | Grab '_keepStartOfPacket' at the type level.
type family KeepStartOfPacket (conf :: AvalonStreamConfig) where
  KeepStartOfPacket ('AvalonStreamConfig _ _ a _ _ _) = a

-- | Grab '_keepEndOfPacket' at the type level.
type family KeepEndOfPacket (conf :: AvalonStreamConfig) where
  KeepEndOfPacket ('AvalonStreamConfig _ _ _ a _ _) = a

-- | Grab '_emptyWidth' at the type level.
type family EmptyWidth (conf :: AvalonStreamConfig) where
  EmptyWidth ('AvalonStreamConfig _ _ _ _ a _) = a

-- | Grab '_readyLatency' at the type level.
type family ReadyLatency (conf :: AvalonStreamConfig) where
  ReadyLatency ('AvalonStreamConfig _ _ _ _ _ a) = a

-- | Shorthand for a "well-behaved" config, so that we don't need to write out
-- a bunch of type constraints later. Holds for every configuration; don't worry
-- about implementing this class.
type GoodAvalonStreamConfig conf =
  ( KnownNat      (ChannelWidth      conf)
  , KnownNat      (ErrorWidth        conf)
  , KeepTypeClass (KeepStartOfPacket conf)
  , KeepTypeClass (KeepEndOfPacket   conf)
  , KnownNat      (EmptyWidth        conf)
  , KnownNat      (ReadyLatency      conf)
  )


-- | Data sent from manager to subordinate.
-- The tvalid field is left out: messages with
-- @tvalid = False@ should be sent as a @NoAvalonStreamM2S@.
data AvalonStreamM2S (conf :: AvalonStreamConfig) (dataType :: Type)
  = NoAvalonStreamM2S
  | AvalonStreamM2S
    { _data          :: dataType
    , _channel       :: Unsigned (ChannelWidth conf)
    , _error         :: Unsigned (ErrorWidth conf)
    , _startofpacket :: KeepType (KeepStartOfPacket conf) Bool
    , _endofpacket   :: KeepType (KeepEndOfPacket conf) Bool
    , _empty         :: Unsigned (EmptyWidth conf)
    }
  deriving (Generic, Bundle)

deriving instance
  ( GoodAvalonStreamConfig conf
  , C.NFDataX dataType
  ) => C.NFDataX (AvalonStreamM2S conf dataType)

deriving instance
  ( GoodAvalonStreamConfig conf
  , NFData dataType
  ) => NFData (AvalonStreamM2S conf dataType)

deriving instance
  ( GoodAvalonStreamConfig conf
  , C.ShowX dataType
  ) => C.ShowX (AvalonStreamM2S conf dataType)

deriving instance
  ( GoodAvalonStreamConfig conf
  , Show dataType
  ) => Show (AvalonStreamM2S conf dataType)

deriving instance
  ( GoodAvalonStreamConfig conf
  , Eq dataType
  ) => Eq (AvalonStreamM2S conf dataType)

-- | "Unimportant" info sent from manager to subordinate. Includes everything in
-- 'AvalonStreamM2S' except for '_data'. Used in 'DfConv.DfConv' implementation.
data AvalonStreamExtraInfo (conf :: AvalonStreamConfig)
  = AvalonStreamExtraInfo
  { _echannel       :: Unsigned (ChannelWidth conf)
  , _eerror         :: Unsigned (ErrorWidth conf)
  , _estartofpacket :: KeepType (KeepStartOfPacket conf) Bool
  , _eendofpacket   :: KeepType (KeepEndOfPacket conf) Bool
  , _eempty         :: Unsigned (EmptyWidth conf)
  }
  deriving (Generic, Bundle)

deriving instance
  ( GoodAvalonStreamConfig conf
  ) => C.NFDataX (AvalonStreamExtraInfo conf)

deriving instance
  ( GoodAvalonStreamConfig conf
  ) => NFData (AvalonStreamExtraInfo conf)

deriving instance
  ( GoodAvalonStreamConfig conf
  ) => C.ShowX (AvalonStreamExtraInfo conf)

deriving instance
  ( GoodAvalonStreamConfig conf
  ) => Show (AvalonStreamExtraInfo conf)

deriving instance
  ( GoodAvalonStreamConfig conf
  ) => Eq (AvalonStreamExtraInfo conf)

deriving instance
  ( GoodAvalonStreamConfig conf
  ) => Hashable (AvalonStreamExtraInfo conf)

-- | Combine "important" data and "unimportant" extra info to make an
-- 'AvalonStreamM2S'.
avalonStreamDataToM2S ::
  Maybe (AvalonStreamExtraInfo conf, dataType) ->
  AvalonStreamM2S conf dataType
avalonStreamDataToM2S Nothing = NoAvalonStreamM2S
avalonStreamDataToM2S (Just (AvalonStreamExtraInfo{..}, _data))
  = AvalonStreamM2S
  { _data
  , _channel       = _echannel
  , _error         = _eerror
  , _startofpacket = _estartofpacket
  , _endofpacket   = _eendofpacket
  , _empty         = _eempty
  }

-- | Split an 'AvalonStreamM2S' into "important" data and "unimportant" extra
-- info.
avalonStreamM2SToData ::
  AvalonStreamM2S conf dataType ->
  Maybe (AvalonStreamExtraInfo conf, dataType)
avalonStreamM2SToData NoAvalonStreamM2S = Nothing
avalonStreamM2SToData AvalonStreamM2S{..}
  = Just (AvalonStreamExtraInfo
  { _echannel       = _channel
  , _eerror         = _error
  , _estartofpacket = _startofpacket
  , _eendofpacket   = _endofpacket
  , _eempty         = _empty
  }, _data)

-- | Data sent from subordinate to manager. A simple acknowledge message.
-- Manager can only send 'AvalonStreamM2S' when '_ready' was true
-- @readyLatency@ clock cycles ago.
newtype AvalonStreamS2M (readyLatency :: Nat) = AvalonStreamS2M { _ready :: Bool }
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show, Bundle)

-- | Type for Avalon Stream protocol.
data AvalonStream (dom :: Domain) (conf :: AvalonStreamConfig) (dataType :: Type)

instance Protocol (AvalonStream dom conf dataType) where
  type Fwd (AvalonStream dom conf dataType)
    = Signal dom (AvalonStreamM2S conf dataType)
  type Bwd (AvalonStream dom conf dataType)
    = Signal dom (AvalonStreamS2M (ReadyLatency conf))

instance (ReadyLatency conf ~ 0) =>
  Backpressure (AvalonStream dom conf dataType) where
  boolsToBwd _ = C.fromList_lazy . fmap AvalonStreamS2M

instance (GoodAvalonStreamConfig conf, NFDataX dataType) =>
  DfConv.DfConv    (AvalonStream dom conf dataType) where
  type Dom         (AvalonStream dom conf dataType) = dom
  type FwdPayload  (AvalonStream dom conf dataType)
    = (AvalonStreamExtraInfo conf, dataType)

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = C.repeat @((ReadyLatency conf)+1) False
    blankOtp = NoAvalonStreamM2S
    stateFn (AvalonStreamS2M thisAck) _ otpItem = do
      modify (thisAck +>>)
      ackQueue <- get
      pure
        ( avalonStreamDataToM2S
           (if (Maybe.isJust otpItem && C.last ackQueue) then otpItem else Nothing)
        , Nothing
        , C.last ackQueue )

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = Nothing
    blankOtp = AvalonStreamS2M { _ready = False }
    stateFn m2s ack _ = do
      noCurrentVal <- gets Maybe.isNothing
      let msgOtp = AvalonStreamS2M { _ready = noCurrentVal }
      when noCurrentVal $ put (avalonStreamM2SToData m2s)
      dfOtp <- get
      when (Maybe.isJust dfOtp && ack) $ put Nothing
      pure (msgOtp, dfOtp, False)

instance
  ( ReadyLatency conf ~ 0
  , GoodAvalonStreamConfig conf
  , NFDataX dataType
  , KnownDomain dom ) =>
  Simulate (AvalonStream dom conf dataType) where
  type SimulateFwdType (AvalonStream dom conf dataType)
    = [AvalonStreamM2S conf dataType]
  type SimulateBwdType (AvalonStream dom conf dataType) = [AvalonStreamS2M 0]
  type SimulateChannels (AvalonStream dom conf dataType) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ = sample_lazy
  sigToSimBwd _ = sample_lazy

  stallC conf (head -> (stallAck, stalls))
    = withClockResetEnable clockGen resetGen enableGen
    $ DfConv.stall Proxy Proxy conf stallAck stalls

instance
  ( ReadyLatency conf ~ 0
  , GoodAvalonStreamConfig conf
  , NFDataX dataType
  , KnownDomain dom ) =>
  Drivable (AvalonStream dom conf dataType) where
  type ExpectType (AvalonStream dom conf dataType)
    = [(AvalonStreamExtraInfo conf, dataType)]

  toSimulateType Proxy = P.map (avalonStreamDataToM2S . Just)
  fromSimulateType Proxy = Maybe.mapMaybe avalonStreamM2SToData

  driveC conf vals
    = withClockResetEnable clockGen resetGen enableGen
    $ DfConv.drive Proxy conf (avalonStreamM2SToData <$> vals)
  sampleC conf ckt
    = withClockResetEnable clockGen resetGen enableGen
    $ fmap avalonStreamDataToM2S
    $ DfConv.sample Proxy conf
    $ ckt

instance
  ( ReadyLatency conf ~ 0
  , GoodAvalonStreamConfig conf
  , NFDataX dataType
  , NFData dataType
  , ShowX dataType
  , Show dataType
  , Eq dataType
  , KnownDomain dom ) =>
  Test (AvalonStream dom conf dataType) where

  expectToLengths Proxy = pure . P.length
  expectN Proxy options nExpected sampled
    = expectN (Proxy @(Df.Df dom _)) options nExpected
    $ Df.maybeToData . avalonStreamM2SToData <$> sampled