{-|
Types and instance declarations for the Avalon-stream protocol.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Avalon.Stream.AvalonStream where

-- base
import           Control.DeepSeq (NFData)
import           Control.Monad (when)
import           Control.Monad.State (get, put, gets)
import           Prelude

import qualified Data.Maybe as Maybe
import           Data.Proxy

-- clash-prelude
import           Clash.Prelude hiding (take, concat, length)
import qualified Clash.Prelude as C

-- testing related
import           Hedgehog.Internal.Property (failWith)
import           Text.Show.Pretty (ppShow)

-- me
import           Protocols.Internal
import qualified Protocols.DfLike as DfLike
import           Protocols.Hedgehog.Internal


-- TODO comment
data AvalonStreamConfig
  = AvalonStreamConfig
  { _channelWidth      :: Nat
  , _errorWidth        :: Nat
  , _keepStartOfPacket :: Bool
  , _keepEndOfPacket   :: Bool
  , _emptyWidth        :: Nat
  , _readyLatency      :: Nat
  }

-- TODO comment
type family ChannelWidth (conf :: AvalonStreamConfig) where
  ChannelWidth ('AvalonStreamConfig a _ _ _ _ _) = a

type family ErrorWidth (conf :: AvalonStreamConfig) where
  ErrorWidth ('AvalonStreamConfig _ a _ _ _ _) = a

type family KeepStartOfPacket (conf :: AvalonStreamConfig) where
  KeepStartOfPacket ('AvalonStreamConfig _ _ a _ _ _) = a

type family KeepEndOfPacket (conf :: AvalonStreamConfig) where
  KeepEndOfPacket ('AvalonStreamConfig _ _ _ a _ _) = a

type family EmptyWidth (conf :: AvalonStreamConfig) where
  EmptyWidth ('AvalonStreamConfig _ _ _ _ a _) = a

type family ReadyLatency (conf :: AvalonStreamConfig) where
  ReadyLatency ('AvalonStreamConfig _ _ _ _ _ a) = a

class
  ( MaybeZeroNat  (ChannelWidth      conf)
  , MaybeZeroNat  (ErrorWidth        conf)
  , KeepTypeClass (KeepStartOfPacket conf)
  , KeepTypeClass (KeepEndOfPacket   conf)
  , MaybeZeroNat  (EmptyWidth        conf)
  , MaybeZeroNat  (ReadyLatency      conf)
  ) => GoodAvalonStreamConfig conf

instance
  ( MaybeZeroNat  (ChannelWidth      conf)
  , MaybeZeroNat  (ErrorWidth        conf)
  , KeepTypeClass (KeepStartOfPacket conf)
  , KeepTypeClass (KeepEndOfPacket   conf)
  , MaybeZeroNat  (EmptyWidth        conf)
  , MaybeZeroNat  (ReadyLatency      conf)
  ) => GoodAvalonStreamConfig conf


-- | Data sent from master to slave.
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

-- | TODO Data sent from master to slave.
-- The tvalid field is left out: messages with
-- @tvalid = False@ should be sent as a @NoAvalonStreamM2S@.
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

avalonStreamDataToM2S :: Maybe (AvalonStreamExtraInfo conf, dataType) -> AvalonStreamM2S conf dataType
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

avalonStreamM2SToData :: AvalonStreamM2S conf dataType -> Maybe (AvalonStreamExtraInfo conf, dataType)
avalonStreamM2SToData NoAvalonStreamM2S = Nothing
avalonStreamM2SToData AvalonStreamM2S{..}
  = Just (AvalonStreamExtraInfo
  { _echannel       = _channel
  , _eerror         = _error
  , _estartofpacket = _startofpacket
  , _eendofpacket   = _endofpacket
  , _eempty         = _empty
  }, _data)

-- | Data sent from slave to master.
-- A simple acknowledge message.
data AvalonStreamS2M (readyLatency :: Nat) = AvalonStreamS2M { _ready :: Bool }
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show, Bundle)

-- | Type for Avalon Stream protocol.
data AvalonStream (dom :: Domain) (conf :: AvalonStreamConfig) (dataType :: Type)

instance Protocol (AvalonStream dom conf dataType) where
  type Fwd (AvalonStream dom conf dataType) = Signal dom (AvalonStreamM2S conf dataType)
  type Bwd (AvalonStream dom conf dataType) = Signal dom (AvalonStreamS2M (ReadyLatency conf))

instance (ReadyLatency conf ~ 0) => Backpressure (AvalonStream dom conf dataType) where
  boolsToBwd _ = C.fromList_lazy . fmap AvalonStreamS2M

-- TODO keep ready on when not receiving data?

instance (ReadyLatency conf ~ 0, GoodAvalonStreamConfig conf, NFDataX dataType) =>
  DfLike.DfLike   (Reverse (AvalonStream dom conf dataType)) where
  type Dom        (Reverse (AvalonStream dom conf dataType)) = dom
  type BwdPayload (Reverse (AvalonStream dom conf dataType)) = (AvalonStreamExtraInfo conf, dataType)

  toDfCircuit _ = DfLike.toDfCircuitHelper s0 blankOtp stateFn where
    s0 = ()
    blankOtp = AvalonStreamS2M { _ready = False }
    stateFn m2s ack _ = pure (AvalonStreamS2M { _ready = ack }, avalonStreamM2SToData m2s, False)

instance (ReadyLatency conf ~ 0, GoodAvalonStreamConfig conf, NFDataX dataType) =>
  DfLike.ImplicitInfoClass    (Reverse (AvalonStream dom conf dataType)) where
  type ImplicitInfoBwdPayload (Reverse (AvalonStream dom conf dataType)) = dataType

  mapFwd _ () = ()
  mapBwd _ (_, dat) = dat

instance (GoodAvalonStreamConfig conf, NFDataX dataType) =>
  DfLike.DfLike    (AvalonStream dom conf dataType) where
  type Dom         (AvalonStream dom conf dataType) = dom
  type FwdPayload  (AvalonStream dom conf dataType) = (AvalonStreamExtraInfo conf, dataType)
  type DfLikeParam (AvalonStream dom conf dataType) = Unsigned (ChannelWidth conf)

  toDfCircuit (proxy, param) = DfLike.toDfCircuitHelper s0 blankOtp (stateFn param) where
    vec0 :: Proxy (AvalonStream dom conf dataType) -> Vec ((ReadyLatency conf)+1) Bool
    vec0 _ = C.repeat False
    s0 = (vec0 proxy, NoAvalonStreamM2S)
    blankOtp = NoAvalonStreamM2S
    stateFn _channel _thisAck _ otpItem = do
      let AvalonStreamS2M thisAck = _thisAck
      ackQueue' <- gets ((thisAck +>>) . fst)
      let ack = C.last ackQueue'
      sending <- gets snd
      put (ackQueue', sending)
      popped <- case (sending, otpItem) of
        (NoAvalonStreamM2S, Just oi) -> put (ackQueue', avalonStreamDataToM2S (Just oi)) >> pure True
        _ -> pure False
      toSend <- gets snd
      case toSend of -- ack might be undefined, so we shouldn't look at it unless we have to
        AvalonStreamM2S{} -> when ack $ put (ackQueue', NoAvalonStreamM2S)
        _ -> pure ()
      pure (toSend, Nothing, popped)

instance (GoodAvalonStreamConfig conf, NFDataX dataType) =>
  DfLike.ImplicitInfoClass    (AvalonStream dom conf dataType) where
  type ImplicitInfoFwdPayload (AvalonStream dom conf dataType) = dataType
  type ImplicitInfoFwdParam   (AvalonStream dom conf dataType) = AvalonStreamExtraInfo conf

  mapFwd (_, a) b = (a, b)
  mapBwd _ () = ()
