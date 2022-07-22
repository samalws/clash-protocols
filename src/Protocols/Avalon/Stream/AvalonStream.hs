{-|
Types and instance declarations for the Avalon-stream protocol.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Avalon.Stream.AvalonStream where

-- base
import           Control.DeepSeq (NFData)
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
import           Protocols.DfLike (DfLike)
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
  deriving (Generic)

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

-- | Data sent from slave to master.
-- A simple acknowledge message.
data AvalonStreamS2M (readyLatency :: Nat) = AvalonStreamS2M { _ready :: Bool } deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

-- | Type for Avalon Stream protocol.
data AvalonStream (dom :: Domain) (conf :: AvalonStreamConfig) (dataType :: Type)

instance Protocol (AvalonStream dom conf dataType) where
  type Fwd (AvalonStream dom conf dataType) = Signal dom (AvalonStreamM2S conf dataType)
  type Bwd (AvalonStream dom conf dataType) = Signal dom (AvalonStreamS2M (ReadyLatency conf))

instance (ReadyLatency conf ~ 0) => Backpressure (AvalonStream dom conf dataType) where
  boolsToBwd _ = C.fromList_lazy . fmap AvalonStreamS2M
