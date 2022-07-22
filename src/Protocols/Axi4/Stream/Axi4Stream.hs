{-|
Types and instance declarations for the AXI4-stream protocol.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Hashable (Unsigned n)

module Protocols.Axi4.Stream.Axi4Stream where

-- base
import           Control.DeepSeq (NFData)
import           Prelude

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

data Axi4StreamConfig
  = Axi4StreamConfig
  { _dataWidth :: Nat
  , _idWidth :: Nat
  , _destWidth :: Nat
  }

class
  ( KnownNat (DataWidth conf)
  , KnownNat (IdWidth conf)
  , KnownNat (DestWidth conf)
  ) => GoodAxi4StreamConfig conf

instance
  ( KnownNat (DataWidth conf)
  , KnownNat (IdWidth conf)
  , KnownNat (DestWidth conf)
  ) => GoodAxi4StreamConfig conf

type family DataWidth (conf :: Axi4StreamConfig) where
  DataWidth ('Axi4StreamConfig a _ _) = a

type family IdWidth (conf :: Axi4StreamConfig) where
  IdWidth ('Axi4StreamConfig _ a _) = a

type family DestWidth (conf :: Axi4StreamConfig) where
  DestWidth ('Axi4StreamConfig _ _ a) = a

-- | A byte sent along an AXI4 Stream. Each byte can either be a data byte, a
-- position byte, or a null byte. The value of position and null bytes should
-- be ignored. Additionally, null bytes can be added or dropped.
data Axi4StreamByte
  = DataByte (Unsigned 8)
  | PositionByte
  | NullByte
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show, Hashable)

-- | Data sent from master to slave. @dataType@ should only ever be
-- @Vec n Axi4StreamByte@ for some n, but is left open-ended in order to be able
-- to implement DfLike. The tvalid field is left out: messages with @tvalid = False@
-- should be sent as a @NoAxi4StreamM2S@. The tdata, @tstrb@, and @tkeep@ fields are
-- all grouped in the @_tdata@ field in this datatype (see @Axi4StreamByte@).
data Axi4StreamM2S (conf :: Axi4StreamConfig) (userType :: Type)
  = NoAxi4StreamM2S
  | Axi4StreamM2S
  { _tdata :: Vec (DataWidth conf) Axi4StreamByte
  , _tlast :: Bool
  , _tid   :: Unsigned (IdWidth conf)
  , _tdest :: Unsigned (DestWidth conf)
  , _tuser :: userType
  }
  deriving (Generic, C.ShowX, Show, NFData)

deriving instance
  ( GoodAxi4StreamConfig conf
  , C.NFDataX userType
  ) => C.NFDataX (Axi4StreamM2S conf userType)

deriving instance
  ( GoodAxi4StreamConfig conf
  , Eq userType
  ) => Eq (Axi4StreamM2S conf userType)

-- | Data sent from slave to master. A simple acknowledge message.
data Axi4StreamS2M = Axi4StreamS2M { _tready :: Bool }
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

-- | Type for AXI4 Stream protocol.
data Axi4Stream (dom :: Domain) (conf :: Axi4StreamConfig) (userType :: Type)

instance Protocol (Axi4Stream dom conf userType) where
  type Fwd (Axi4Stream dom conf userType) = Signal dom (Axi4StreamM2S conf userType)
  type Bwd (Axi4Stream dom conf userType) = Signal dom Axi4StreamS2M

instance Backpressure (Axi4Stream dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . fmap Axi4StreamS2M
