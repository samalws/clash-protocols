{-|
Defines ReadData channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.ReadData
  ( M2S_ReadData(..)
  , S2M_ReadData(..)
  , Axi4ReadData
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Prelude hiding
  ((!!), map, zip, zipWith, filter, fst, snd, either, const, pure)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal

-- | AXI4 Read Data channel protocol
data Axi4ReadData
  (dom :: C.Domain)
  (kr :: KeepResponse)
  (iw :: IdWidth)
  (userType :: Type)
  (dataType :: Type)

instance Protocol (Axi4ReadData dom kr iw userType dataType) where
  type Fwd (Axi4ReadData dom kr iw userType dataType) =
    C.Signal dom (S2M_ReadData kr iw userType dataType)
  type Bwd (Axi4ReadData dom kr iw userType dataType) =
    C.Signal dom M2S_ReadData

instance Backpressure (Axi4ReadData dom kr iw userType dataType) where
  boolsToBwd _ = C.fromList_lazy . coerce

-- | See Table A2-6 "Read data channel signals"
data S2M_ReadData
  (kr :: KeepResponse)
  (iw :: IdWidth)
  (userType :: Type)
  (dataType :: Type)
  = S2M_NoReadData
  | S2M_ReadData
    { -- | Read address id*
      _rid :: !(C.BitVector (Width iw))

    , -- | Read data
      _rdata :: !dataType

      -- | Read response
    , _rresp :: !(ResponseType kr)

      -- | Read last
    , _rlast :: !Bool

      -- | User data
    , _ruser :: !userType
    }
  deriving (Generic)

-- | See Table A2-6 "Read data channel signals"
newtype M2S_ReadData = M2S_ReadData { _rready :: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX dataType
  , C.NFDataX (ResponseType kr)
  , C.KnownNat (Width iw)
  ) =>
  C.NFDataX (S2M_ReadData kr iw userType dataType)

deriving instance
  ( C.KnownNat (Width iw)
  , Show userType
  , Show dataType
  , Show (ResponseType kr) ) =>
  Show (S2M_ReadData kr iw userType dataType)
