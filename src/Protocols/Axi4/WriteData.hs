{-|
Defines WriteData channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.WriteData
  ( M2S_WriteData(..)
  , S2M_WriteData(..)
  , Axi4WriteData
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.TypeNats (Nat)
import Prelude hiding
  ((!!), map, zip, zipWith, filter, fst, snd, either, const, pure)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal

-- | AXI4 Write Data channel protocol
data Axi4WriteData
  (dom :: C.Domain)
  (ks :: KeepStrobe)
  (nBytes :: Nat)
  (userType :: Type)

instance Protocol (Axi4WriteData dom ks nBytes userType) where
  type Fwd (Axi4WriteData dom ks nBytes userType) =
    C.Signal dom (M2S_WriteData ks nBytes userType)
  type Bwd (Axi4WriteData dom ks nBytes userType) =
    C.Signal dom S2M_WriteData

instance Backpressure (Axi4WriteData dom ks dataType userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

-- | See Table A2-3 "Write data channel signals". If strobing is kept, the data
-- will be a vector of 'Maybe' bytes. If strobing is not kept, data will be a
-- 'C.BitVector'.
data M2S_WriteData
  (ks :: KeepStrobe)
  (nBytes :: Nat)
  (userType :: Type)
  = M2S_NoWriteData
  | M2S_WriteData
    { -- | Write data
      _wdata :: !(StrictStrobeType nBytes ks)

      -- | Write last
    , _wlast :: !Bool

      -- | User data
    , _wuser :: !userType
    }
  deriving (Generic)

-- | See Table A2-3 "Write data channel signals"
newtype S2M_WriteData = S2M_WriteData { _wready :: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX (StrictStrobeType nBytes ks) ) =>
  C.NFDataX (M2S_WriteData ks nBytes userType)

deriving instance
  ( Show userType
  , Show (StrictStrobeType nBytes ks)
  , C.KnownNat nBytes ) =>
  Show (M2S_WriteData ks nBytes userType)
