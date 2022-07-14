{-|
Defines WriteAddress channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.WriteAddress
  ( M2S_WriteAddress(..)
  , S2M_WriteAddress(..)
  , Axi4WriteAddress
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal

-- | AXI4 Write Address channel protocol
data Axi4WriteAddress
  (dom :: C.Domain)
  (kb :: KeepBurst)
  (ksz :: KeepSize)
  (lw :: LengthWidth)
  (iw :: IdWidth)
  (aw :: AddrWidth)
  (kr :: KeepRegion)
  (kbl :: KeepBurstLength)
  (kl :: KeepLock)
  (kc :: KeepCache)
  (kp :: KeepPermissions)
  (kq :: KeepQos)
  (userType :: Type)

instance Protocol (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) where
  type Fwd (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    C.Signal dom (M2S_WriteAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)
  type Bwd (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    C.Signal dom S2M_WriteAddress

instance Backpressure (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) where
  boolsToBwd _ = C.fromList_lazy . coerce


-- | See Table A2-2 "Write address channel signals"
data M2S_WriteAddress
  (kb :: KeepBurst)
  (ksz :: KeepSize)
  (lw :: LengthWidth)
  (iw :: IdWidth)
  (aw :: AddrWidth)
  (kr :: KeepRegion)
  (kbl :: KeepBurstLength)
  (kl :: KeepLock)
  (kc :: KeepCache)
  (kp :: KeepPermissions)
  (kq :: KeepQos)
  (userType :: Type)
  = M2S_NoWriteAddress
  | M2S_WriteAddress
    { -- | Write address id*
      _awid :: !(C.BitVector (Width iw))

      -- | Write address
    , _awaddr :: !(C.BitVector (Width aw))

      -- | Write region*
    , _awregion:: !(RegionType kr)

      -- | Burst length*
    , _awlen :: !(BurstLengthType kbl)

      -- | Burst size*
    , _awsize :: !(SizeType ksz)

      -- | Burst type*
    , _awburst :: !(BurstType kb)

      -- | Lock type*
    , _awlock :: !(LockType kl)

      -- | Cache type*
    , _awcache :: !(CacheType kc)

      -- | Protection type
    , _awprot :: !(PermissionsType kp)

      -- | QoS value
    , _awqos :: !(QosType kq)

      -- | User data
    , _awuser :: !userType
    }
  deriving (Generic)

-- | See Table A2-2 "Write address channel signals"
newtype S2M_WriteAddress = S2M_WriteAddress { _awready :: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.KnownNat (Width iw)
  , C.KnownNat (Width aw)
  , Show (SizeType ksz)
  , Show (BurstType kb)
  , Show userType
  , Show (RegionType kr)
  , Show (BurstLengthType kbl)
  , Show (LockType kl)
  , Show (CacheType kc)
  , Show (PermissionsType kp)
  , Show (QosType kq)
  ) =>
  Show (M2S_WriteAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX (BurstType kb)
  , C.NFDataX (SizeType ksz)
  , C.NFDataX (BurstType kb)
  , C.NFDataX userType
  , C.NFDataX (RegionType kr)
  , C.NFDataX (BurstLengthType kbl)
  , C.NFDataX (LockType kl)
  , C.NFDataX (CacheType kc)
  , C.NFDataX (PermissionsType kp)
  , C.NFDataX (QosType kq)
  , C.KnownNat (Width iw)
  , C.KnownNat (Width aw)
  ) =>
  C.NFDataX (M2S_WriteAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)
