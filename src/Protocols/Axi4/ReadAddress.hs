{-|
Defines ReadAddress channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.ReadAddress
  ( M2S_ReadAddress(..)
  , S2M_ReadAddress(..)
  , Axi4ReadAddress
  ) where

-- base
import Data.Coerce
import Data.Kind (Type)
import GHC.Generics (Generic)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal

-- | AXI4 Read Address channel protocol
data Axi4ReadAddress
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

instance Protocol (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) where
  type Fwd (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    C.Signal dom (M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)
  type Bwd (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    C.Signal dom S2M_ReadAddress

instance Backpressure (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

-- | See Table A2-5 "Read address channel signals"

data M2S_ReadAddress
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
  = M2S_NoReadAddress
  | M2S_ReadAddress
    { -- | Read address id*
      _arid :: !(C.BitVector (Width iw))

      -- | Read address
    , _araddr :: !(C.BitVector (Width aw))

      -- | Read region*
    , _arregion :: !(RegionType kr)

      -- | Burst length*
    , _arlen :: !(BurstLengthType kbl)

      -- | Burst size*
    , _arsize :: !(SizeType ksz)

      -- | Burst type*
    , _arburst :: !(BurstType kb)

      -- | Lock type*
    , _arlock :: !(LockType kl)

      -- | Cache type* (has been renamed to modifiable in AXI spec)
    , _arcache :: !(CacheType kc)

      -- | Protection type
    , _arprot :: !(PermissionsType kp)

      -- | QoS value
    , _arqos :: !(QosType kq)

      -- | User data
    , _aruser :: !userType
    }
  deriving (Generic)

-- | See Table A2-5 "Read address channel signals"
newtype S2M_ReadAddress = S2M_ReadAddress
  { _arready :: Bool }
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
  , Show (QosType kq) ) =>
  Show (M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)

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
  C.NFDataX (M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)
