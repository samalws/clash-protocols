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
import Data.Proxy
import GHC.Generics (Generic)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

data Axi4WriteAddressConfig = Axi4WriteAddressConfig
  { _waKeepBurst       :: Bool
  , _waKeepSize        :: Bool
  , _waLengthWidth     :: C.Nat -- TODO this isn't even used??
  , _waIdWidth         :: C.Nat
  , _waAddrWidth       :: C.Nat
  , _waKeepRegion      :: Bool
  , _waKeepBurstLength :: Bool
  , _waKeepLock        :: Bool
  , _waKeepCache       :: Bool
  , _waKeepPermissions :: Bool
  , _waKeepQos         :: Bool
  }

type family WAKeepBurst (c :: Axi4WriteAddressConfig) where
  WAKeepBurst ('Axi4WriteAddressConfig a _ _ _ _ _ _ _ _ _ _) = a

type family WAKeepSize (c :: Axi4WriteAddressConfig) where
  WAKeepSize ('Axi4WriteAddressConfig _ a _ _ _ _ _ _ _ _ _) = a

type family WALengthWidth (c :: Axi4WriteAddressConfig) where
  WALengthWidth ('Axi4WriteAddressConfig _ _ a _ _ _ _ _ _ _ _) = a

type family WAIdWidth (c :: Axi4WriteAddressConfig) where
  WAIdWidth ('Axi4WriteAddressConfig _ _ _ a _ _ _ _ _ _ _) = a

type family WAAddrWidth (c :: Axi4WriteAddressConfig) where
  WAAddrWidth ('Axi4WriteAddressConfig _ _ _ _ a _ _ _ _ _ _) = a

type family WAKeepRegion (c :: Axi4WriteAddressConfig) where
  WAKeepRegion ('Axi4WriteAddressConfig _ _ _ _ _ a _ _ _ _ _) = a

type family WAKeepBurstLength (c :: Axi4WriteAddressConfig) where
  WAKeepBurstLength ('Axi4WriteAddressConfig _ _ _ _ _ _ a _ _ _ _) = a

type family WAKeepLock (c :: Axi4WriteAddressConfig) where
  WAKeepLock ('Axi4WriteAddressConfig _ _ _ _ _ _ _ a _ _ _) = a

type family WAKeepCache (c :: Axi4WriteAddressConfig) where
  WAKeepCache ('Axi4WriteAddressConfig _ _ _ _ _ _ _ _ a _ _) = a

type family WAKeepPermissions (c :: Axi4WriteAddressConfig) where
  WAKeepPermissions ('Axi4WriteAddressConfig _ _ _ _ _ _ _ _ _ a _) = a

type family WAKeepQos (c :: Axi4WriteAddressConfig) where
  WAKeepQos ('Axi4WriteAddressConfig _ _ _ _ _ _ _ _ _ _ a) = a

-- | AXI4 Write Address channel protocol
data Axi4WriteAddress
  (dom :: C.Domain)
  (conf :: Axi4WriteAddressConfig)
  (userType :: Type)

instance Protocol (Axi4WriteAddress dom conf userType) where
  type Fwd (Axi4WriteAddress dom conf userType) =
    C.Signal dom (M2S_WriteAddress conf userType)
  type Bwd (Axi4WriteAddress dom conf userType) =
    C.Signal dom S2M_WriteAddress

instance Backpressure (Axi4WriteAddress dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4WriteAddress dom conf) userType where
  type Data (Axi4WriteAddress dom conf) userType =
    M2S_WriteAddress conf userType

  type Payload userType = userType

  type Ack (Axi4WriteAddress dom conf) userType =
    S2M_WriteAddress

  getPayload _ (M2S_WriteAddress{_awuser}) = Just _awuser
  getPayload _ M2S_NoWriteAddress = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_awuser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = M2S_NoWriteAddress
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4WriteAddress dom conf userType) where

  type SimulateFwdType (Axi4WriteAddress dom conf userType) =
    [M2S_WriteAddress conf userType]

  type SimulateBwdType (Axi4WriteAddress dom conf userType) =
    [S2M_WriteAddress]

  type SimulateChannels (Axi4WriteAddress dom conf userType) = 1

  simToSigFwd Proxy = C.fromList_lazy
  simToSigBwd Proxy = C.fromList_lazy
  sigToSimFwd Proxy = C.sample_lazy
  sigToSimBwd Proxy = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-2 "Write address channel signals"
data M2S_WriteAddress
  (conf :: Axi4WriteAddressConfig)
  (userType :: Type)
  = M2S_NoWriteAddress
  | M2S_WriteAddress
    { -- | Write address id*
      _awid :: !(C.BitVector (WAIdWidth conf))

      -- | Write address
    , _awaddr :: !(C.BitVector (WAAddrWidth conf))

      -- | Write region*
    , _awregion:: !(RegionType (WAKeepRegion conf))

      -- | Burst length*
    , _awlen :: !(BurstLengthType (WAKeepBurstLength conf))

      -- | Burst size*
    , _awsize :: !(SizeType (WAKeepSize conf))

      -- | Burst type*
    , _awburst :: !(BurstType (WAKeepBurst conf))

      -- | Lock type*
    , _awlock :: !(LockType (WAKeepLock conf))

      -- | Cache type*
    , _awcache :: !(CacheType (WAKeepCache conf))

      -- | Protection type
    , _awprot :: !(PermissionsType (WAKeepPermissions conf))

      -- | QoS value
    , _awqos :: !(QosType (WAKeepQos conf))

      -- | User data
    , _awuser :: !userType
    }
  deriving (Generic)

-- | See Table A2-2 "Write address channel signals"
newtype S2M_WriteAddress = S2M_WriteAddress { _awready :: Bool }
  deriving (Show, Generic, C.NFDataX)

{-
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
-}
