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
import Data.Proxy
import GHC.Generics (Generic)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

data Axi4ReadAddressConfig = Axi4ReadAddressConfig
  { _raKeepBurst       :: Bool
  , _raKeepSize        :: Bool
  , _raLengthWidth     :: C.Nat
  , _raIdWidth         :: C.Nat
  , _raAddrWidth       :: C.Nat
  , _raKeepRegion      :: Bool
  , _raKeepBurstLength :: Bool
  , _raKeepLock        :: Bool
  , _raKeepCache       :: Bool
  , _raKeepPermissions :: Bool
  , _raKeepQos         :: Bool
  }

type family RAKeepBurst (conf :: Axi4ReadAddressConfig) where
  RAKeepBurst ('Axi4ReadAddressConfig a _ _ _ _ _ _ _ _ _ _) = a

type family RAKeepSize (conf :: Axi4ReadAddressConfig) where
  RAKeepSize ('Axi4ReadAddressConfig _ a _ _ _ _ _ _ _ _ _) = a

type family RALengthWidth (conf :: Axi4ReadAddressConfig) where
  RALengthWidth ('Axi4ReadAddressConfig _ _ a _ _ _ _ _ _ _ _) = a

type family RAIdWidth (conf :: Axi4ReadAddressConfig) where
  RAIdWidth ('Axi4ReadAddressConfig _ _ _ a _ _ _ _ _ _ _) = a

type family RAAddrWidth (conf :: Axi4ReadAddressConfig) where
  RAAddrWidth ('Axi4ReadAddressConfig _ _ _ _ a _ _ _ _ _ _) = a

type family RAKeepRegion (conf :: Axi4ReadAddressConfig) where
  RAKeepRegion ('Axi4ReadAddressConfig _ _ _ _ _ a _ _ _ _ _) = a

type family RAKeepBurstLength (conf :: Axi4ReadAddressConfig) where
  RAKeepBurstLength ('Axi4ReadAddressConfig _ _ _ _ _ _ a _ _ _ _) = a

type family RAKeepLock (conf :: Axi4ReadAddressConfig) where
  RAKeepLock ('Axi4ReadAddressConfig _ _ _ _ _ _ _ a _ _ _) = a

type family RAKeepCache (conf :: Axi4ReadAddressConfig) where
  RAKeepCache ('Axi4ReadAddressConfig _ _ _ _ _ _ _ _ a _ _) = a

type family RAKeepPermissions (conf :: Axi4ReadAddressConfig) where
  RAKeepPermissions ('Axi4ReadAddressConfig _ _ _ _ _ _ _ _ _ a _) = a

type family RAKeepQos (conf :: Axi4ReadAddressConfig) where
  RAKeepQos ('Axi4ReadAddressConfig _ _ _ _ _ _ _ _ _ _ a) = a

-- | AXI4 Read Address channel protocol
data Axi4ReadAddress
  (dom :: C.Domain)
  (conf :: Axi4ReadAddressConfig)
  (userType :: Type)

instance Protocol (Axi4ReadAddress dom conf userType) where
  type Fwd (Axi4ReadAddress dom conf userType) =
    C.Signal dom (M2S_ReadAddress conf userType)
  type Bwd (Axi4ReadAddress dom conf userType) =
    C.Signal dom S2M_ReadAddress

instance Backpressure (Axi4ReadAddress dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4ReadAddress dom conf) userType where
  type Data (Axi4ReadAddress dom conf) userType =
    M2S_ReadAddress conf userType

  type Payload userType = userType

  type Ack (Axi4ReadAddress dom conf) userType =
    S2M_ReadAddress

  getPayload _ (M2S_ReadAddress{_aruser}) = Just _aruser
  getPayload _ M2S_NoReadAddress = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_aruser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = M2S_NoReadAddress
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4ReadAddress dom conf userType) where

  type SimulateFwdType (Axi4ReadAddress dom conf userType) =
    [M2S_ReadAddress conf userType]

  type SimulateBwdType (Axi4ReadAddress dom conf userType) =
    [S2M_ReadAddress]

  type SimulateChannels (Axi4ReadAddress dom conf userType) = 1

  simToSigFwd Proxy = C.fromList_lazy
  simToSigBwd Proxy = C.fromList_lazy
  sigToSimFwd Proxy = C.sample_lazy
  sigToSimBwd Proxy = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-5 "Read address channel signals"

data M2S_ReadAddress
  (conf :: Axi4ReadAddressConfig)
  (userType :: Type)
  = M2S_NoReadAddress
  | M2S_ReadAddress
    { -- | Read address id*
      _arid :: !(C.BitVector (RAIdWidth conf))

      -- | Read address
    , _araddr :: !(C.BitVector (RAAddrWidth conf))

      -- | Read region*
    , _arregion :: !(RegionType (RAKeepRegion conf))

      -- | Burst length*
    , _arlen :: !(BurstLengthType (RAKeepBurstLength conf))

      -- | Burst size*
    , _arsize :: !(SizeType (RAKeepSize conf))

      -- | Burst type*
    , _arburst :: !(BurstType (RAKeepBurst conf))

      -- | Lock type*
    , _arlock :: !(LockType (RAKeepLock conf))

      -- | Cache type* (has been renamed to modifiable in AXI spec)
    , _arcache :: !(CacheType (RAKeepCache conf))

      -- | Protection type
    , _arprot :: !(PermissionsType (RAKeepPermissions conf))

      -- | QoS value
    , _arqos :: !(QosType (RAKeepQos conf))

      -- | User data
    , _aruser :: !userType
    }
  deriving (Generic)

-- | See Table A2-5 "Read address channel signals"
newtype S2M_ReadAddress = S2M_ReadAddress
  { _arready :: Bool }
  deriving (Show, Generic, C.NFDataX)
