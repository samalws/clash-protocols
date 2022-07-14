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
import Data.Proxy
import           Prelude hiding
  ((!!), map, zip, zipWith, filter, fst, snd, either, const, pure)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

data Axi4ReadDataConfig = Axi4ReadDataConfig
  { _rdKeepResponse :: Bool
  , _rdIdWidth      :: C.Nat
  }

type family RDKeepResponse (conf :: Axi4ReadDataConfig) where
  RDKeepResponse ('Axi4ReadDataConfig a _) = a

type family RDIdWidth (conf :: Axi4ReadDataConfig) where
  RDIdWidth ('Axi4ReadDataConfig _ a) = a

-- | AXI4 Read Data channel protocol
data Axi4ReadData
  (dom :: C.Domain)
  (conf :: Axi4ReadDataConfig)
  (userType :: Type)
  (dataType :: Type)

instance Protocol (Axi4ReadData dom conf userType dataType) where
  type Fwd (Axi4ReadData dom conf userType dataType) =
    C.Signal dom (S2M_ReadData conf userType dataType)
  type Bwd (Axi4ReadData dom conf userType dataType) =
    C.Signal dom M2S_ReadData

instance Backpressure (Axi4ReadData dom conf userType dataType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4ReadData dom conf userType) dataType where
  type Data (Axi4ReadData dom conf userType) dataType =
    S2M_ReadData conf userType dataType

  type Payload dataType = dataType

  type Ack (Axi4ReadData dom conf userType) dataType  =
    M2S_ReadData

  getPayload _ (S2M_ReadData{_rdata}) = Just _rdata
  getPayload _ S2M_NoReadData = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_rdata=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = S2M_NoReadData
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX dataType, C.ShowX dataType, Show dataType) =>
  Simulate (Axi4ReadData dom conf userType dataType) where

  type SimulateFwdType (Axi4ReadData dom conf userType dataType) =
    [S2M_ReadData conf userType dataType]

  type SimulateBwdType (Axi4ReadData dom conf userType dataType) =
    [M2S_ReadData]

  type SimulateChannels (Axi4ReadData dom conf userType dataType) = 1

  simToSigFwd Proxy = C.fromList_lazy
  simToSigBwd Proxy = C.fromList_lazy
  sigToSimFwd Proxy = C.sample_lazy
  sigToSimBwd Proxy = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-6 "Read data channel signals"
data S2M_ReadData
  (conf :: Axi4ReadDataConfig)
  (userType :: Type)
  (dataType :: Type)
  = S2M_NoReadData
  | S2M_ReadData
    { -- | Read address id*
      _rid :: !(C.BitVector (RDIdWidth conf))

    , -- | Read data
      _rdata :: !dataType

      -- | Read response
    , _rresp :: !(ResponseType (RDKeepResponse conf))

      -- | Read last
    , _rlast :: !Bool

      -- | User data
    , _ruser :: !userType
    }
  deriving (Generic)

-- | See Table A2-6 "Read data channel signals"
newtype M2S_ReadData = M2S_ReadData { _rready :: Bool }
  deriving (Show, Generic, C.NFDataX)
