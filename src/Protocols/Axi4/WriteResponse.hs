{-|
Defines WriteResponse channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.WriteResponse
  ( M2S_WriteResponse(..)
  , S2M_WriteResponse(..)
  , Axi4WriteResponse
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Proxy

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

data Axi4WriteResponseConfig = Axi4WriteResponseConfig
  { _wrKeepResponse :: Bool
  , _wrIdWidth      :: C.Nat
  }

type family WRKeepResponse (conf :: Axi4WriteResponseConfig) where
  WRKeepResponse ('Axi4WriteResponseConfig a _) = a

type family WRIdWidth (conf :: Axi4WriteResponseConfig) where
  WRIdWidth ('Axi4WriteResponseConfig _ a) = a

-- | AXI4 Read Data channel protocol
data Axi4WriteResponse
  (dom :: C.Domain)
  (conf :: Axi4WriteResponseConfig)
  (userType :: Type)

instance Protocol (Axi4WriteResponse dom conf userType) where
  type Fwd (Axi4WriteResponse dom conf userType) =
    C.Signal dom (S2M_WriteResponse conf userType)
  type Bwd (Axi4WriteResponse dom conf userType) =
    C.Signal dom M2S_WriteResponse

instance Backpressure (Axi4WriteResponse dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4WriteResponse dom conf) userType where
  type Data (Axi4WriteResponse dom conf) userType =
    S2M_WriteResponse conf userType

  type Payload userType = userType

  type Ack (Axi4WriteResponse dom conf) userType =
    M2S_WriteResponse

  getPayload _ (S2M_WriteResponse{_buser}) = Just _buser
  getPayload _ _ = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_buser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = S2M_NoWriteResponse
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4WriteResponse dom conf userType) where

  type SimulateFwdType (Axi4WriteResponse dom conf userType) =
    [S2M_WriteResponse conf userType]

  type SimulateBwdType (Axi4WriteResponse dom conf userType) =
    [M2S_WriteResponse]

  type SimulateChannels (Axi4WriteResponse dom conf userType) = 1

  simToSigFwd Proxy = C.fromList_lazy
  simToSigBwd Proxy = C.fromList_lazy
  sigToSimFwd Proxy = C.sample_lazy
  sigToSimBwd Proxy = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-4 "Write response channel signals"
data S2M_WriteResponse
  (conf :: Axi4WriteResponseConfig)
  (userType :: Type)
  = S2M_NoWriteResponse
  | S2M_WriteResponse
    { -- | Response ID
      _bid :: !(C.BitVector (WRIdWidth conf))

      -- | Write response
    , _bresp :: !(ResponseType (WRKeepResponse conf))

      -- | User data
    , _buser :: !userType
    }
  deriving (Generic)

-- | See Table A2-4 "Write response channel signals"
newtype M2S_WriteResponse = M2S_WriteResponse { _bready :: Bool }
  deriving (Show, Generic, C.NFDataX)
