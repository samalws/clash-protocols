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

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal

-- | AXI4 Read Data channel protocol
data Axi4WriteResponse
  (dom :: C.Domain)
  (kr :: KeepResponse)
  (iw :: IdWidth)
  (userType :: Type)

instance Protocol (Axi4WriteResponse dom kr iw userType) where
  type Fwd (Axi4WriteResponse dom kr iw userType) =
    C.Signal dom (S2M_WriteResponse kr iw userType)
  type Bwd (Axi4WriteResponse dom kr iw userType) =
    C.Signal dom M2S_WriteResponse

instance Backpressure (Axi4WriteResponse dom kr iw userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

-- | See Table A2-4 "Write response channel signals"
data S2M_WriteResponse
  (kr :: KeepResponse)
  (iw :: IdWidth)
  (userType :: Type)
  = S2M_NoWriteResponse
  | S2M_WriteResponse
    { -- | Response ID
      _bid :: !(C.BitVector (Width iw))

      -- | Write response
    , _bresp :: !(ResponseType kr)

      -- | User data
    , _buser :: !userType
    }
  deriving (Generic)

-- | See Table A2-4 "Write response channel signals"
newtype M2S_WriteResponse = M2S_WriteResponse { _bready :: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX (ResponseType kr)
  , C.KnownNat (Width iw) ) =>
  C.NFDataX (S2M_WriteResponse kr iw userType)

deriving instance
  ( Show userType
  , Show (ResponseType kr)
  , C.KnownNat (Width iw) ) =>
  Show (S2M_WriteResponse kr iw userType)
