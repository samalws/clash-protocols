{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Protocols.Axi4.Stream.Axi4Stream where -- TODO bad module name

-- base
import           Control.DeepSeq (NFData)
import           Prelude hiding ()

import qualified Data.Maybe as Maybe
import           Data.Proxy
import qualified Prelude as P

-- clash-prelude
import           Clash.Prelude
import qualified Clash.Prelude as C

-- me
import           Protocols.Internal
import           Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

data Axi4StreamByte = DataByte (Unsigned 8) | PositionByte {- TODO no data? -} | NullByte  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

data Axi4StreamM2S idWidth destWidth userType dataWidth
  = NoAxi4StreamM2S
  | Axi4StreamM2S
  {
    streamBytes :: Vec dataWidth Axi4StreamByte, -- TODO what to name it?
    tLast       :: Bool,
    tId         :: BitVector idWidth,
    tDest       :: BitVector destWidth,
    tUser       :: userType
  }
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

data Axi4StreamS2M = Axi4StreamS2M { tReady :: Bool } deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)
