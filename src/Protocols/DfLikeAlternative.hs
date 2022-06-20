{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Protocols.DfLikeAlternative where

import           Clash.Prelude
import           Control.Monad.State (State)
import           Data.Proxy (Proxy(..))


-- Describes how a protocol behaves at the data input end, using a state machine
-- Can take parameters and provide whatever state type you would like
class (NFDataX (DfLikeInpState fwd bwd dat), NFDataX dat) => DfLikeInput fwd bwd dat where
  -- | State carried between clock cycles
  type DfLikeInpState fwd bwd dat
  -- | User-provided parameters for the dfLike input
  type DfLikeInpParam fwd bwd dat
  -- | Initial state, given depth and user params
  dfLikeInpS0 :: Proxy (fwd,bwd,dat) -> DfLikeInpParam fwd bwd dat -> DfLikeInpState fwd bwd dat
  -- | Blank input, used when reset is on
  -- Doesn't look at current state, but can look at depth and user params
  -- Should not acknowledge any incoming data
  dfLikeInpBlank :: Proxy (fwd,bwd,dat) -> DfLikeInpParam fwd bwd dat -> bwd
  -- | State machine run every clock cycle at the dfLike input port
  -- Given user-provided params; data at the input port; and whether an input can be taken
  -- Can update state using State monad
  -- Returns data to output back to the port (usually an acknowledge signal), and Maybe an item that was consumed
  dfLikeInpFn :: Proxy (fwd,bwd,dat) -> DfLikeInpParam fwd bwd dat -> fwd -> Bool -> State (DfLikeInpState fwd bwd dat) (bwd, Maybe dat)

-- Describes how a protocol behaves at the data output end, using a state machine
-- Can take parameters and provide whatever state type you would like
class (NFDataX (DfLikeOtpState fwd bwd dat), NFDataX dat) => DfLikeOutput fwd bwd dat where
  -- | State carried between clock cycles
  type DfLikeOtpState fwd bwd dat
  -- | User-provided parameters for the dfLike output
  type DfLikeOtpParam fwd bwd dat
  -- | Initial state, given depth and user params
  dfLikeOtpS0 :: Proxy (fwd,bwd,dat) -> DfLikeOtpParam fwd bwd dat -> DfLikeOtpState fwd bwd dat
  -- | Blank input, used when reset is on
  -- Doesn't look at current state, but can look at depth and user params
  -- Should not acknowledge any incoming data
  dfLikeOtpBlank :: Proxy (fwd,bwd,dat) -> DfLikeOtpParam fwd bwd dat -> fwd
  -- | State machine run every clock cycle at the dfLike output port
  -- Given user-provided params; data at the output port (usually an acknowledge signal); and Maybe the next data item to output
  -- Can update state using State monad
  -- Returns data to output to the port, and whether a data item was taken
  -- The 'Maybe dat' input is allowed to change arbitrarily between clock cycles
  dfLikeOtpFn :: Proxy (fwd,bwd,dat) -> DfLikeOtpParam fwd bwd dat -> bwd -> Maybe dat -> State (DfLikeOtpState fwd bwd dat) (fwd, Bool)
