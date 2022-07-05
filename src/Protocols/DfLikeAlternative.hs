{-|
Defines a mix-and-match interface for creating fifo buffers.
Buffers can be made from one protocol to another,
and are parametrized on the amount of items in the buffer.
Blockram is used to store fifo buffer items.
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, UndecidableInstances #-}

module Protocols.DfLikeAlternative where

import           Prelude hiding (replicate, last, repeat)
import           Control.Monad (when)
import           Control.Monad.State (StateT(..), State, runState, get, put, gets, modify)
import           Clash.Prelude hiding ((&&), (||), not)
import           Data.Either (fromRight)
import           Data.Functor.Identity (Identity(..))
import           Data.Maybe (isJust, fromJust, isNothing, fromMaybe)
import           Data.Proxy (Proxy(..))

-- me
import           Protocols.Avalon.Stream.AvalonStream
import           Protocols.Avalon.MemMap.AvalonMemMap
import           Protocols.Axi4.Stream.Axi4Stream
import           Protocols.Df (Data(..))
import           Protocols.Internal


-- Describes how a protocol behaves at the data input end, using a state machine
-- Can take parameters and provide whatever state type you would like
class (NFDataX (DfLikeInpState fwd bwd dat), NFDataX dat) => DfLikeInput fwd bwd dat where
  -- | State carried between clock cycles
  type DfLikeInpState fwd bwd dat
  -- | User-provided parameters for the dfLike input
  type DfLikeInpParam fwd bwd dat
  -- | Initial state, given user params
  dfLikeInpS0 :: Proxy (fwd,bwd,dat) -> DfLikeInpParam fwd bwd dat -> DfLikeInpState fwd bwd dat
  -- | Blank input, used when reset is on
  -- Doesn't look at current state, but can look at user params
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
  -- | Initial state, given user params
  dfLikeOtpS0 :: Proxy (fwd,bwd,dat) -> DfLikeOtpParam fwd bwd dat -> DfLikeOtpState fwd bwd dat
  -- | Blank input, used when reset is on
  -- Doesn't look at current state, but can look at user params
  -- Should not acknowledge any incoming data
  dfLikeOtpBlank :: Proxy (fwd,bwd,dat) -> DfLikeOtpParam fwd bwd dat -> fwd
  -- | State machine run every clock cycle at the dfLike output port
  -- Given user-provided params; data at the output port (usually an acknowledge signal); and Maybe the next data item to output
  -- Can update state using State monad
  -- Returns data to output to the port, and whether a data item was taken
  -- The 'Maybe dat' input is allowed to change arbitrarily between clock cycles
  dfLikeOtpFn :: Proxy (fwd,bwd,dat) -> DfLikeOtpParam fwd bwd dat -> bwd -> Bool -> dat {- TODO Maybe dat -} -> State (DfLikeOtpState fwd bwd dat) (fwd, Bool)


-- DfLike classes for Df

instance (NFDataX dat) => DfLikeInput (Data dat) Ack dat where
  type DfLikeInpState (Data dat) Ack dat = ()
  type DfLikeInpParam (Data dat) Ack dat = ()
  dfLikeInpS0 _ _ = ()
  dfLikeInpBlank _ _ = Ack False
  dfLikeInpFn _ _ (Data inp) True = pure (Ack True, Just inp)
  dfLikeInpFn _ _ _ _ = pure (Ack False, Nothing)

instance (NFDataX dat) => DfLikeOutput (Data dat) Ack dat where
  type DfLikeOtpState (Data dat) Ack dat = Maybe dat
  type DfLikeOtpParam (Data dat) Ack dat = ()
  dfLikeOtpS0 _ _ = Nothing
  dfLikeOtpBlank _ _ = NoData
  dfLikeOtpFn _ _ (Ack ack) ready queueItem = do
    sending <- get
    retVal <- case (sending, ready) of
      (Just toSend, _) -> pure (Data toSend, False)
      (Nothing, True) -> put (Just queueItem) >> pure (Data queueItem, True)
      (Nothing, False) -> pure (NoData, False)
    shouldReadAck <- gets isJust -- ack might be undefined, so we shouldn't look at it unless we have to
    when (shouldReadAck && ack) $ put Nothing
    pure retVal


-- DfLike classes for Axi4Stream

instance (KnownNat idWidth, KnownNat destWidth, NFDataX dataType) =>
    DfLikeInput (Axi4StreamM2S idWidth destWidth () dataType) Axi4StreamS2M dataType where
  type DfLikeInpState (Axi4StreamM2S idWidth destWidth () dataType) Axi4StreamS2M dataType
    = ()
  type DfLikeInpParam (Axi4StreamM2S idWidth destWidth () dataType) Axi4StreamS2M dataType
    = ()

  dfLikeInpS0 _ _ = ()
  dfLikeInpBlank _ _ = Axi4StreamS2M { _tready = False }
  dfLikeInpFn _ _ (Axi4StreamM2S { _tdata }) True = pure (Axi4StreamS2M { _tready = True }, Just _tdata)
  dfLikeInpFn _ _ _ _ = pure (Axi4StreamS2M { _tready = False }, Nothing)

instance (KnownNat idWidth, KnownNat destWidth, NFDataX dataType) =>
    DfLikeOutput (Axi4StreamM2S idWidth destWidth () dataType) Axi4StreamS2M dataType where
  type DfLikeOtpState (Axi4StreamM2S idWidth destWidth () dataType) Axi4StreamS2M dataType
    = Axi4StreamM2S idWidth destWidth () dataType
  type DfLikeOtpParam (Axi4StreamM2S idWidth destWidth () dataType) Axi4StreamS2M dataType
    = Unsigned destWidth

  dfLikeOtpS0 _ _ = NoAxi4StreamM2S
  dfLikeOtpBlank _ _ = NoAxi4StreamM2S
  dfLikeOtpFn _ _tdest _ack ready queueItem = do
    let (Axi4StreamS2M ack) = _ack
    sending <- get
    popped <- case (sending, ready) of
      (NoAxi4StreamM2S, True) -> put (Axi4StreamM2S { _tdata = queueItem, _tlast = False, _tid = 0, _tdest, _tuser = () }) >> pure True
      _ -> pure False
    toSend <- get
    case toSend of -- ack might be undefined, so we shouldn't look at it unless we have to
      Axi4StreamM2S{} -> when ack $ put NoAxi4StreamM2S
      _ -> pure ()
    pure (toSend, popped)


-- DfLike classes for AvalonStream
-- TODO keep ready on when not receiving data?

instance (NFDataX dataType) =>
    DfLikeInput (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) (AvalonStreamS2M 0) dataType where
  type DfLikeInpState (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) (AvalonStreamS2M 0) dataType
    = ()
  type DfLikeInpParam (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) (AvalonStreamS2M 0) dataType
    = ()

  dfLikeInpS0 _ _ = ()
  dfLikeInpBlank _ _ = AvalonStreamS2M { _ready = False }
  dfLikeInpFn _ _ (AvalonStreamM2S { _data }) True = pure (AvalonStreamS2M { _ready = True }, Just _data)
  dfLikeInpFn _ _ _ _ = pure (AvalonStreamS2M { _ready = False }, Nothing)

instance (KnownNat errorWidth, KnownNat emptyWidth, KnownNat readyLatency, NFDataX dataType) =>
    DfLikeOutput (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) (AvalonStreamS2M readyLatency) dataType where
  type DfLikeOtpState (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) (AvalonStreamS2M readyLatency) dataType
    = (Vec (readyLatency+1) Bool, AvalonStreamM2S channelWidth errorWidth emptyWidth dataType)
  type DfLikeOtpParam (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) (AvalonStreamS2M readyLatency) dataType
    = (Unsigned channelWidth)

  dfLikeOtpS0 _ _ = (repeat False, NoAvalonStreamM2S)
  dfLikeOtpBlank _ _ = NoAvalonStreamM2S
  dfLikeOtpFn _ _channel _thisAck ready queueItem = do
    let AvalonStreamS2M thisAck = _thisAck
    ackQueue' <- gets ((thisAck +>>) . fst)
    let ack = last ackQueue'
    sending <- gets snd
    put (ackQueue', sending)
    popped <- case (sending, ready) of
      (NoAvalonStreamM2S, True) -> put (ackQueue', AvalonStreamM2S { _data = queueItem, _channel, _error = 0, _startofpacket = True, _endofpacket = True, _empty = 0 }) >> pure True
      _ -> pure False
    toSend <- gets snd
    case toSend of -- ack might be undefined, so we shouldn't look at it unless we have to
      AvalonStreamM2S{} -> when ack $ put (ackQueue', NoAvalonStreamM2S)
      _ -> pure ()
    pure (toSend, popped)


-- DfLike classes for AvalonMemMap
-- TODO keep waitrequest on when not receiving data?

instance (GoodMMSlaveConfig config, NFDataX readDataType, NFDataX writeDataType) =>
    DfLikeInput (AvalonSlaveIn config writeDataType) (AvalonSlaveOut config readDataType) writeDataType where
  type DfLikeInpState (AvalonSlaveIn config writeDataType) (AvalonSlaveOut config readDataType) writeDataType
    = ()
  type DfLikeInpParam (AvalonSlaveIn config writeDataType) (AvalonSlaveOut config readDataType) writeDataType
    = Unsigned (AddrWidth (SShared config))

  dfLikeInpS0 _ _ = ()
  dfLikeInpBlank _ _ = boolToMMSlaveAck False
  dfLikeInpFn _ addr si True | isJust (mmSlaveInToMaybe si) && si_addr si == addr = pure (boolToMMSlaveAck True, mmSlaveInToMaybe si)
  dfLikeInpFn _ _ _ _ = pure (boolToMMSlaveAck False, Nothing)

instance (GoodMMMasterConfig config, NFDataX readDataType, NFDataX writeDataType) =>
    DfLikeInput (AvalonMasterIn config readDataType) (AvalonMasterOut config writeDataType) readDataType where
  type DfLikeInpState (AvalonMasterIn config readDataType) (AvalonMasterOut config writeDataType) readDataType
    = Bool -- ready value last frame
  type DfLikeInpParam (AvalonMasterIn config readDataType) (AvalonMasterOut config writeDataType) readDataType
    = Unsigned (AddrWidth (MShared config))

  dfLikeInpS0 _ _ = True
  dfLikeInpBlank _ _ = mmMasterOutNoData

  dfLikeInpFn _ addr mi ready = do
    lastReady <- get
    put ready
    pure (
      AvalonMasterOut
      { mo_addr        = addr
      , mo_read        = toKeepBool ready
      , mo_write       = toKeepBool False
      , mo_byteEnable  = if ready then bitCoerce (repeat True) else 0
      , mo_burstCount  = if ready then 1 else 0
      , mo_flush       = toKeepBool False
      , mo_writeData   = errorX "No writeData for Avalon MM dfLikeInpFn"
      }
      , if mi_waitRequest mi && lastReady then Nothing else Just (mi_readData mi))

instance (GoodMMSlaveConfig config, NFDataX readDataType, NFDataX writeDataType) =>
    DfLikeOutput (AvalonSlaveOut config readDataType) (AvalonSlaveIn config writeDataType) readDataType where
  type DfLikeOtpState (AvalonSlaveOut config readDataType) (AvalonSlaveIn config writeDataType) readDataType
    = ()
  type DfLikeOtpParam (AvalonSlaveOut config readDataType) (AvalonSlaveIn config writeDataType) readDataType
    = Unsigned (AddrWidth (SShared config))

  dfLikeOtpS0 _ _ = ()
  dfLikeOtpBlank _ _ = boolToMMSlaveAck False
  dfLikeOtpFn _ addr si ready queueItem
    = pure (
    AvalonSlaveOut
    { so_waitRequest   = toKeepBool (not ready)
    , so_readDataValid = toKeepBool ready
    , so_readyForData  = toKeepBool False
    , so_dataAvailable = toKeepBool ready
    , so_endOfPacket   = toKeepBool True
    , so_irq           = toKeepBool True
    , so_readData      = queueItem
    }
    , ready
    && fromKeepBool True (si_chipSelect si)
    && fromKeepBool True (si_read si)
    && 0 /= fromMaybeEmptyNum 1 (si_byteEnable si)
    && si_addr si == addr)

instance (GoodMMMasterConfig config, NFDataX readDataType, NFDataX writeDataType) =>
    DfLikeOutput (AvalonMasterOut config writeDataType) (AvalonMasterIn config readDataType) writeDataType where
  type DfLikeOtpState (AvalonMasterOut config writeDataType) (AvalonMasterIn config readDataType) writeDataType
    = Maybe writeDataType
  type DfLikeOtpParam (AvalonMasterOut config writeDataType) (AvalonMasterIn config readDataType) writeDataType
    = Unsigned (AddrWidth (MShared config))

  dfLikeOtpS0 _ _ = Nothing
  dfLikeOtpBlank _ _ = mmMasterOutNoData
  dfLikeOtpFn _ addr mi ready queueItem = do
    sending <- get
    retVal <- case (sending, ready) of
      (Just toSend, _) -> pure (mmMasterOutSendingData { mo_writeData = toSend, mo_addr = addr }, False)
      (Nothing, True) -> put (Just queueItem) >> pure (mmMasterOutSendingData { mo_writeData = queueItem, mo_addr = addr }, True)
      (Nothing, False) -> pure (mmMasterOutNoData, False)
    shouldReadAck <- gets isJust -- ack might be undefined, so we shouldn't look at it unless we have to
    when (shouldReadAck && mmMasterInToBool mi) $ put Nothing
    pure retVal


-- | Generalized fifo for DfLike
-- Uses blockram to store data
fifo ::
  HiddenClockResetEnable dom =>
  KnownNat depth =>
  NFDataX dat =>
  DfLikeInput fwdA bwdA dat =>
  DfLikeOutput fwdB bwdB dat =>
  DfLikeInpState fwdA bwdA dat ~ sA =>
  DfLikeOtpState fwdB bwdB dat ~ sB =>
  Proxy (fwdA,bwdA,dat) ->
  Proxy (fwdB,bwdB,dat) ->
  SNat depth ->
  DfLikeInpParam fwdA bwdA dat ->
  DfLikeOtpParam fwdB bwdB dat ->
  (Signal dom fwdA, Signal dom bwdB) ->
  (Signal dom bwdA, Signal dom fwdB)
fifo pxyA pxyB fifoDepth paramA paramB = hideReset circuitFunction where

  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset (inpA, inpB) = (otpA, otpB) where
    -- initialize bram
    brRead = readNew (blockRam (replicate fifoDepth $ errorX "fifo: undefined initial fifo buffer value")) brReadAddr brWrite
    -- run the state machine (a mealy machine)
    (brReadAddr, brWrite, otpA, otpB) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB)

  -- when reset is on, set state to initial state and output blank outputs
  machineAsFunction _ (_, True, _, _) = (s0, (0, Nothing, dfLikeInpBlank pxyA paramA, dfLikeOtpBlank pxyB paramB))
  machineAsFunction (sA,sB,rAddr,wAddr,amtLeft) (brRead, False, iA, iB) =
    let -- run the input port state machine
        ((oA, maybePush), sA') = runState (dfLikeInpFn pxyA paramA iA (amtLeft > 0)) sA
        -- potentially push an item onto blockram
        brWrite = (wAddr,) <$> maybePush
        -- adjust write address and amount left (output state machine doesn't see amountLeft')
        (wAddr', amtLeft') = if (isJust maybePush) then (incIdxLooping wAddr, amtLeft-1) else (wAddr, amtLeft)
        -- if we're about to push onto an empty queue, we can pop immediately instead
        (brRead_, amtLeft_) = if (amtLeft == maxBound && isJust maybePush) then (fromJust maybePush, amtLeft') else (brRead, amtLeft)
        -- run the output port state machine
        ((oB, popped), sB') = runState (dfLikeOtpFn pxyB paramB iB (amtLeft_ < maxBound) brRead_) sB
        -- adjust blockram read address and amount left
        (rAddr', amtLeft'') = if popped then (incIdxLooping rAddr, amtLeft'+1) else (rAddr, amtLeft')
        brReadAddr = rAddr'
        -- return our new state and outputs
    in  ((sA', sB', rAddr', wAddr', amtLeft''), (brReadAddr, brWrite, oA, oB))

  -- initial state
  -- (s0 for input port (taken from class), s0 for output port (taken from class), next read address, next write address, space left in bram)
  s0 = (dfLikeInpS0 pxyA paramA, dfLikeOtpS0 pxyB paramB, _0 fifoDepth, _0 fifoDepth, _maxBound fifoDepth)

  -- type level hack
  -- make sure we have the right Index number
  _0 :: (KnownNat n) => SNat n -> Index n
  _0 = const 0

  -- type level hack
  -- make sure we have the right Index number
  _maxBound :: (KnownNat n) => SNat n -> Index (n+1)
  _maxBound = const maxBound

  -- loop around to 0 if we're about to overflow, otherwise increment
  incIdxLooping idx = if idx == maxBound then 0 else idx+1
