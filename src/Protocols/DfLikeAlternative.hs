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


-- | Describes how a protocol behaves at one side of a circuit, using a state machine
-- Can take parameters and provide whatever state type you would like
-- Can be used for either the left or right side of a circuit
-- Supports both input data and output data (TODO is it really DfLike then if there's data going both ways?)
class (NFDataX (DfLikeState inp otp datInp datOtp)) => DfLikeAlternative inp otp datInp datOtp where
  -- | State carried between clock cycles
  type DfLikeState inp otp datInp datOtp
  -- | User-provided parameters (e.g. address to respond to)
  type DfLikeParam inp otp datInp datOtp
  -- | Initial state, given user params
  dfLikeS0 :: Proxy (inp,otp,datInp,datOtp) -> DfLikeParam inp otp datInp datOtp -> DfLikeState inp otp datInp datOtp
  -- | Blank input, used when reset is on
  -- Doesn't look at current state, but can look at user params
  -- Should not acknowledge any incoming data; doing so will result in data loss
  dfLikeBlank :: Proxy (inp,otp,datInp,datOtp) -> DfLikeParam inp otp datInp datOtp -> otp
  -- | State machine run every clock cycle at this port.
  -- Given user-provided params; input data at the port; acknowledge for inputted data (can be either True or False if there is no inputted data); and Maybe the next data item to output.
  -- Can update state using State monad.
  -- Returns data to output to the port; Maybe data inputted from the port; and whether an output data item was taken.
  -- The 'Maybe datOtp' argument is allowed to change arbitrarily between clock cycles, as is the 'Maybe datInp' return value.
  -- If the 'Maybe datInp' return value is 'Nothing', the 'Bool' argument can be either 'True' or 'False';
  --   the same goes for the 'Maybe datOtp' argument and the 'Bool' return value.
  dfLikeFn :: Proxy (inp,otp,datInp,datOtp) -> DfLikeParam inp otp datInp datOtp -> inp -> Bool -> Maybe datOtp -> State (DfLikeState inp otp datInp datOtp) (otp, Maybe datInp, Bool)


-- DfLike classes for Df

instance (NFDataX dat) => DfLikeAlternative (Data dat) Ack dat () where
  type DfLikeState (Data dat) Ack dat () = ()
  type DfLikeParam (Data dat) Ack dat () = ()
  dfLikeS0 _ _ = ()
  dfLikeBlank _ _ = Ack False
  dfLikeFn _ _ (Data inp) True _ = pure (Ack True, Just inp, False)
  dfLikeFn _ _ _ _ _ = pure (Ack False, Nothing, False)

instance (NFDataX dat) => DfLikeAlternative Ack (Data dat) () dat where
  type DfLikeState Ack (Data dat) () dat = Maybe dat
  type DfLikeParam Ack (Data dat) () dat = ()
  dfLikeS0 _ _ = Nothing
  dfLikeBlank _ _ = NoData
  dfLikeFn _ _ (Ack ack) _ otpItem = do
    sending <- get
    retVal <- case (sending, otpItem) of
      (Just toSend, _) -> pure (Data toSend, Nothing, False)
      (Nothing, Just oi) -> put (Just oi) >> pure (Data oi, Nothing, True)
      (Nothing, Nothing) -> pure (NoData, Nothing, False)
    shouldReadAck <- gets isJust -- ack might be undefined, so we shouldn't look at it unless we have to
    when (shouldReadAck && ack) $ put Nothing
    pure retVal


-- DfLike classes for Axi4Stream

instance (KnownNat idWidth, KnownNat destWidth, NFDataX dataType) =>
    DfLikeAlternative (Axi4StreamM2S idWidth destWidth () dataType) Axi4StreamS2M dataType () where
  type DfLikeState (Axi4StreamM2S idWidth destWidth () dataType) Axi4StreamS2M dataType ()
    = ()
  type DfLikeParam (Axi4StreamM2S idWidth destWidth () dataType) Axi4StreamS2M dataType ()
    = ()

  dfLikeS0 _ _ = ()
  dfLikeBlank _ _ = Axi4StreamS2M { _tready = False }
  dfLikeFn _ _ (Axi4StreamM2S { _tdata }) True _ = pure (Axi4StreamS2M { _tready = True }, Just _tdata, False)
  dfLikeFn _ _ _ _ _ = pure (Axi4StreamS2M { _tready = False }, Nothing, False)

instance (KnownNat idWidth, KnownNat destWidth, NFDataX dataType) =>
    DfLikeAlternative Axi4StreamS2M (Axi4StreamM2S idWidth destWidth () dataType) () dataType where
  type DfLikeState Axi4StreamS2M (Axi4StreamM2S idWidth destWidth () dataType) () dataType
    = Axi4StreamM2S idWidth destWidth () dataType
  type DfLikeParam Axi4StreamS2M (Axi4StreamM2S idWidth destWidth () dataType) () dataType
    = Unsigned destWidth

  dfLikeS0 _ _ = NoAxi4StreamM2S
  dfLikeBlank _ _ = NoAxi4StreamM2S
  dfLikeFn _ _tdest _ack _ otpItem = do
    let (Axi4StreamS2M ack) = _ack
    sending <- get
    popped <- case (sending, otpItem) of
      (NoAxi4StreamM2S, Just oi) -> put (Axi4StreamM2S { _tdata = oi, _tlast = False, _tid = 0, _tdest, _tuser = () }) >> pure True
      _ -> pure False
    toSend <- get
    case toSend of -- ack might be undefined, so we shouldn't look at it unless we have to
      Axi4StreamM2S{} -> when ack $ put NoAxi4StreamM2S
      _ -> pure ()
    pure (toSend, Nothing, popped)


-- DfLike classes for AvalonStream
-- TODO keep ready on when not receiving data?

instance (NFDataX dataType) =>
    DfLikeAlternative (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) (AvalonStreamS2M 0) dataType () where
  type DfLikeState (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) (AvalonStreamS2M 0) dataType ()
    = ()
  type DfLikeParam (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) (AvalonStreamS2M 0) dataType ()
    = ()

  dfLikeS0 _ _ = ()
  dfLikeBlank _ _ = AvalonStreamS2M { _ready = False }
  dfLikeFn _ _ (AvalonStreamM2S { _data }) True _ = pure (AvalonStreamS2M { _ready = True }, Just _data, False)
  dfLikeFn _ _ _ _ _ = pure (AvalonStreamS2M { _ready = False }, Nothing, False)

instance (KnownNat errorWidth, KnownNat emptyWidth, KnownNat readyLatency, NFDataX dataType) =>
    DfLikeAlternative (AvalonStreamS2M readyLatency) (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) () dataType where
  type DfLikeState (AvalonStreamS2M readyLatency) (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) () dataType
    = (Vec (readyLatency+1) Bool, AvalonStreamM2S channelWidth errorWidth emptyWidth dataType)
  type DfLikeParam (AvalonStreamS2M readyLatency) (AvalonStreamM2S channelWidth errorWidth emptyWidth dataType) () dataType
    = (Unsigned channelWidth)

  dfLikeS0 _ _ = (repeat False, NoAvalonStreamM2S)
  dfLikeBlank _ _ = NoAvalonStreamM2S
  dfLikeFn _ _channel _thisAck _ otpItem = do
    let AvalonStreamS2M thisAck = _thisAck
    ackQueue' <- gets ((thisAck +>>) . fst)
    let ack = last ackQueue'
    sending <- gets snd
    put (ackQueue', sending)
    popped <- case (sending, otpItem) of
      (NoAvalonStreamM2S, Just oi) -> put (ackQueue', AvalonStreamM2S { _data = oi, _channel, _error = 0, _startofpacket = True, _endofpacket = True, _empty = 0 }) >> pure True
      _ -> pure False
    toSend <- gets snd
    case toSend of -- ack might be undefined, so we shouldn't look at it unless we have to
      AvalonStreamM2S{} -> when ack $ put (ackQueue', NoAvalonStreamM2S)
      _ -> pure ()
    pure (toSend, Nothing, popped)


-- DfLike classes for AvalonMemMap
-- TODO keep waitrequest on when not receiving data?

-- TODO add in read
instance (GoodMMSlaveConfig config, NFDataX readDataType, NFDataX writeDataType) =>
    DfLikeAlternative (AvalonSlaveIn config writeDataType) (AvalonSlaveOut config readDataType) writeDataType () where
  type DfLikeState (AvalonSlaveIn config writeDataType) (AvalonSlaveOut config readDataType) writeDataType ()
    = ()
  type DfLikeParam (AvalonSlaveIn config writeDataType) (AvalonSlaveOut config readDataType) writeDataType ()
    = Unsigned (AddrWidth (SShared config))

  dfLikeS0 _ _ = ()
  dfLikeBlank _ _ = boolToMMSlaveAck False
  dfLikeFn _ addr si True _ | isJust (mmSlaveInToMaybe si) && si_addr si == addr = pure (boolToMMSlaveAck True, mmSlaveInToMaybe si, False)
  dfLikeFn _ _ _ _ _ = pure (boolToMMSlaveAck False, Nothing, False)

-- TODO add in read
instance (GoodMMMasterConfig config, NFDataX readDataType, NFDataX writeDataType) =>
    DfLikeAlternative (AvalonMasterIn config readDataType) (AvalonMasterOut config writeDataType) () writeDataType where
  type DfLikeState (AvalonMasterIn config readDataType) (AvalonMasterOut config writeDataType) () writeDataType
    = Maybe writeDataType
  type DfLikeParam (AvalonMasterIn config readDataType) (AvalonMasterOut config writeDataType) () writeDataType
    = Unsigned (AddrWidth (MShared config))

  dfLikeS0 _ _ = Nothing
  dfLikeBlank _ _ = mmMasterOutNoData
  dfLikeFn _ addr mi _ otpItem = do
    sending <- get
    retVal <- case (sending, otpItem) of
      (Just toSend, _) -> pure (mmMasterOutSendingData { mo_writeData = toSend, mo_addr = addr }, Nothing, False)
      (Nothing, Just oi) -> put (Just oi) >> pure (mmMasterOutSendingData { mo_writeData = oi, mo_addr = addr }, Nothing, True)
      (Nothing, Nothing) -> pure (mmMasterOutNoData, Nothing, False)
    shouldReadAck <- gets isJust -- ack might be undefined, so we shouldn't look at it unless we have to
    when (shouldReadAck && mmMasterInToBool mi) $ put Nothing
    pure retVal


-- | Map a function over DfLike transferred data; if the function returns Nothing, we don't send it along to the right
mapMaybe ::
  HiddenClockResetEnable dom =>
  DfLikeAlternative inpA otpA datA readA =>
  DfLikeAlternative inpB otpB readB datB =>
  Proxy (inpA,otpA,datA,readA) ->
  Proxy (inpB,otpB,readB,datB) ->
  DfLikeParam inpA otpA datA readA ->
  DfLikeParam inpB otpB readB datB ->
  (datA -> Maybe datB) ->
  (Signal dom inpA, Signal dom inpB) ->
  (Signal dom otpA, Signal dom otpB)
mapMaybe pxyA pxyB paramA paramB mapFn = unbundle . mealy machineAsFunction s0 . bundle where
  s0 = (dfLikeS0 pxyA paramA, dfLikeS0 pxyB paramB)
  -- TODO reset
  machineAsFunction (sA, sB) (inpA, inpB) = let
    ((otpA, dat, _), sA') = runState (dfLikeFn pxyA paramA inpA (isJust dat && (ack || isNothing fdat)) Nothing) sA
    ((otpB, _, ack), sB') = runState (dfLikeFn pxyB paramB inpB False fdat) sB
    fdat = mapFn =<< dat
    in ((sA', sB'), (otpA, otpB))

-- | Map a function over DfLike transferred data
map ::
  HiddenClockResetEnable dom =>
  DfLikeAlternative inpA otpA datA readA =>
  DfLikeAlternative inpB otpB readB datB =>
  Proxy (inpA,otpA,datA,readA) ->
  Proxy (inpB,otpB,readB,datB) ->
  DfLikeParam inpA otpA datA readA ->
  DfLikeParam inpB otpB readB datB ->
  (datA -> datB) ->
  (Signal dom inpA, Signal dom inpB) ->
  (Signal dom otpA, Signal dom otpB)
map pxyA pxyB paramA paramB mapFn = mapMaybe pxyA pxyB paramA paramB (Just . mapFn)

-- | Use a boolean function to filter DfLike transferred data
filter ::
  HiddenClockResetEnable dom =>
  DfLikeAlternative inpA otpA dat readA =>
  DfLikeAlternative inpB otpB readB dat =>
  Proxy (inpA,otpA,dat,readA) ->
  Proxy (inpB,otpB,readB,dat) ->
  DfLikeParam inpA otpA dat readA ->
  DfLikeParam inpB otpB readB dat ->
  (dat -> Bool) ->
  (Signal dom inpA, Signal dom inpB) ->
  (Signal dom otpA, Signal dom otpB)
filter pxyA pxyB paramA paramB filterFn = mapMaybe pxyA pxyB paramA paramB (\a -> if filterFn a then Just a else Nothing)


-- | Generalized fifo for DfLike
-- Uses blockram to store data
fifo ::
  HiddenClockResetEnable dom =>
  KnownNat depth =>
  NFDataX dat =>
  DfLikeAlternative inpA otpA dat readA =>
  DfLikeAlternative inpB otpB readB dat =>
  Proxy (inpA,otpA,dat,readA) ->
  Proxy (inpB,otpB,readB,dat) ->
  DfLikeParam inpA otpA dat readA ->
  DfLikeParam inpB otpB readB dat ->
  SNat depth ->
  (Signal dom inpA, Signal dom inpB) ->
  (Signal dom otpA, Signal dom otpB)
fifo pxyA pxyB paramA paramB fifoDepth = hideReset circuitFunction where

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
  machineAsFunction _ (_, True, _, _) = (s0, (0, Nothing, dfLikeBlank pxyA paramA, dfLikeBlank pxyB paramB))
  machineAsFunction (sA,sB,rAddr,wAddr,amtLeft) (brRead, False, iA, iB) =
    let -- run the input port state machine
        ((oA, maybePush, _), sA') = runState (dfLikeFn pxyA paramA iA (amtLeft > 0) Nothing) sA
        -- potentially push an item onto blockram
        brWrite = (wAddr,) <$> maybePush
        -- adjust write address and amount left (output state machine doesn't see amountLeft')
        (wAddr', amtLeft') = if (isJust maybePush) then (incIdxLooping wAddr, amtLeft-1) else (wAddr, amtLeft)
        -- if we're about to push onto an empty queue, we can pop immediately instead
        (brRead_, amtLeft_) = if (amtLeft == maxBound && isJust maybePush) then (fromJust maybePush, amtLeft') else (brRead, amtLeft)
        -- run the output port state machine
        ((oB, _, popped), sB') = runState (dfLikeFn pxyB paramB iB False (if (amtLeft_ < maxBound) then Just brRead_ else Nothing)) sB
        -- adjust blockram read address and amount left
        (rAddr', amtLeft'') = if popped then (incIdxLooping rAddr, amtLeft'+1) else (rAddr, amtLeft')
        brReadAddr = rAddr'
        -- return our new state and outputs
    in  ((sA', sB', rAddr', wAddr', amtLeft''), (brReadAddr, brWrite, oA, oB))

  -- initial state
  -- (s0 for input port (taken from class), s0 for output port (taken from class), next read address, next write address, space left in bram)
  s0 = (dfLikeS0 pxyA paramA, dfLikeS0 pxyB paramB, _0 fifoDepth, _0 fifoDepth, _maxBound fifoDepth)

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
