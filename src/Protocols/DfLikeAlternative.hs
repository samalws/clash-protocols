{-|
Defines a mix-and-match interface for creating fifo buffers.
Buffers can be made from one protocol to another,
and are parametrized on the amount of items in the buffer.
Blockram is used to store fifo buffer items.
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, UndecidableInstances #-}

module Protocols.DfLikeAlternative where

import           Prelude hiding (replicate, last, repeat, unzip)
import           Control.Arrow (first, second, (***))
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
import           Protocols.Axi4.Common (KeepBurst(..), KeepSize(..), KeepBurstLength(..), KeepResponse(..), KeepStrobe(..), Width, BurstMode(..), Resp(..))
import           Protocols.Axi4.ReadAddress (Axi4ReadAddress, M2S_ReadAddress(..), S2M_ReadAddress(..))
import           Protocols.Axi4.ReadData (Axi4ReadData, M2S_ReadData(..), S2M_ReadData(..))
import           Protocols.Axi4.WriteAddress (Axi4WriteAddress, M2S_WriteAddress(..), S2M_WriteAddress(..))
import           Protocols.Axi4.WriteData (Axi4WriteData, M2S_WriteData(..), S2M_WriteData(..))
import           Protocols.Axi4.WriteResponse (Axi4WriteResponse, M2S_WriteResponse(..), S2M_WriteResponse(..))
import           Protocols.Axi4.Stream.Axi4Stream
import           Protocols.Df (Data(..), Df)
import qualified Protocols.Df as Df
import           Protocols.Internal


-- | Describes how a protocol behaves at one side of a circuit, using a state machine
-- Can take parameters and provide whatever state type you would like.
-- Defaults to being the right side of the circuit (Fwd ~ Otp, Bwd ~ Inp),
-- but this can be switched using 'Reverse' from 'Protocols.Internal'.
-- Supports both input data and output data (TODO is it really "Df Like" then if there's data going both ways?)
class (Protocol df) => DfLikeAlternative df where
  -- | Domain that messages are being sent over
  type Dom df :: Domain
  -- | Information being sent in to this df port
  type InpPayload df
  -- | Information being sent out of this df port
  type OtpPayload df
  -- | User-provided parameters (e.g. address to respond to)
  type DfLikeParam df
  -- | Circuit which converts Df into this protocol's messages.
  -- There are two Df channels, one for read data and one for write data
  dfLikeCkt :: (HiddenClockResetEnable (Dom df))
    => DfLikeCktArgs df
    -> Circuit (Df (Dom df) (OtpPayload df), Reverse (Df (Dom df) (InpPayload df))) df

type DfLikeCktArgs df = (Proxy df, DfLikeParam df)

dfLikeCktHelper ::
  ( HiddenClockResetEnable (Dom df)
  , Protocol df
  , Bwd df ~ Unbundled (Dom df) inpMsg
  , Fwd df ~ Unbundled (Dom df) otpMsg
  , NFDataX state
  , Bundle inpMsg
  , Bundle otpMsg
  )
  => state
  -> otpMsg
  -> ( inpMsg
    -> Bool
    -> Maybe (OtpPayload df)
    -> State state (otpMsg, Maybe (InpPayload df), Bool)
     )
  -> Circuit (Df (Dom df) (OtpPayload df), Reverse (Df (Dom df) (InpPayload df))) df
dfLikeCktHelper s0 blankOtp stateFn = Circuit $ (unbundle *** unbundle) . unbundle . hideReset cktFn . bundle . (bundle *** bundle) where
  cktFn reset inp = mealy transFn s0 ((,) <$> unsafeToHighPolarity reset <*> inp)
  transFn _ (True, _) = (s0, ((Ack False, NoData), blankOtp))
  transFn s (False, ((toOtp, Ack inpAck), inp)) = let
    ((otp, inputted, otpAck), s') = runState (stateFn inp inpAck (Df.dataToMaybe toOtp)) s
    in (s', ((Ack otpAck, Df.maybeToData inputted), otp))


-- DfLike classes for Df

instance (NFDataX dat) => DfLikeAlternative (Reverse (Df dom dat)) where
  type Dom         (Reverse (Df dom dat)) = dom
  type InpPayload  (Reverse (Df dom dat)) = dat
  type OtpPayload  (Reverse (Df dom dat)) = ()
  type DfLikeParam (Reverse (Df dom dat)) = ()
  dfLikeCkt _ = Circuit (\((_,b),c) -> ((pure (Ack False),c),b))

instance (NFDataX dat) => DfLikeAlternative (Df dom dat) where
  type Dom         (Df dom dat) = dom
  type InpPayload  (Df dom dat) = ()
  type OtpPayload  (Df dom dat) = dat
  type DfLikeParam (Df dom dat) = ()
  dfLikeCkt _ = Circuit (\((a,_),c) -> ((c,pure NoData),a))


-- Fifo classes for Axi4

instance (NFDataX wrUser, KnownNat wdBytes, KnownNat (Width aw), KnownNat (Width iw)) =>
  DfLikeAlternative
    (Reverse (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser),
     Reverse (Axi4WriteData dom 'KeepStrobe wdBytes wdUser),
     Axi4WriteResponse dom 'KeepResponse iw wrUser)
    where

  type Dom
    (Reverse (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser),
     Reverse (Axi4WriteData dom 'KeepStrobe wdBytes wdUser),
     Axi4WriteResponse dom 'KeepResponse iw wrUser)
    = dom
  type InpPayload
    (Reverse (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser),
     Reverse (Axi4WriteData dom 'KeepStrobe wdBytes wdUser),
     Axi4WriteResponse dom 'KeepResponse iw wrUser)
    = Vec wdBytes (Maybe (BitVector 8))
  type OtpPayload
    (Reverse (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser),
     Reverse (Axi4WriteData dom 'KeepStrobe wdBytes wdUser),
     Axi4WriteResponse dom 'KeepResponse iw wrUser)
    = ()
  type DfLikeParam
    (Reverse (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser),
     Reverse (Axi4WriteData dom 'KeepStrobe wdBytes wdUser),
     Axi4WriteResponse dom 'KeepResponse iw wrUser)
    = (BitVector (Width aw), wrUser)
    -- write data address, user data for write response

  dfLikeCkt (_, param) = dfLikeCktHelper s0 blankOtp (stateFn param) where
    s0 = (Nothing, S2M_NoWriteResponse)
   
    blankOtp = (S2M_WriteAddress{_awready = False}, S2M_WriteData{_wready = False}, S2M_NoWriteResponse)
   
    stateFn (dataAddr,wrUser) (wAddrVal, wDataVal, wRespAck) ack _ = do
      wAddrAck <- processWAddr wAddrVal
      (wDataAck, inpItem) <- processWData wDataVal
      wRespVal <- gets snd
      processWRespAck
      pure ((wAddrAck, wDataAck, wRespVal), inpItem, False)
      where
   
      processWAddr M2S_NoWriteAddress = pure (S2M_WriteAddress{_awready = False})
      processWAddr M2S_WriteAddress{ _awburst } | _awburst /= BmFixed = pure (S2M_WriteAddress{_awready = True})
      processWAddr M2S_WriteAddress{_awaddr, _awid} = do
        (_,b) <- get
        put (if _awaddr == dataAddr then Just _awid else Nothing, b)
        pure (S2M_WriteAddress{_awready = True})
   
      processWData M2S_NoWriteData = pure (S2M_WriteData{_wready = False}, Nothing)
      processWData M2S_WriteData{_wlast, _wdata} = do
        (shouldRead,respS2M) <- get
        -- we only want to output _wready = false if we're the recpient of the writes AND ack is false
        -- we only want to return data if we're the recpient of the writes
        -- we only want to output on writeresponse if we're the recipient of the writes AND ack is true
        case (shouldRead, ack) of
          (Nothing, _) -> pure (S2M_WriteData{_wready = True}, Nothing)
          (Just _, False) -> pure (S2M_WriteData{_wready = False}, Just _wdata)
          (Just sr, True) -> do
            put (Nothing,
                 if _wlast then S2M_WriteResponse {_bid = sr, _bresp = ROkay, _buser = wrUser } else respS2M)
            pure (S2M_WriteData{_wready = True}, Just _wdata)
   
      processWRespAck = when (_bready wRespAck) $ modify (\(a,_) -> (a,S2M_NoWriteResponse))

instance (NFDataX dat, NFDataX rdUser, KnownNat (Width aw), KnownNat (Width iw)) =>
  DfLikeAlternative
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
    where

  type Dom
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
     = dom
  type InpPayload
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
     = ()
  type OtpPayload
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
     = dat
  type DfLikeParam
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
    = (BitVector (Width aw), rdUser)
    -- data address, user responses

  dfLikeCkt (_, param) = dfLikeCktHelper s0 blankOtp (stateFn param) where
    s0 =
      (0,
       errorX "DfLike for Axi4: No initial value for read id",
       S2M_NoReadData)
   
    blankOtp = (S2M_ReadAddress { _arready = False }, S2M_NoReadData)
   
    stateFn (dataAddr,usr) (addrVal, dataAck) _ otpItem = do
      addrAck <- processAddr addrVal
      (dataVal,sentData) <- sendData
      processDataAck dataAck
      pure ((addrAck,dataVal),Nothing,sentData)
      where
        processAddr M2S_NoReadAddress = pure (S2M_ReadAddress { _arready = False })
        processAddr M2S_ReadAddress{_arburst} | _arburst /= BmFixed = pure (S2M_ReadAddress{ _arready = True })
        processAddr M2S_ReadAddress{_araddr,_arlen,_arid} = do
          (burstLenLeft,_,c) <- get
          when (burstLenLeft == 0 && (_araddr == dataAddr)) $ put (_arlen, _arid, c)
          pure (S2M_ReadAddress{ _arready = burstLenLeft == 0 })
   
        sendData = do
          (burstLenLeft,readId,currOtp) <- get
          sentData <- case (currOtp, burstLenLeft == 0, otpItem) of
            (S2M_NoReadData, False, Just oi) -> do
              put (burstLenLeft-1, readId, S2M_ReadData { _rid = readId, _rdata = oi, _rresp = ROkay, _rlast = burstLenLeft == 1, _ruser = usr })
              pure True
            _ -> pure False
          (_,_,currOtp') <- get
          pure (currOtp', sentData)
   
        processDataAck M2S_ReadData{_rready} = when _rready $ do
          (a,b,_) <- get
          put (a,b,S2M_NoReadData)


-- DfLike classes for Axi4Stream

instance (KnownNat idWidth, KnownNat destWidth) =>
  DfLikeAlternative (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) where
  type Dom          (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) = dom
  type InpPayload   (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) = Vec dataLen Axi4StreamByte
  type OtpPayload   (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) = ()
  type DfLikeParam  (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) = ()

  dfLikeCkt _ = dfLikeCktHelper s0 blankOtp stateFn where
    s0 = ()
    blankOtp = Axi4StreamS2M { _tready = False }
    stateFn (Axi4StreamM2S { _tdata }) ack _ = pure (Axi4StreamS2M { _tready = ack }, Just _tdata, False)
    stateFn _ _ _ = pure (Axi4StreamS2M { _tready = False }, Nothing, False)

instance (KnownNat idWidth, KnownNat destWidth, KnownNat dataLen) =>
  DfLikeAlternative (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) where
  type Dom          (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = dom
  type InpPayload   (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = ()
  type OtpPayload   (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = (Vec dataLen Axi4StreamByte)
  type DfLikeParam  (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = Unsigned destWidth

  dfLikeCkt (_, param) = dfLikeCktHelper s0 blankOtp (stateFn param) where
    s0 = NoAxi4StreamM2S
    blankOtp = NoAxi4StreamM2S
    stateFn _tdest _ack _ otpItem = do
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
  DfLikeAlternative (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) where
  type Dom          (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = dom
  type InpPayload   (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = dataType
  type OtpPayload   (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = ()
  type DfLikeParam  (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = ()

  dfLikeCkt _ = dfLikeCktHelper s0 blankOtp stateFn where
    s0 = ()
    blankOtp = AvalonStreamS2M { _ready = False }
    stateFn (AvalonStreamM2S { _data }) ack _ = pure (AvalonStreamS2M { _ready = ack }, Just _data, False)
    stateFn _ _ _ = pure (AvalonStreamS2M { _ready = False }, Nothing, False)

instance (KnownNat errorWidth, KnownNat emptyWidth, KnownNat readyLatency, NFDataX dataType) =>
  DfLikeAlternative (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) where
  type Dom          (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = dom
  type InpPayload   (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = ()
  type OtpPayload   (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = dataType
  type DfLikeParam  (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = Unsigned channelWidth

  dfLikeCkt (proxy, param) = dfLikeCktHelper s0 blankOtp (stateFn param) where
    vec0 :: Proxy (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) -> Vec (readyLatency+1) Bool
    vec0 _ = repeat False
    s0 = (vec0 proxy, NoAvalonStreamM2S)
    blankOtp = NoAvalonStreamM2S
    stateFn _channel _thisAck _ otpItem = do
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
  DfLikeAlternative (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) where
  type Dom          (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = dom
  type InpPayload   (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = writeDataType
  type OtpPayload   (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = ()
  type DfLikeParam  (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = Unsigned (AddrWidth (SShared config))

  dfLikeCkt (_, param) = dfLikeCktHelper s0 blankOtp (stateFn param) where
    s0 = ()
    blankOtp = boolToMMSlaveAck False
    stateFn addr si ack _ | isJust (mmSlaveInToMaybe si) && si_addr si == addr = pure (boolToMMSlaveAck ack, mmSlaveInToMaybe si, False)
    stateFn _ _ _ _ = pure (boolToMMSlaveAck False, Nothing, False)

-- TODO add in read
instance (GoodMMMasterConfig config, NFDataX readDataType, NFDataX writeDataType) =>
  DfLikeAlternative (AvalonMMMaster dom config readDataType writeDataType) where
  type Dom          (AvalonMMMaster dom config readDataType writeDataType) = dom
  type InpPayload   (AvalonMMMaster dom config readDataType writeDataType) = ()
  type OtpPayload   (AvalonMMMaster dom config readDataType writeDataType) = writeDataType
  type DfLikeParam  (AvalonMMMaster dom config readDataType writeDataType) = Unsigned (AddrWidth (MShared config))

  dfLikeCkt (_, param) = dfLikeCktHelper s0 blankOtp (stateFn param) where
    s0 = Nothing
    blankOtp = mmMasterOutNoData
    stateFn addr mi _ otpItem = do
      sending <- get
      retVal <- case (sending, otpItem) of
        (Just toSend, _) -> pure (mmMasterOutSendingData { mo_writeData = toSend, mo_addr = addr }, Nothing, False)
        (Nothing, Just oi) -> put (Just oi) >> pure (mmMasterOutSendingData { mo_writeData = oi, mo_addr = addr }, Nothing, True)
        (Nothing, Nothing) -> pure (mmMasterOutNoData, Nothing, False)
      shouldReadAck <- gets isJust -- ack might be undefined, so we shouldn't look at it unless we have to
      when (shouldReadAck && mmMasterInToBool mi) $ put Nothing
      pure retVal

dfToDfLikeInp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => DfLikeCktArgs df
  -> Circuit (Reverse df) (Df (Dom df) (InpPayload df))
dfToDfLikeInp
  = (coerceCircuit :: Circuit a (Reverse (Reverse b)) -> Circuit a b)
  . reverseCircuit
  . mapCircuit (pure NoData, ) snd id id
  . dfLikeCkt

dfToDfLikeOtp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => DfLikeCktArgs df
  -> Circuit (Df (Dom df) (OtpPayload df)) df
dfToDfLikeOtp = mapCircuit (, pure (Ack False)) fst id id . dfLikeCkt

vecToDfLikeInp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => KnownNat n
  => Vec n (DfLikeCktArgs df)
  -> Circuit (Vec n (Reverse df)) (Vec n (Df (Dom df) (InpPayload df)))
vecToDfLikeInp = vecCircuits . fmap dfToDfLikeInp

vecToDfLikeOtp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => KnownNat n
  => Vec n (DfLikeCktArgs df)
  -> Circuit (Vec n (Df (Dom df) (OtpPayload df))) (Vec n df)
vecToDfLikeOtp = vecCircuits . fmap dfToDfLikeOtp

tupToDfLikeInp
  :: DfLikeAlternative dfA
  => DfLikeAlternative dfB
  => Dom dfA ~ Dom dfB
  => HiddenClockResetEnable (Dom dfA)
  => (DfLikeCktArgs dfA, DfLikeCktArgs dfB)
  -> Circuit (Reverse (dfA, dfB)) (Df (Dom dfA) (InpPayload dfA), Df (Dom dfB) (InpPayload dfB))
tupToDfLikeInp (argsA, argsB) = coerceCircuit $ tupCircuits (dfToDfLikeInp argsA) (dfToDfLikeInp argsB)

tupToDfLikeOtp
  :: DfLikeAlternative dfA
  => DfLikeAlternative dfB
  => Dom dfA ~ Dom dfB
  => HiddenClockResetEnable (Dom dfA)
  => (DfLikeCktArgs dfA, DfLikeCktArgs dfB)
  -> Circuit (Df (Dom dfA) (OtpPayload dfA), Df (Dom dfB) (OtpPayload dfB)) (dfA, dfB)
tupToDfLikeOtp (argsA, argsB) = coerceCircuit $ tupCircuits (dfToDfLikeOtp argsA) (dfToDfLikeOtp argsB)

-- | Map a function over DfLike transferred data; if the function returns Nothing, we don't send it along to the right
mapMaybe ::
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  DfLikeCktArgs dfA ->
  DfLikeCktArgs dfB ->
  (InpPayload dfA -> Maybe (OtpPayload dfB)) ->
  Circuit (Reverse dfA) dfB
mapMaybe argsA argsB mapFn = dfToDfLikeInp argsA |> Df.mapMaybe mapFn |> dfToDfLikeOtp argsB

-- | Map a function over DfLike transferred data
map ::
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  DfLikeCktArgs dfA ->
  DfLikeCktArgs dfB ->
  (InpPayload dfA -> OtpPayload dfB) ->
  Circuit (Reverse dfA) dfB
map argsA argsB mapFn = dfToDfLikeInp argsA |> Df.map mapFn |> dfToDfLikeOtp argsB

-- | Use a boolean function to filter DfLike transferred data
filter ::
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  InpPayload dfA ~ OtpPayload dfB =>
  DfLikeCktArgs dfA ->
  DfLikeCktArgs dfB ->
  (InpPayload dfA -> Bool) ->
  Circuit (Reverse dfA) dfB
filter argsA argsB filterFn = dfToDfLikeInp argsA |> Df.filter filterFn |> dfToDfLikeOtp argsB

-- | Like 'Prelude.zipWith'. User-provided function combines input data from two sources
zipWith ::
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  DfLikeAlternative dfC =>
  Dom dfA ~ Dom dfB =>
  Dom dfA ~ Dom dfC =>
  HiddenClockResetEnable (Dom dfA) =>
  (DfLikeCktArgs dfA, DfLikeCktArgs dfB) ->
  DfLikeCktArgs dfC ->
  (InpPayload dfA -> InpPayload dfB -> OtpPayload dfC) ->
  Circuit (Reverse (dfA, dfB)) dfC
zipWith argsAB argsC mapFn = tupToDfLikeInp argsAB |> Df.zipWith mapFn |> dfToDfLikeOtp argsC

-- | Copy data of a single DfLike stream to multiple
fanout ::
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  InpPayload dfA ~ OtpPayload dfB =>
  KnownNat numB =>
  1 <= numB =>
  DfLikeCktArgs dfA ->
  Vec numB (DfLikeCktArgs dfB) ->
  Circuit (Reverse dfA) (Vec numB dfB)
fanout argsA argsB = dfToDfLikeInp argsA |> Df.fanout |> vecToDfLikeOtp argsB

-- | Generalized fifo for DfLike
-- Uses blockram to store data
fifo ::
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  KnownNat depth =>
  InpPayload dfA ~ OtpPayload dfB =>
  NFDataX (InpPayload dfA) =>
  DfLikeCktArgs dfA ->
  DfLikeCktArgs dfB ->
  SNat depth ->
  Circuit (Reverse dfA) dfB
fifo argsA argsB fifoDepth = dfToDfLikeInp argsA |> dfFifo |> dfToDfLikeOtp argsB where
  dfFifo = Circuit $ hideReset circuitFunction

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
  machineAsFunction _ (_, True, _, _) = (s0, (0, Nothing, Ack False, NoData))
  machineAsFunction (rAddr,wAddr,amtLeft) (brRead, False, pushData, Ack popped) =
    let -- potentially push an item onto blockram
        maybePush = if amtLeft > 0 then (Df.dataToMaybe pushData) else Nothing
        brWrite = (wAddr,) <$> maybePush
        -- adjust write address and amount left (output state machine doesn't see amountLeft')
        (wAddr', amtLeft') = if (isJust maybePush) then (incIdxLooping wAddr, amtLeft-1) else (wAddr, amtLeft)
        -- if we're about to push onto an empty queue, we can pop immediately instead
        (brRead_, amtLeft_) = if (amtLeft == maxBound && isJust maybePush) then (fromJust maybePush, amtLeft') else (brRead, amtLeft)
        -- adjust blockram read address and amount left
        (rAddr', amtLeft'') = if (amtLeft_ < maxBound && popped) then (incIdxLooping rAddr, amtLeft'+1) else (rAddr, amtLeft')
        brReadAddr = rAddr'
        -- return our new state and outputs
        otpAck = isJust maybePush
        otpDat = if (amtLeft_ < maxBound) then Data brRead_ else NoData
    in  ((rAddr', wAddr', amtLeft''), (brReadAddr, brWrite, Ack otpAck, otpDat))

  -- initial state
  -- (s0 for input port (taken from class), s0 for output port (taken from class), next read address, next write address, space left in bram)
  s0 = (_0 fifoDepth, _0 fifoDepth, _maxBound fifoDepth)

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
