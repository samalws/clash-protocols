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
import           Protocols.Internal


-- | Describes how a protocol behaves at one side of a circuit, using a state machine
-- Can take parameters and provide whatever state type you would like.
-- Defaults to being the right side of the circuit (Fwd ~ Otp, Bwd ~ Inp),
-- but this can be switched using 'Reverse' from 'Protocols.Internal'.
-- Supports both input data and output data (TODO is it really "Df Like" then if there's data going both ways?)
class
  ( Protocol df
  , Fwd df ~ Unbundled (Dom df) (OtpMsg  df)
  , Bwd df ~ Unbundled (Dom df) (InpMsg  df)
  , NFDataX (DfLikeState df)
  , Bundle (InpMsg  df)
  , Bundle (OtpMsg  df))
  => DfLikeAlternative df where
  -- | Domain that messages are being sent over
  type Dom df :: Domain
  -- | Messages being sent in to this df port
  type InpMsg  df
  -- | Messages being sent out of this df port
  type OtpMsg  df
  -- | Information being sent in to this df port
  type InpPayload df
  -- | Information being sent out of this df port
  type OtpPayload df
  -- | State carried between clock cycles
  type DfLikeState df
  -- | User-provided parameters (e.g. address to respond to)
  type DfLikeParam df
  -- | Initial state, given user params
  dfLikeS0 :: Proxy df -> DfLikeParam df -> DfLikeState df
  -- | Blank input, used when reset is on.
  -- Doesn't look at current state, but can look at user params.
  -- Should not acknowledge any incoming data; doing so will result in data loss
  dfLikeBlank :: Proxy df -> DfLikeParam df -> OtpMsg  df
  -- | State machine run every clock cycle at this port.
  -- Given user-provided params; input message at the port; acknowledge for inp payload; and Maybe the next write payload to output.
  -- Can update state using State monad.
  -- Returns message to output to the port; Maybe payload inputted from the port; and whether an output payload item was taken.
  -- The 'Maybe OtpPayload' argument is allowed to change arbitrarily between clock cycles, as is the 'Maybe InpPayload' return value.
  -- If the 'Maybe InpPayload' return value is 'Nothing', the 'Bool' argument must be 'True';
  --   the same goes for the 'Maybe OtpPayload' argument and the 'Bool' return value.
  dfLikeFn :: Proxy df -> DfLikeParam df -> InpMsg  df -> Bool -> Maybe (OtpPayload df) -> State (DfLikeState df) (OtpMsg  df, Maybe (InpPayload df), Bool)


-- DfLike classes for Df

instance (NFDataX dat) => DfLikeAlternative (Reverse (Df dom dat)) where
  type Dom         (Reverse (Df dom dat)) = dom
  type InpMsg      (Reverse (Df dom dat)) = Data dat
  type OtpMsg      (Reverse (Df dom dat)) = Ack
  type InpPayload  (Reverse (Df dom dat)) = dat
  type OtpPayload  (Reverse (Df dom dat)) = ()
  type DfLikeState (Reverse (Df dom dat)) = ()
  type DfLikeParam (Reverse (Df dom dat)) = ()
  dfLikeS0 _ _ = ()
  dfLikeBlank _ _ = Ack False
  dfLikeFn _ _ (Data inp) ack _ = pure (Ack ack, Just inp, False)
  dfLikeFn _ _ _ _ _ = pure (Ack False, Nothing, False)

instance (NFDataX dat) => DfLikeAlternative (Df dom dat) where
  type Dom         (Df dom dat) = dom
  type InpMsg      (Df dom dat) = Ack
  type OtpMsg      (Df dom dat) = Data dat
  type InpPayload  (Df dom dat) = ()
  type OtpPayload  (Df dom dat) = dat
  type DfLikeState (Df dom dat) = Maybe dat
  type DfLikeParam (Df dom dat) = ()
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
  type InpMsg
    (Reverse (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser),
     Reverse (Axi4WriteData dom 'KeepStrobe wdBytes wdUser),
     Axi4WriteResponse dom 'KeepResponse iw wrUser)
    = (M2S_WriteAddress 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser,
       M2S_WriteData 'KeepStrobe wdBytes wdUser,
       M2S_WriteResponse)
  type OtpMsg
    (Reverse (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser),
     Reverse (Axi4WriteData dom 'KeepStrobe wdBytes wdUser),
     Axi4WriteResponse dom 'KeepResponse iw wrUser)
    =  (S2M_WriteAddress,
        S2M_WriteData,
        S2M_WriteResponse 'KeepResponse iw wrUser)
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
  type DfLikeState
    (Reverse (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser),
     Reverse (Axi4WriteData dom 'KeepStrobe wdBytes wdUser),
     Axi4WriteResponse dom 'KeepResponse iw wrUser)
    = (Maybe (BitVector (Width iw)), S2M_WriteResponse 'KeepResponse iw wrUser)
    -- (Just write id if we're being written to (otherwise Nothing), write response)
  type DfLikeParam
    (Reverse (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser),
     Reverse (Axi4WriteData dom 'KeepStrobe wdBytes wdUser),
     Axi4WriteResponse dom 'KeepResponse iw wrUser)
    = (BitVector (Width aw), wrUser)
    -- write data address, user data for write response

  dfLikeS0 _ _ = (Nothing, S2M_NoWriteResponse)

  dfLikeBlank _ _ = (S2M_WriteAddress{_awready = False}, S2M_WriteData{_wready = False}, S2M_NoWriteResponse)

  dfLikeFn _ (dataAddr,wrUser) (wAddrVal, wDataVal, wRespAck) ack _ = do
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
  type InpMsg
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
    = (M2S_ReadAddress 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData,
       M2S_ReadData)
  type OtpMsg
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
    = (S2M_ReadAddress,
       S2M_ReadData 'KeepResponse iw rdUser dat)
  type InpPayload
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
     = ()
  type OtpPayload
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
     = dat
  type DfLikeState
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
    = (Index (2^8), BitVector (Width iw), S2M_ReadData 'KeepResponse iw rdUser dat)
    -- (burst length left, read id, read data currently sending)
  type DfLikeParam
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
    = (BitVector (Width aw), rdUser)
    -- data address, user responses

  dfLikeS0 _ _ =
    (0,
     errorX "DfLike for Axi4: No initial value for read id",
     S2M_NoReadData)

  dfLikeBlank _ _ = (S2M_ReadAddress { _arready = False }, S2M_NoReadData)

  dfLikeFn _ (dataAddr,usr) (addrVal, dataAck) _ otpItem = do
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
  type InpMsg       (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) = Axi4StreamM2S idWidth destWidth () (Vec dataLen Axi4StreamByte)
  type OtpMsg       (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) = Axi4StreamS2M
  type InpPayload   (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) = Vec dataLen Axi4StreamByte
  type OtpPayload   (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) = ()
  type DfLikeState  (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) = ()
  type DfLikeParam  (Reverse (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte))) = ()

  dfLikeS0 _ _ = ()
  dfLikeBlank _ _ = Axi4StreamS2M { _tready = False }
  dfLikeFn _ _ (Axi4StreamM2S { _tdata }) ack _ = pure (Axi4StreamS2M { _tready = ack }, Just _tdata, False)
  dfLikeFn _ _ _ _ _ = pure (Axi4StreamS2M { _tready = False }, Nothing, False)

instance (KnownNat idWidth, KnownNat destWidth, KnownNat dataLen) =>
  DfLikeAlternative (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) where
  type Dom          (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = dom
  type InpMsg       (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = Axi4StreamS2M
  type OtpMsg       (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = Axi4StreamM2S idWidth destWidth () (Vec dataLen Axi4StreamByte)
  type InpPayload   (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = ()
  type OtpPayload   (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = (Vec dataLen Axi4StreamByte)
  type DfLikeState  (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = Axi4StreamM2S idWidth destWidth () (Vec dataLen Axi4StreamByte)
  type DfLikeParam  (Axi4Stream dom idWidth destWidth () (Vec dataLen Axi4StreamByte)) = Unsigned destWidth

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
  DfLikeAlternative (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) where
  type Dom          (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = dom
  type InpMsg       (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = AvalonStreamM2S channelWidth errorWidth emptyWidth dataType
  type OtpMsg       (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = AvalonStreamS2M 0
  type InpPayload   (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = dataType
  type OtpPayload   (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = ()
  type DfLikeState  (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = ()
  type DfLikeParam  (Reverse (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType)) = ()

  dfLikeS0 _ _ = ()
  dfLikeBlank _ _ = AvalonStreamS2M { _ready = False }
  dfLikeFn _ _ (AvalonStreamM2S { _data }) ack _ = pure (AvalonStreamS2M { _ready = ack }, Just _data, False)
  dfLikeFn _ _ _ _ _ = pure (AvalonStreamS2M { _ready = False }, Nothing, False)

instance (KnownNat errorWidth, KnownNat emptyWidth, KnownNat readyLatency, NFDataX dataType) =>
  DfLikeAlternative (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) where
  type Dom          (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = dom
  type InpMsg       (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = AvalonStreamS2M readyLatency
  type OtpMsg       (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = AvalonStreamM2S channelWidth errorWidth emptyWidth dataType
  type InpPayload   (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = ()
  type OtpPayload   (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = dataType
  type DfLikeState  (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = (Vec (readyLatency+1) Bool, AvalonStreamM2S channelWidth errorWidth emptyWidth dataType)
  type DfLikeParam  (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = Unsigned channelWidth

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
  DfLikeAlternative (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) where
  type Dom          (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = dom
  type InpMsg       (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = AvalonSlaveIn config writeDataType
  type OtpMsg       (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = AvalonSlaveOut config readDataType
  type InpPayload   (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = writeDataType
  type OtpPayload   (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = ()
  type DfLikeState  (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = ()
  type DfLikeParam  (Reverse (AvalonMMSlave dom 0 config readDataType writeDataType)) = Unsigned (AddrWidth (SShared config))

  dfLikeS0 _ _ = ()
  dfLikeBlank _ _ = boolToMMSlaveAck False
  dfLikeFn _ addr si ack _ | isJust (mmSlaveInToMaybe si) && si_addr si == addr = pure (boolToMMSlaveAck ack, mmSlaveInToMaybe si, False)
  dfLikeFn _ _ _ _ _ = pure (boolToMMSlaveAck False, Nothing, False)

-- TODO add in read
instance (GoodMMMasterConfig config, NFDataX readDataType, NFDataX writeDataType) =>
  DfLikeAlternative (AvalonMMMaster dom config readDataType writeDataType) where
  type Dom          (AvalonMMMaster dom config readDataType writeDataType) = dom
  type InpMsg       (AvalonMMMaster dom config readDataType writeDataType) = AvalonMasterIn config readDataType
  type OtpMsg       (AvalonMMMaster dom config readDataType writeDataType) = AvalonMasterOut config writeDataType
  type InpPayload   (AvalonMMMaster dom config readDataType writeDataType) = ()
  type OtpPayload   (AvalonMMMaster dom config readDataType writeDataType) = writeDataType
  type DfLikeState  (AvalonMMMaster dom config readDataType writeDataType) = Maybe writeDataType
  type DfLikeParam  (AvalonMMMaster dom config readDataType writeDataType) = Unsigned (AddrWidth (MShared config))

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
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  Proxy dfA ->
  Proxy dfB ->
  DfLikeParam dfA ->
  DfLikeParam dfB ->
  (InpPayload dfA -> Maybe (OtpPayload dfB)) ->
  Circuit (Reverse dfA) dfB
mapMaybe pxyA pxyB paramA paramB mapFn = Circuit $ (unbundle *** unbundle) . unbundle . hideReset circuitFunction . bundle . (bundle *** bundle) where
  s0 = (dfLikeS0 pxyA paramA, dfLikeS0 pxyB paramB)
  circuitFunction reset inp = mux (unsafeToHighPolarity reset) (pure (dfLikeBlank pxyA paramA, dfLikeBlank pxyB paramB)) (mealy machineAsFunction s0 inp)
  machineAsFunction (sA, sB) (inpA, inpB) = let
    ((otpA, dat, _), sA') = runState (dfLikeFn pxyA paramA inpA (isJust dat && (ack || isNothing fdat)) Nothing) sA
    ((otpB, _, ack), sB') = runState (dfLikeFn pxyB paramB inpB False fdat) sB
    fdat = mapFn =<< dat
    in ((sA', sB'), (otpA, otpB))

-- | Map a function over DfLike transferred data
map ::
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  Proxy dfA ->
  Proxy dfB ->
  DfLikeParam dfA ->
  DfLikeParam dfB ->
  (InpPayload dfA -> OtpPayload dfB) ->
  Circuit (Reverse dfA) dfB
map pxyA pxyB paramA paramB mapFn = mapMaybe pxyA pxyB paramA paramB (Just . mapFn)

-- | Use a boolean function to filter DfLike transferred data
filter ::
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  InpPayload dfA ~ OtpPayload dfB =>
  Proxy dfA ->
  Proxy dfB ->
  DfLikeParam dfA ->
  DfLikeParam dfB ->
  (InpPayload dfA -> Bool) ->
  Circuit (Reverse dfA) dfB
filter pxyA pxyB paramA paramB filterFn = mapMaybe pxyA pxyB paramA paramB (\a -> if filterFn a then Just a else Nothing)

-- | Like 'Prelude.zipWith'. User-provided function combines input data from two sources
zipWith ::
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  DfLikeAlternative dfC =>
  Dom dfA ~ Dom dfB =>
  Dom dfA ~ Dom dfC =>
  HiddenClockResetEnable (Dom dfA) =>
  Proxy dfA ->
  Proxy dfB ->
  Proxy dfC ->
  DfLikeParam dfA ->
  DfLikeParam dfB ->
  DfLikeParam dfC ->
  (InpPayload dfA -> InpPayload dfB -> OtpPayload dfC) ->
  Circuit (Reverse dfA, Reverse dfB) dfC
zipWith pxyA pxyB pxyC paramA paramB paramC mapFn = Circuit $ ((unbundle *** unbundle) *** unbundle) . first unbundle . unbundle . hideReset circuitFunction . bundle . first bundle . ((bundle *** bundle) *** bundle) where
  s0 = (dfLikeS0 pxyA paramA, dfLikeS0 pxyB paramB, dfLikeS0 pxyC paramC)
  circuitFunction reset inp = mux (unsafeToHighPolarity reset) (pure ((dfLikeBlank pxyA paramA, dfLikeBlank pxyB paramB), dfLikeBlank pxyC paramC)) (mealy machineAsFunction s0 inp)
  machineAsFunction (sA, sB, sC) ((inpA, inpB), inpC) = let
    ((otpA, datA, _), sA') = runState (dfLikeFn pxyA paramA inpA (isJust fdat && ack) Nothing) sA
    ((otpB, datB, _), sB') = runState (dfLikeFn pxyB paramB inpB (isJust fdat && ack) Nothing) sB
    ((otpC, _, ack), sC') = runState (dfLikeFn pxyC paramC inpC False fdat) sC
    fdat = mapFn <$> datA <*> datB
    in ((sA', sB', sC'), ((otpA, otpB), otpC))

-- | Copy data of a single DfLike stream to multiple
fanout ::
  DfLikeAlternative dfA =>
  DfLikeAlternative dfB =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  InpPayload dfA ~ OtpPayload dfB =>
  NFDataX (InpPayload dfA) =>
  KnownNat numB =>
  Proxy dfA ->
  Proxy dfB ->
  DfLikeParam dfA ->
  Vec numB (DfLikeParam dfB) ->
  Circuit (Reverse dfA) (Vec numB dfB)
fanout pxyA pxyB paramA paramsB = Circuit $ (unbundle *** fmap unbundle . unbundle) . unbundle . hideReset circuitFunction . bundle . (bundle *** bundle . fmap bundle) where
  s0 = (dfLikeS0 pxyA paramA, dfLikeS0 pxyB <$> paramsB, Nothing, repeat False)
  circuitFunction reset inp = mux (unsafeToHighPolarity reset) (pure (dfLikeBlank pxyA paramA, dfLikeBlank pxyB <$> paramsB)) (mealy machineAsFunction s0 inp)
  machineAsFunction (sA, sBs, sendingDat, alreadySents) (inpA, inpsB) = let
    ((otpA, dat, _), sA') = runState (dfLikeFn pxyA paramA inpA (isNothing sendingDat && isJust dat) Nothing) sA
    sendingDat' = sendingDat <|> dat
    alreadySents' = if (isJust sendingDat' && isNothing sendingDat) then pure False else alreadySents
    (otpB, (alreadySents'', sBs')) = second unzip . unzip $ individualBFn sendingDat' <$> alreadySents' <*> paramsB <*> inpsB <*> sBs
    sendingDat'' = if alreadySents'' == repeat True then Nothing else sendingDat'
    in ((sA', sBs', sendingDat'', alreadySents''), (otpA, otpB))
  individualBFn maybeDat alreadySent param inp dfState = let
    ((otp, _, ack), dfState') = runState (dfLikeFn pxyB param inp False (if alreadySent then Nothing else maybeDat)) dfState
    alreadySent' = ack || alreadySent
    in (otp, (alreadySent', dfState'))

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
  Proxy dfA ->
  Proxy dfB ->
  DfLikeParam dfA ->
  DfLikeParam dfB ->
  SNat depth ->
  Circuit (Reverse dfA) dfB
fifo pxyA pxyB paramA paramB fifoDepth = Circuit ((unbundle *** unbundle) . hideReset circuitFunction . (bundle *** bundle)) where

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
