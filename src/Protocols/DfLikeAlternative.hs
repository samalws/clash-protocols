{-|
Defines a class for protocols which are "similar" to 'Df'.
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, UndecidableInstances #-}

module Protocols.DfLikeAlternative where

import           Prelude hiding (replicate, last, repeat, unzip)
import           Control.Arrow ((***))
import           Control.Monad (when)
import           Control.Monad.State (State, runState, get, put, gets, modify)
import           Clash.Prelude hiding ((&&), (||), not)
import           Data.Maybe (isJust, fromJust)
import           Data.Proxy (Proxy(..))

-- me
import           Protocols.Axi4.Common (KeepBurst(..), KeepSize(..), KeepBurstLength(..), KeepResponse(..), KeepStrobe(..), Width, BurstMode(..), Resp(..))
import           Protocols.Axi4.ReadAddress (Axi4ReadAddress, M2S_ReadAddress(..), S2M_ReadAddress(..))
import           Protocols.Axi4.ReadData (Axi4ReadData, M2S_ReadData(..), S2M_ReadData(..))
import           Protocols.Axi4.WriteAddress (Axi4WriteAddress, M2S_WriteAddress(..), S2M_WriteAddress(..))
import           Protocols.Axi4.WriteData (Axi4WriteData, M2S_WriteData(..), S2M_WriteData(..))
import           Protocols.Axi4.WriteResponse (Axi4WriteResponse, M2S_WriteResponse(..), S2M_WriteResponse(..))
import           Protocols.Df (Data(..), Df)
import qualified Protocols.Df as Df
import           Protocols.Internal


-- | Class for protocols that are "similar" to 'Df',
-- i.e. they can be converted into a 'Df' port using a 'Circuit'.
-- Can take parameters, e.g. for addresses.
-- Defaults to being the right side of the circuit (Fwd ~ Otp, Bwd ~ Inp),
-- but this can be switched using 'Reverse' from 'Protocols.Internal'.
-- Supports both input data and output data
class (Protocol df) => DfLikeAlternative df where
  -- | Domain that messages are being sent over
  type Dom df :: Domain
  -- | Information being sent in
  type InpPayload df
  -- | Information being sent out
  type OtpPayload df
  -- | User-provided parameters (e.g. address to respond to)
  type DfLikeParam df
  -- | Circuit which converts Df into this protocol's messages.
  -- There are two Df channels, one for read data and one for write data
  dfLikeCkt :: (HiddenClockResetEnable (Dom df))
    => DfLikeCktArgs df
    -> Circuit (Df (Dom df) (OtpPayload df), Reverse (Df (Dom df) (InpPayload df))) df

-- | Arguments given to 'dfLikeCkt'
type DfLikeCktArgs df = (Proxy df, DfLikeParam df)

-- | Helper function to make it easier to implement 'DfLikeAlternative'.
-- 'Ack's are automatically converted to/from 'Bool's,
-- and 'Data's to/from 'Maybe'.
-- A default 'otpMsg' value is given for if reset is currently on.
-- The 'State' machine is run every clock cycle.
-- Parameters: initial state, default 'otpMsg', and 'State' machine function
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


-- Fifo classes for Axi4 slave port

-- Does not support burst modes other than fixed.
-- Always sends 'ROkay' along 'WriteResponse' channel
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

-- Always sends 'ROkay' along 'WriteResponse' channel
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


-- | Convert 'DfLike' into a *one-way* 'Df' port, at the data input end
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

-- | Convert 'DfLike' into a *one-way* 'Df' port, at the data output end
dfToDfLikeOtp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => DfLikeCktArgs df
  -> Circuit (Df (Dom df) (OtpPayload df)) df
dfToDfLikeOtp = mapCircuit (, pure (Ack False)) fst id id . dfLikeCkt

-- | Convert a vec of 'DfLike's into a vec of *one-way* 'Df' ports, at the data input end
vecToDfLikeInp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => KnownNat n
  => Vec n (DfLikeCktArgs df)
  -> Circuit (Vec n (Reverse df)) (Vec n (Df (Dom df) (InpPayload df)))
vecToDfLikeInp = vecCircuits . fmap dfToDfLikeInp

-- | Convert a vec of 'DfLike's into a vec of *one-way* 'Df' ports, at the data output end
vecToDfLikeOtp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => KnownNat n
  => Vec n (DfLikeCktArgs df)
  -> Circuit (Vec n (Df (Dom df) (OtpPayload df))) (Vec n df)
vecToDfLikeOtp = vecCircuits . fmap dfToDfLikeOtp

-- | Convert a pair of (possibly different from each other) 'DfLike's into a pair of *one-way* 'Df' ports, at the data input end
tupToDfLikeInp
  :: DfLikeAlternative dfA
  => DfLikeAlternative dfB
  => Dom dfA ~ Dom dfB
  => HiddenClockResetEnable (Dom dfA)
  => (DfLikeCktArgs dfA, DfLikeCktArgs dfB)
  -> Circuit (Reverse (dfA, dfB)) (Df (Dom dfA) (InpPayload dfA), Df (Dom dfB) (InpPayload dfB))
tupToDfLikeInp (argsA, argsB) = coerceCircuit $ tupCircuits (dfToDfLikeInp argsA) (dfToDfLikeInp argsB)

-- | Convert a pair of (possibly different from each other) 'DfLike's into a pair of *one-way* 'Df' ports, at the data output end
tupToDfLikeOtp
  :: DfLikeAlternative dfA
  => DfLikeAlternative dfB
  => Dom dfA ~ Dom dfB
  => HiddenClockResetEnable (Dom dfA)
  => (DfLikeCktArgs dfA, DfLikeCktArgs dfB)
  -> Circuit (Df (Dom dfA) (OtpPayload dfA), Df (Dom dfB) (OtpPayload dfB)) (dfA, dfB)
tupToDfLikeOtp (argsA, argsB) = coerceCircuit $ tupCircuits (dfToDfLikeOtp argsA) (dfToDfLikeOtp argsB)

-- | 'Df.mapMaybe' but converted to be used for any 'DfLike'.
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

-- | 'Df.map' but converted to be used for any 'DfLike'.
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

-- | 'Df.filter' but converted to be used for any 'DfLike'.
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

-- | 'Df.zipWith' but converted to be used for any 'DfLike'.
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

-- | 'Df.fanout' but converted to be used for any 'DfLike'.
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

-- | Generalized fifo for DfLike.
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
