{-|
This module implements a type class 'DfLikeAlternative' which serves as a generalization
of the 'Protocols.Df.Df' protocol. Similar protocols can provide an instance
for it and subsequently expose all the functionality implemented in this module.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.DfLikeAlternative
  (
    -- * Typeclass and its associated types/fns
    DfLikeAlternative
  , Dom
  , BwdPayload
  , FwdPayload
  , DfLikeParam
  , toDfCircuit

    -- * Helper functions
  , fromDfCircuit
  , toDfCircuitHelper

    -- * Utilities to use DfLike one-way
  , dfToDfLikeInp
  , dfToDfLikeOtp
  , vecToDfLikeInp
  , vecToDfLikeOtp
  , tupToDfLikeInp
  , tupToDfLikeOtp

    -- * Df functions generalized to Dflike
  , const, void, pure
  , map, bimap
  , fst, snd
  , mapMaybe, catMaybes
  , filter
  , either
  , first, {-firstT,-} mapLeft
  , second, {-secondT,-} mapRight
  , zipWith, zip
  , partition
  , route
  , select
  , selectN
  , selectUntil
  , fanin
  , mfanin
  , fanout
  , bundleVec
  , unbundleVec
  , roundrobin
  , DfLike.CollectMode(..)
  , roundrobinCollect
  , registerFwd
  , registerBwd
  , fifo
  ) where


import qualified Prelude as P
import           Control.Arrow ((***))
import           Control.Monad (when)
import           Control.Monad.State (State, runState, get, put, gets, modify)
import           Clash.Prelude hiding
                   (map, fst, snd, zipWith, const, pure, filter, either, zip, select)
import qualified Data.Bifunctor as B
import           Data.Proxy (Proxy(..))

-- me
import qualified Protocols.DfLike as DfLike -- TODO
import           Protocols.Axi4.Common
import           Protocols.Axi4.ReadAddress
                   (Axi4ReadAddress, M2S_ReadAddress(..), S2M_ReadAddress(..))
import           Protocols.Axi4.ReadData
                   (Axi4ReadData, M2S_ReadData(..), S2M_ReadData(..))
import           Protocols.Axi4.WriteAddress
                   (Axi4WriteAddress, M2S_WriteAddress(..), S2M_WriteAddress(..))
import           Protocols.Axi4.WriteData
                   (Axi4WriteData, M2S_WriteData(..), S2M_WriteData(..))
import           Protocols.Axi4.WriteResponse
                   (Axi4WriteResponse, M2S_WriteResponse(..), S2M_WriteResponse(..))
import           Protocols.Df (Data(..), Df)
import qualified Protocols.Df as Df
import           Protocols.Internal


-- | Class for protocols that are "similar" to 'Df',
-- i.e. they can be converted into a 'Df' port using a 'Circuit' (see 'toDfCircuit').
-- This is for protocols that carry some "interesting" data,
-- as well as some "uninteresting" data (e.g. address, burst length).
-- The 'Circuit' should abstract away the complexities of each protocol,
-- so that they can be dealt with uniformly using 'Df'.
-- For pipelined protocols, which can carry both in the same cycle,
-- the 'Circuit' should pass along the interesting parts
-- but not the uninteresting parts.
--
-- Can take parameters, e.g. for addresses.
-- Defaults to being the right side of the circuit (@Fwd ~ Otp, Bwd ~ Inp@),
-- but this can be switched using 'Reverse' from 'Protocols.Internal'.
-- Supports both bwd (input/read) data and fwd (output/write) data.
class (Protocol df) => DfLikeAlternative df where
  -- | Domain that messages are being sent over.
  -- It should be true that @Fwd df ~ Signal dom [something]@
  -- and that @Bwd df ~ Signal dom [something]@.
  type Dom df :: Domain
  -- | Information being sent into the port, along @Bwd df@.
  -- This is the information being carried over the protocol,
  -- /not/ the messages being carried over the protocol,
  -- so it doesn't include auxiliary information like address or burst length.
  -- If no data is sent in this direction, set this to @()@.
  type BwdPayload df
  -- | Information being sent out from the port, along @Fwd df@.
  -- This is the information being carried over the protocol,
  -- /not/ the messages being carried over the protocol,
  -- so it doesn't include auxiliary information like address or burst length.
  -- If no data is sent in this direction, set this to @()@.
  type FwdPayload df
  -- | User-provided parameters for 'toDfCircuit'
  -- (e.g. address to respond to, so that different 'DfLikeAlternative'
  -- components can respond to different addresses).
  -- If you don't need to take params, set this to @()@.
  type DfLikeParam df
  -- | Circuit which converts Df into this protocol's messages.
  -- This should deal with all the complexities of your protocol
  -- such as addresses, bursts, pipelining, etc. so that
  -- a circuit connected to the 'Df' end doesn't have to worry about all that.
  -- There are two Df channels, one for fwd data and one for bwd data,
  -- so data can be sent both ways at once.
  -- This circuit is expected to follow all of the conventions of 'Df';
  -- for example, 'Df.Data' should stay the same
  -- between clock cycles unless acknowledged.
  toDfCircuit :: (HiddenClockResetEnable (Dom df))
    => (Proxy df, DfLikeParam df)
    -> Circuit (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df))) df

-- | 'toDfCircuit', but 'Df' is on the other side.
-- 'BwdPayload' remains the input data,
-- and 'FwdPayload' remains the output data.
-- All the functionality from 'toDfCircuit' is preserved.
fromDfCircuit
  :: (DfLikeAlternative df, HiddenClockResetEnable (Dom df))
  => (Proxy df, DfLikeParam df)
  -> Circuit (Reverse df)
             (Reverse (Df (Dom df) (FwdPayload df)), Df (Dom df) (BwdPayload df))
fromDfCircuit = coerceCircuit . reverseCircuit . toDfCircuit

-- | Helper function to make it easier to implement 'DfLikeAlternative'.
-- 'Ack's are automatically converted to/from 'Bool's,
-- and 'Df.Data's to/from 'Maybe'.
-- A default @otpMsg@ value is given for if reset is currently on.
-- The 'State' machine is run every clock cycle.
-- Parameters: initial state, default @otpMsg@, and 'State' machine function
toDfCircuitHelper ::
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
    -> Maybe (FwdPayload df)
    -> State state (otpMsg, Maybe (BwdPayload df), Bool)
     )
  -> Circuit (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df))) df
toDfCircuitHelper s0 blankOtp stateFn
  = Circuit
  $ (unbundle *** unbundle)
  . unbundle
  . hideReset cktFn
  . bundle
  . (bundle *** bundle)
 where
  cktFn reset inp = mealy transFn s0 ((,) <$> unsafeToHighPolarity reset <*> inp)
  transFn _ (True, _) = (s0, ((Ack False, NoData), blankOtp))
  transFn s (False, ((toOtp, Ack inpAck), inp)) = let
    ((otp, inputted, otpAck), s') = runState (stateFn inp inpAck (Df.dataToMaybe toOtp)) s
    in (s', ((Ack otpAck, Df.maybeToData inputted), otp))


-- DfLike classes for Df

instance (NFDataX dat) => DfLikeAlternative (Reverse (Df dom dat)) where
  type Dom         (Reverse (Df dom dat)) = dom
  type BwdPayload  (Reverse (Df dom dat)) = dat
  type FwdPayload  (Reverse (Df dom dat)) = ()
  type DfLikeParam (Reverse (Df dom dat)) = ()
  toDfCircuit _ = Circuit (\((_, b), c) -> ((P.pure (Ack False), c), b))

instance (NFDataX dat) => DfLikeAlternative (Df dom dat) where
  type Dom         (Df dom dat) = dom
  type BwdPayload  (Df dom dat) = ()
  type FwdPayload  (Df dom dat) = dat
  type DfLikeParam (Df dom dat) = ()
  toDfCircuit _ = Circuit (\((a, _), c) -> ((c, P.pure NoData), a))


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
  type BwdPayload
    (Reverse (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser),
     Reverse (Axi4WriteData dom 'KeepStrobe wdBytes wdUser),
     Axi4WriteResponse dom 'KeepResponse iw wrUser)
    = Vec wdBytes (Maybe (BitVector 8))
  type FwdPayload
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

  toDfCircuit (_, param) = toDfCircuitHelper s0 blankOtp (stateFn param) where
    s0 = (Nothing, S2M_NoWriteResponse)

    blankOtp = ( S2M_WriteAddress{_awready = False}
               , S2M_WriteData{_wready = False}
               , S2M_NoWriteResponse)

    stateFn (dataAddr,wrUser) (wAddrVal, wDataVal, wRespAck) ack _ = do
      wAddrAck <- processWAddr wAddrVal
      (wDataAck, inpItem) <- processWData wDataVal
      wRespVal <- gets P.snd
      processWRespAck
      P.pure ((wAddrAck, wDataAck, wRespVal), inpItem, False)
      where

      processWAddr M2S_NoWriteAddress = P.pure (S2M_WriteAddress{_awready = False})
      processWAddr M2S_WriteAddress{ _awburst, _awaddr, _awid }
        | _awburst /= BmFixed = P.pure (S2M_WriteAddress{_awready = True})
        | otherwise = do
           (_,b) <- get
           put (if _awaddr == dataAddr then Just _awid else Nothing, b)
           P.pure (S2M_WriteAddress{_awready = True})

      processWData M2S_NoWriteData = P.pure (S2M_WriteData{_wready = False}, Nothing)
      processWData M2S_WriteData{_wlast, _wdata} = do
        (shouldRead,respS2M) <- get
        -- we only want to output _wready = false
        --   if we're the recpient of the writes AND ack is false
        -- we only want to return data if we're the recpient of the writes
        -- we only want to output on writeresponse
        --   if we're the recipient of the writes AND ack is true
        case (shouldRead, ack) of
          (Nothing, _) -> P.pure (S2M_WriteData{_wready = True}, Nothing)
          (Just _, False) -> P.pure (S2M_WriteData{_wready = False}, Just _wdata)
          (Just sr, True) -> do
            put (Nothing,
                 if _wlast
                 then S2M_WriteResponse {_bid = sr, _bresp = ROkay, _buser = wrUser }
                 else respS2M)
            P.pure (S2M_WriteData{_wready = True}, Just _wdata)

      processWRespAck = when (_bready wRespAck) $
                        modify (\(a, _) -> (a, S2M_NoWriteResponse))

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
  type BwdPayload
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
     = ()
  type FwdPayload
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
     = dat
  type DfLikeParam
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser dat)
    = (BitVector (Width aw), rdUser)
    -- data address, user responses

  toDfCircuit (_, param) = toDfCircuitHelper s0 blankOtp (stateFn param) where
    s0 =
      (0,
       errorX "DfLike for Axi4: No initial value for read id",
       S2M_NoReadData)

    blankOtp = (S2M_ReadAddress { _arready = False }, S2M_NoReadData)

    stateFn (dataAddr,usr) (addrVal, dataAck) _ otpItem = do
      addrAck <- processAddr addrVal
      (dataVal,sentData) <- sendData
      processDataAck dataAck
      P.pure ((addrAck,dataVal),Nothing,sentData)
      where
        processAddr M2S_NoReadAddress = P.pure (S2M_ReadAddress { _arready = False })
        processAddr M2S_ReadAddress{_arburst,_araddr,_arlen,_arid}
          | _arburst /= BmFixed = P.pure (S2M_ReadAddress{ _arready = True })
          | otherwise = do
              (burstLenLeft,_,c) <- get
              when (burstLenLeft == 0 && (_araddr == dataAddr)) $ put (_arlen, _arid, c)
              P.pure (S2M_ReadAddress{ _arready = burstLenLeft == 0 })

        sendData = do
          (burstLenLeft,readId,currOtp) <- get
          sentData <- case (currOtp, burstLenLeft == 0, otpItem) of
            (S2M_NoReadData, False, Just oi) -> do
              put (burstLenLeft-1, readId,
                S2M_ReadData
                  { _rid = readId
                  , _rdata = oi
                  , _rresp = ROkay
                  , _rlast = burstLenLeft == 1
                  , _ruser = usr })
              P.pure True
            _ -> P.pure False
          (_,_,currOtp') <- get
          P.pure (currOtp', sentData)

        processDataAck M2S_ReadData{_rready} = when _rready $ do
          (a,b,_) <- get
          put (a,b,S2M_NoReadData)


-- | Convert 'DfLikeAlternative' into a /one-way/ 'Df' port,
-- at the data input end
dfToDfLikeInp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => (Proxy df, DfLikeParam df)
  -> Circuit (Reverse df) (Df (Dom df) (BwdPayload df))
dfToDfLikeInp = mapCircuit id id P.snd (P.pure NoData, ) . fromDfCircuit

-- | Convert 'DfLikeAlternative' into a /one-way/ 'Df' port,
-- at the data output end
dfToDfLikeOtp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => (Proxy df, DfLikeParam df)
  -> Circuit (Df (Dom df) (FwdPayload df)) df
dfToDfLikeOtp = mapCircuit (, P.pure (Ack False)) P.fst id id . toDfCircuit

-- | Convert a vec of 'DfLikeAlternative's into a vec of /one-way/ 'Df' ports,
-- at the data input end
vecToDfLikeInp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => KnownNat n
  => Vec n (Proxy df, DfLikeParam df)
  -> Circuit (Vec n (Reverse df)) (Vec n (Df (Dom df) (BwdPayload df)))
vecToDfLikeInp = vecCircuits . fmap dfToDfLikeInp

-- | Convert a vec of 'DfLikeAlternative's into a vec of /one-way/ 'Df' ports,
-- at the data output end
vecToDfLikeOtp
  :: DfLikeAlternative df
  => HiddenClockResetEnable (Dom df)
  => KnownNat n
  => Vec n (Proxy df, DfLikeParam df)
  -> Circuit (Vec n (Df (Dom df) (FwdPayload df))) (Vec n df)
vecToDfLikeOtp = vecCircuits . fmap dfToDfLikeOtp

-- | Convert a pair of (possibly different from each other) 'DfLikeAlternative's
-- into a pair of /one-way/ 'Df' ports, at the data input end
tupToDfLikeInp
  :: DfLikeAlternative dfA
  => DfLikeAlternative dfB
  => Dom dfA ~ Dom dfB
  => HiddenClockResetEnable (Dom dfA)
  => ((Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB))
  -> Circuit (Reverse (dfA, dfB))
             (Df (Dom dfA) (BwdPayload dfA), Df (Dom dfB) (BwdPayload dfB))
tupToDfLikeInp (argsA, argsB) = coerceCircuit
                              $ tupCircuits (dfToDfLikeInp argsA) (dfToDfLikeInp argsB)

-- | Convert a pair of (possibly different from each other) 'DfLikeAlternative's
-- into a pair of /one-way/ 'Df' ports, at the data output end
tupToDfLikeOtp
  :: DfLikeAlternative dfA
  => DfLikeAlternative dfB
  => Dom dfA ~ Dom dfB
  => HiddenClockResetEnable (Dom dfA)
  => ((Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB))
  -> Circuit (Df (Dom dfA) (FwdPayload dfA), Df (Dom dfB) (FwdPayload dfB))
             (dfA, dfB)
tupToDfLikeOtp (argsA, argsB) = coerceCircuit
                              $ tupCircuits (dfToDfLikeOtp argsA) (dfToDfLikeOtp argsB)


-- | Like 'P.map'
map ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (BwdPayload dfA -> FwdPayload dfB) ->
  Circuit (Reverse dfA) dfB
map dfA dfB f
  =  dfToDfLikeInp dfA
  |> Df.map f
  |> dfToDfLikeOtp dfB

-- | Like 'P.fst'
fst ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ (a, b)
  , FwdPayload dfB ~ a ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
fst dfA dfB
  =  dfToDfLikeInp dfA
  |> Df.fst
  |> dfToDfLikeOtp dfB

-- | Like 'P.fst'
snd ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ (a, b)
  , FwdPayload dfB ~ b ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
snd dfA dfB
  =  dfToDfLikeInp dfA
  |> Df.snd
  |> dfToDfLikeOtp dfB

-- | Like 'Data.Bifunctor.bimap'
bimap ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , BwdPayload dfA ~ p a c
  , FwdPayload dfB ~ p b d ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (a -> b) ->
  (c -> d) ->
  Circuit (Reverse dfA) dfB
bimap dfA dfB f g
  =  dfToDfLikeInp dfA
  |> Df.bimap f g
  |> dfToDfLikeOtp dfB

-- | Like 'Data.Bifunctor.first'
first ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , BwdPayload dfA ~ p a c
  , FwdPayload dfB ~ p b c ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (a -> b) ->
  Circuit (Reverse dfA) dfB
first dfA dfB f
  =  dfToDfLikeInp dfA
  |> Df.first f
  |> dfToDfLikeOtp dfB

-- | Like 'Data.Bifunctor.second'
second ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , BwdPayload dfA ~ p a b
  , FwdPayload dfB ~ p a c ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (b -> c) ->
  Circuit (Reverse dfA) dfB
second dfA dfB f
  =  dfToDfLikeInp dfA
  |> Df.second f
  |> dfToDfLikeOtp dfB

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  FwdPayload dfB ->
  Circuit (Reverse dfA) dfB
const dfA dfB b
  =  dfToDfLikeInp dfA
  |> Df.const b
  |> dfToDfLikeOtp dfB

-- | Drive a constant value composed of /a/.
pure ::
  ( DfLikeAlternative df
  , HiddenClockResetEnable (Dom df) ) =>
  (Proxy df, DfLikeParam df) ->
  FwdPayload df ->
  Circuit () df
pure df a
  =  Df.pure a
  |> dfToDfLikeOtp df

-- | Ignore incoming data
void ::
  ( DfLikeAlternative df
  , HiddenClockResetEnable (Dom df) ) =>
  (Proxy df, DfLikeParam df) ->
  Circuit (Reverse df) ()
void df = dfToDfLikeInp df
  |> Df.void

-- | Like 'Data.Maybe.catMaybes'
catMaybes ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ Maybe (FwdPayload dfB) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
catMaybes dfA dfB
  =  dfToDfLikeInp dfA
  |> Df.catMaybes
  |> dfToDfLikeOtp dfB

-- | Like 'Data.Maybe.mapMaybe'
mapMaybe ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , NFDataX (FwdPayload dfB) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (BwdPayload dfA -> Maybe (FwdPayload dfB)) ->
  Circuit (Reverse dfA) dfB
mapMaybe dfA dfB f
  =  dfToDfLikeInp dfA
  |> Df.mapMaybe f
  |> dfToDfLikeOtp dfB

-- | Like 'P.filter'
filter ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (BwdPayload dfA -> Bool) ->
  Circuit (Reverse dfA) dfB
filter dfA dfB f
  =  dfToDfLikeInp dfA
  |> Df.filter f
  |> dfToDfLikeOtp dfB

-- | Like 'Data.Either.Combinators.mapLeft'
mapLeft ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ Either a c
  , FwdPayload dfB ~ Either b c ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (a -> b) ->
  Circuit (Reverse dfA) dfB
mapLeft dfA dfB f
  =  dfToDfLikeInp dfA
  |> Df.mapLeft f
  |> dfToDfLikeOtp dfB

-- | Like 'Data.Either.Combinators.mapRight'
mapRight ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ Either a b
  , FwdPayload dfB ~ Either a c ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (b -> c) ->
  Circuit (Reverse dfA) dfB
mapRight dfA dfB f
  =  dfToDfLikeInp dfA
  |> Df.mapRight f
  |> dfToDfLikeOtp dfB

-- | Like 'Data.Either.either'
either ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ Either a b ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (a -> FwdPayload dfB) ->
  (b -> FwdPayload dfB) ->
  Circuit (Reverse dfA) dfB
either dfA dfB f g
  =  dfToDfLikeInp dfA
  |> Df.either f g
  |> dfToDfLikeOtp dfB

-- | Like 'P.zipWith'. Any data not in /Payload/ is copied from stream A.
zipWith ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , DfLikeAlternative dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA) ) =>
  ((Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB)) ->
  (Proxy dfC, DfLikeParam dfC) ->
  (BwdPayload dfA -> BwdPayload dfB -> FwdPayload dfC) ->
  Circuit (Reverse (dfA, dfB)) dfC
zipWith dfAB dfC f
  =  tupToDfLikeInp dfAB
  |> Df.zipWith f
  |> dfToDfLikeOtp dfC

-- | Like 'P.zip'
zip ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , DfLikeAlternative dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfC ~ (BwdPayload dfA, BwdPayload dfB) ) =>
  ((Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB)) ->
  (Proxy dfC, DfLikeParam dfC) ->
  Circuit (Reverse (dfA, dfB)) dfC
zip dfAB dfC
  =  tupToDfLikeInp dfAB
  |> Df.zip
  |> dfToDfLikeOtp dfC

-- | Like 'P.partition'
partition ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , DfLikeAlternative dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , BwdPayload dfA ~ FwdPayload dfC ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  ((Proxy dfB, DfLikeParam dfB), (Proxy dfC, DfLikeParam dfC)) ->
  (BwdPayload dfA -> Bool) ->
  Circuit (Reverse dfA) (dfB, dfC)
partition dfA dfBC f
  =  dfToDfLikeInp dfA
  |> Df.partition f
  |> tupToDfLikeOtp dfBC

-- | Route a DfLike stream to another corresponding to the index
route ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ (Index n, FwdPayload dfB)
  , KnownNat n ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  Vec n (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) (Vec n dfB)
route dfA dfB
  =  dfToDfLikeInp dfA
  |> Df.route
  |> vecToDfLikeOtp dfB

-- | Select data from the channel indicated by the DfLike stream carrying
-- @Index n@.
select ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , DfLikeAlternative dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfC
  , BwdPayload dfB ~ Index n
  , KnownNat n ) =>
  (Vec n (Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB)) ->
  (Proxy dfC, DfLikeParam dfC) ->
  Circuit (Vec n (Reverse dfA), Reverse dfB) dfC
select (dfA, dfB) dfC
  =  tupCircuits (vecToDfLikeInp dfA) (dfToDfLikeInp dfB)
  |> Df.select
  |> dfToDfLikeOtp dfC

-- | Select /selectN/ samples from channel /n/.
selectN ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , DfLikeAlternative dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfC
  , BwdPayload dfB ~ (Index n, Index selectN)
  , KnownNat n
  , KnownNat selectN ) =>
  (Vec n (Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB)) ->
  (Proxy dfC, DfLikeParam dfC) ->
  Circuit (Vec n (Reverse dfA), Reverse dfB) dfC
selectN (dfA, dfB) dfC
  =  tupCircuits (vecToDfLikeInp dfA) (dfToDfLikeInp dfB)
  |> Df.selectN
  |> dfToDfLikeOtp dfC

-- | Selects samples from channel /n/ until the predicate holds. The cycle in
-- which the predicate turns true is included.
selectUntil ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , DfLikeAlternative dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfC
  , BwdPayload dfB ~ Index n
  , KnownNat n ) =>
  (Vec n (Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB)) ->
  (Proxy dfC, DfLikeParam dfC) ->
  (BwdPayload dfA -> Bool) ->
  Circuit (Vec n (Reverse dfA), Reverse dfB) dfC
selectUntil (dfA, dfB) dfC f
  =  tupCircuits (vecToDfLikeInp dfA) (dfToDfLikeInp dfB)
  |> Df.selectUntil f
  |> dfToDfLikeOtp dfC

-- | Copy data of a single DfLike stream to multiple. LHS will only receive
-- an acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA)
  , KnownNat numB
  , numB ~ (decNumB + 1) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  Vec numB (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) (Vec numB dfB)
fanout dfA dfB
  =  dfToDfLikeInp dfA
  |> Df.fanout
  |> vecToDfLikeOtp dfB

-- | Merge data of multiple streams using a user supplied function
fanin ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA)
  , KnownNat numA
  , numA ~ (decNumA + 1) ) =>
  Vec numA (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (BwdPayload dfA -> BwdPayload dfA -> BwdPayload dfA) ->
  Circuit (Vec numA (Reverse dfA)) dfB
fanin dfA dfB f
  =  vecToDfLikeInp dfA
  |> Df.fanin f
  |> dfToDfLikeOtp dfB

-- | Merge data of multiple streams using Monoid's '<>'.
mfanin ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA)
  , Monoid (BwdPayload dfA)
  , KnownNat numA
  , numA ~ (decNumA + 1) ) =>
  Vec numA (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Vec numA (Reverse dfA)) dfB
mfanin dfA dfB
  =  vecToDfLikeInp dfA
  |> Df.mfanin
  |> dfToDfLikeOtp dfB

-- | Bundle a vector of DfLike streams into one.
bundleVec ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , Vec n (BwdPayload dfA) ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1) ) =>
  Vec n (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Vec n (Reverse dfA)) dfB
bundleVec dfA dfB
  =  vecToDfLikeInp dfA
  |> Df.bundleVec
  |> dfToDfLikeOtp dfB

-- | Split up a DfLike stream of a vector into multiple independent DfLike streams.
unbundleVec ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ Vec n (FwdPayload dfB)
  , NFDataX (FwdPayload dfB)
  , KnownNat n
  , n ~ (decN + 1) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  Vec n (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) (Vec n dfB)
unbundleVec dfA dfB
  =  dfToDfLikeInp dfA
  |> Df.unbundleVec
  |> vecToDfLikeOtp dfB

-- | Distribute data across multiple components on the RHS. Useful if you want
-- to parallelize a workload across multiple (slow) workers. For optimal
-- throughput, you should make sure workers can accept data every /n/ cycles.
roundrobin ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  Vec n (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) (Vec n dfB)
roundrobin dfA dfB
  =  dfToDfLikeInp dfA
  |> Df.roundrobin
  |> vecToDfLikeOtp dfB

-- | Opposite of 'roundrobin'. Useful to collect data from workers that only
-- produce a result with an interval of /n/ cycles.
roundrobinCollect ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1) ) =>
  Vec n (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  DfLike.CollectMode ->
  Circuit (Vec n (Reverse dfA)) dfB
roundrobinCollect dfA dfB mode
  =  vecToDfLikeInp dfA
  |> Df.roundrobinCollect mode
  |> dfToDfLikeOtp dfB

-- | Place register on /forward/ part of a circuit.
registerFwd ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
registerFwd dfA dfB
  =  dfToDfLikeInp dfA
  |> Df.registerFwd
  |> dfToDfLikeOtp dfB

-- | Place register on /backward/ part of a circuit. This is implemented using a
-- in-logic two-element shift register.
registerBwd ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
registerBwd dfA dfB
  =  dfToDfLikeInp dfA
  |> Df.registerBwd
  |> dfToDfLikeOtp dfB

-- | A fifo buffer with user-provided depth.
-- Uses blockram to store data
fifo ::
  ( DfLikeAlternative dfA
  , DfLikeAlternative dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , KnownNat depth
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  SNat depth ->
  Circuit (Reverse dfA) dfB
fifo argsA argsB fifoDepth
  =  dfToDfLikeInp argsA
  |> Df.fifo fifoDepth
  |> dfToDfLikeOtp argsB where
