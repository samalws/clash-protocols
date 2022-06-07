{-# LANGUAGE FlexibleContexts #-}

module Protocols.Axi4.Fifo (axi4Source, axi4Sink) where

import Clash.Prelude hiding (pure, not, (||), (&&))
import Control.Monad.State
import Prelude hiding (replicate, repeat, foldl)
import Protocols.Axi4.Common hiding (Data)
import Protocols.Axi4.ReadAddress
import Protocols.Axi4.ReadData hiding (pure)
import Protocols.Axi4.WriteAddress
import Protocols.Axi4.WriteData
import Protocols.Df hiding (pure)
import Protocols.Internal


-- move all the Just's to the right, and pad with zeros on the left
accountForStrobe :: (KnownNat n, BitPack a) => Vec n (Maybe a) -> Vec n a
accountForStrobe = foldl f s0 where
  s0 = repeat (bitCoerce $ repeat False) -- Vec n of all zeros
  f v (Just a) = v <<+ a
  f v Nothing = v

-- we have Index (depth+1) but we only want to access blockram up to depth-1
incIdxLooping :: (KnownNat n) => Index n -> Index n
incIdxLooping idx = if idx >= (maxBound-1) then 0 else idx+1

-- | Axi4 to Df source
-- * Writing to the given address pushes an item onto the fifo
-- * Writes are only acknowledged once there is free space in the fifo
-- * Reading from statusAddress returns how much free space is left in the buffer
-- * Doesn't accept a new read request before the old one is done
-- * Reading/writing to any other address is acknowledged, but the fifo element is not popped and nothing is replied
-- * Only responds to BmFixed burst mode, other modes are acknowledged but nothing happens
-- * Respects burst length, will respond with the correct number of replies
-- * Can use the user channel on ReadData, replying with a fixed user-provided reply
-- * Uses blockram to store data
axi4Source ::
  (1 + n) ~ depth =>
  (depth+1) ~ (1+depth) =>
  HiddenClockResetEnable dom =>
  KnownNat depth =>
  KnownNat (Width aw) =>
  KnownNat (Width w_iw) =>
  KnownNat (Width r_iw) =>
  KnownNat (BitSize dat) =>
  KnownNat wdBytes =>
  (BitSize dat) <= (wdBytes * 8) => -- not necessary for it to typecheck but necessary for it to work correctly
  NFDataX dat =>
  BitPack dat =>
  NFDataX rdUser =>
  -- | Address to respond to
  BitVector (Width aw) ->
  -- | Address to ask for status (how much fixed space is left in the buffer)
  Maybe (BitVector (Width aw)) ->
  -- | Depth of the fifo
  SNat depth ->
  -- | Fixed user response for fifo status
  rdUser ->
  -- |
  Circuit
    (Axi4WriteAddress dom 'KeepBurst waKeepSize w_lw w_iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser,
     Axi4WriteData dom 'KeepStrobe wdBytes wdUser,
     Axi4ReadAddress dom 'KeepBurst 'NoSize r_lw r_iw aw raKeepRegion 'KeepBurstLength raKeepLock raKeepCache raKeepPermissions raKeepQos raUser,
     Reverse (Axi4ReadData dom 'KeepResponse r_iw rdUser (Index (depth+1))))
    (Df dom dat)
axi4Source respondAddress statusAddress fifoDepth rdUser = Circuit (hideReset circuitFunction) where

  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset ((inpA, inpB, inpC, inpD), inpE) = ((otpA, otpB, otpC, otpD), otpE) where
    brRead = readNew (blockRam (replicate fifoDepth $ errorX "axi4Source: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpA, otpB, otpC, otpD, otpE) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB, inpC, inpD, inpE)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  -- initial state
  -- (last write address given was respondAddress, number of times left to send status output, status output read id, status output, data output, amount of space left, next place to read from, next place to write to)
  s0 = let numFree = maxBound
           nextRW = numFree * 0
           -- extremely janky, but necessary for type checking;
           --   forces nextRead and nextWrite to have the same type (that is, Index (depth+1)) as numFree
           --   (since ghc wouldn't be able to tell their type otherwise)
           -- nextRW is initialized to 0, although it could take on any value and the buffer would still work
       in
           (False, 0, 0, S2M_NoReadData, NoData, numFree, nextRW, nextRW)

  -- when reset is on, output blank/default and don't change any state
  fullStateMachine (_,True,_,_,_,_,_) = pure (0, Nothing, S2M_WriteAddress{_awready = False}, S2M_WriteData{_wready = False}, S2M_ReadAddress{_arready = False}, S2M_NoReadData, NoData)
  fullStateMachine (brRead,False,addrM2S,dataM2S,statusAddrM2S,statusAckM2S,Ack ack) = do
    sendData brRead
    -- fix some outputs before they get changed for next time later on
    (_, _, _, _, dataOut, _, brReadAddr, _) <- get
    ackA <- processWriteAddress addrM2S
    (brWrite, ackB) <- pushInpData dataM2S
    ackC <- processReadAddress statusAddrM2S
    sendStatus
    (_, _, _, statusOut, _, _, _, _) <- get
    clearStatus statusAckM2S
    clearData ack
    pure (brReadAddr, brWrite, ackA, ackB, ackC, statusOut, dataOut)

  -- decide which data to send, given block ram read value
  sendData brRead = do
    (a,b,c,d,currOtp,numFree,nextRead,h) <- get
    case (currOtp,numFree == maxBound) of
      (NoData, False) -> put (a, b, c, d, Data brRead, numFree+1, incIdxLooping nextRead, h) -- pop
      _ -> pure ()

  -- log whether we're the target of the next data write; return ack
  processWriteAddress M2S_NoWriteAddress = pure (S2M_WriteAddress{_awready = False})
  processWriteAddress addrM2S | _awburst addrM2S /= BmFixed = pure (S2M_WriteAddress{_awready = True})
  processWriteAddress addrM2S = do
    (_,b,c,d,e,f,g,h) <- get
    put (_awaddr addrM2S == respondAddress, b, c, d, e, f, g, h)
    pure (S2M_WriteAddress{_awready = True})

  -- push input data onto the stack
  pushInpData M2S_NoWriteData = pure (Nothing, S2M_WriteData{_wready = False})
  pushInpData inpDat = do
    (shouldRead,b,c,d,e,numFree,g,nextWrite) <- get
    -- we only want to output _wready = false if we're the recpient of the writes AND our buffer is full
    -- we only want to push if we're the recpient of the writes AND our buffer has space available
    if (not shouldRead || numFree == 0) then pure (Nothing, S2M_WriteData{_wready = not shouldRead}) else do
      put (False,b,c,d,e,numFree-1,g,incIdxLooping nextWrite)
      pure (Just (nextWrite, unpack $ resize $ pack $ accountForStrobe $ _wdata inpDat), S2M_WriteData{_wready = True})

  -- if state is being asked for, log the burst length requested
  processReadAddress M2S_NoReadAddress = pure (S2M_ReadAddress { _arready = False })
  processReadAddress addrM2S | _arburst addrM2S /= BmFixed = pure (S2M_ReadAddress{ _arready = True })
  processReadAddress addrM2S = do
    (a,burstLenLeft,_,d,e,f,g,h) <- get
    when (Just (_araddr addrM2S) == statusAddress && burstLenLeft == 0) $ put (a,_arlen addrM2S,_arid addrM2S,d,e,f,g,h)
    pure (S2M_ReadAddress { _arready = burstLenLeft /= 0 })

  -- write down what status message we're sending (so it doesn't change between clock cycles)
  sendStatus = do
    (a,burstLenLeft,rid,statusOut,e,numFree,g,h) <- get
    -- case (our status is requested, we aren't already giving it) of
    case (burstLenLeft == 0, statusOut) of
      (False, S2M_NoReadData) -> put (a,burstLenLeft-1,rid,S2M_ReadData { _rdata = numFree, _rid = rid, _rresp = ROkay, _rlast = burstLenLeft == 1, _ruser = rdUser },e,numFree,g,h)
      _ -> pure ()

  -- when we're acknowledged, clear our output for next clock cycle (it stays the same this cycle)
  clearStatus ack = when (_rready ack) $ do
    (a,b,c,_,e,f,g,h) <- get
    put (a, b, c, S2M_NoReadData, e, f, g, h)

  clearData ack = when ack $ do
    (a,b,c,d,_,f,g,h) <- get
    put (a, b, c, d, NoData, f, g, h)

-- | Axi4 to Df sink
-- * Reading from the given address pops an item from the fifo and returns Right
-- * Reading from statusAddress returns Left, and how much free space is left in the buffer
-- * Doesn't accept a new read request before the old one is done
-- * Reading from any other address is acknowledged, but the fifo element is not popped and nothing is replied
-- * When reading from an empty fifo, will respond with error and Left 0
-- * Only responds to BmFixed burst mode, other modes are acknowledged but nothing happens
-- * Respects burst length, will respond with the correct number of replies
-- * Can use the user channel on ReadData,
-- *   replying with one of three user-provided replies,
-- *   depending on what message is being sent (fifo item, fifo status, or error)
-- * Uses blockram to store data
axi4Sink ::
  (1 + n) ~ depth =>
  HiddenClockResetEnable dom =>
  KnownNat depth =>
  KnownNat (BitSize dat) =>
  KnownNat (Width aw) =>
  KnownNat (Width iw) =>
  NFDataX dat =>
  NFDataX rdUser =>
  -- | Address to respond to
  BitVector (Width aw) ->
  -- | Address to ask for status (how much space is left in buffer)
  Maybe (BitVector (Width aw)) ->
  -- | Depth of the fifo
  SNat depth ->
  -- | User responses for fifo item, fifo status, and error
  rdUser -> rdUser -> rdUser ->
  -- |
  Circuit
    (Df dom dat)
    (Reverse (Axi4ReadAddress dom 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData),
     Axi4ReadData dom 'KeepResponse iw rdUser (Either (Index (depth+1)) dat))
axi4Sink respondAddress statusAddress fifoDepth rdUserRead rdUserStatus rdUserErr = Circuit (hideReset circuitFunction) where

  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset (inpA, (inpB, inpC)) = (otpA, (otpB, otpC)) where
    brRead = readNew (blockRam (replicate fifoDepth $ errorX "axi4Sink: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpA, otpB, otpC) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB, inpC)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  -- initial state
  -- (amount left in read burst, read ID, axi read data (amt left or data), amount of space left, next place to read from, next place to write to, fifo)
  s0 = let numFree = maxBound
           nextRW = numFree * 0
           -- extremely janky, but necessary for type checking;
           --   forces nextRead and nextWrite to have the same type (that is, Index (depth+1)) as numFree
           --   (since ghc wouldn't be able to tell their type otherwise)
           -- nextRW is initialized to 0, although it could take on any value and the buffer would still work
       in
           (Left 0, 0, S2M_NoReadData, numFree, nextRW, nextRW)

  -- when reset is on, output blank/default and don't change any state
  fullStateMachine (_,True,_,_,_) = pure (0, Nothing, Ack False, S2M_ReadAddress{_arready = False}, S2M_NoReadData{})
  fullStateMachine (brRead,False,inpDat,addrM2S,dataM2S) = do
    ackB <- processAddr addrM2S
    sendData brRead
    -- fix some outputs before they get changed for next time later on
    (_, _, dataS2M, _, brReadAddr, _) <- get
    clearData dataM2S
    (brWrite, ackA) <- pushInpData inpDat
    pure (brReadAddr, brWrite, ackA, ackB, dataS2M)

  -- log whether our status/fifo elements are being asked for; return ack
  processAddr M2S_NoReadAddress = pure (S2M_ReadAddress{_arready = False})
  processAddr addrM2S | _arburst addrM2S /= BmFixed = pure (S2M_ReadAddress{_arready = True})
  processAddr addrM2S = do
    (burstLenLeft,_,c,d,e,f) <- get
    let canAccept = burstLenLeft == Left 0 || burstLenLeft == Right 0
    when canAccept $ put (pureProcessAddr (_araddr addrM2S) (_arlen addrM2S), _arid addrM2S, c, d, e, f)
    pure (S2M_ReadAddress{_arready = canAccept})

  pureProcessAddr addr n | addr == respondAddress = Right n
  pureProcessAddr addr n | Just addr == statusAddress = Left n
  pureProcessAddr _ _ = Left 0

  -- decide which data to send, given block ram read value
  sendData brRead = do
    (burstLenLeft,rid,currOtp,numFree,nextRead,f) <- get
    -- case (do we want status or fifo element?, are we outputting anything currently?, do we have anything in our buffer?) of
    case (burstLenLeft, currOtp, numFree == maxBound) of
      (Left 0, _, _) -> pure ()
      (Right 0, _, _) -> pure ()
      (Left n, S2M_NoReadData, _) ->
        put (Left (n-1),
             rid,
             S2M_ReadData { _rid = rid, _rdata = Left numFree, _rresp = ROkay, _rlast = n == 1, _ruser = rdUserStatus },
             numFree,
             nextRead,
             f)
      (Right n, S2M_NoReadData, False) ->
        put (Right (n-1),
             rid,
             S2M_ReadData { _rid = rid, _rdata = Right brRead, _rresp = ROkay, _rlast = n == 1, _ruser = rdUserRead },
             numFree+1,
             incIdxLooping nextRead,
             f)
      (Right n, S2M_NoReadData, True) ->
        put (Right (n-1),
             rid,
             S2M_ReadData { _rid = rid, _rdata = Left numFree, _rresp = RSlaveError, _rlast = n == 1, _ruser = rdUserErr },
             numFree,
             nextRead,
             f)
      _ -> pure ()

  -- when we're acknowledged, clear our output for next clock cycle (it stays the same this cycle)
  clearData dataM2S = when (_rready dataM2S) $ do
    (a,b,_,d,e,f) <- get
    put (a,b,S2M_NoReadData,d,e,f)

  -- push input data onto the stack
  pushInpData NoData = pure (Nothing, Ack False)
  pushInpData (Data inpDat) = do
    (a,b,c,numFree,e,nextWrite) <- get
    if numFree == 0 then pure (Nothing, Ack False) else do
      put (a,b,c,numFree-1,e,incIdxLooping nextWrite)
      pure (Just (nextWrite, inpDat), Ack True)
