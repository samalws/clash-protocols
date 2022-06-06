{-# LANGUAGE FlexibleContexts #-}

module Protocols.Axi4.Fifo where

import Clash.Prelude hiding (pure, not, (||), (&&))
import Control.Monad.State
import Prelude hiding (replicate)
import Protocols.Axi4.Common hiding (Data)
import Protocols.Axi4.ReadAddress
import Protocols.Axi4.ReadData hiding (pure)
import Protocols.Axi4.WriteAddress
import Protocols.Axi4.WriteData
import Protocols.Df hiding (pure)
import Protocols.Internal


-- | Axi4 to Df source
-- * Writing to the given address pushes an item onto the fifo
-- * Writes are only acknowledged once there is free space in the fifo
-- * Reading from statusAddress returns how much free space is left in the buffer
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
  KnownNat (BitSize dat) =>
  KnownNat (Width aw) =>
  KnownNat (Width iw) =>
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
    (Axi4WriteAddress dom 'KeepBurst waKeepSize lw iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser,
     Axi4WriteData dom 'NoStrobe ((BitSize dat + 7) `Div` 8) wdUser,
     Axi4ReadAddress dom 'KeepBurst raKeepSize lw iw aw raKeepRegion 'KeepBurstLength raKeepLock raKeepCache raKeepPermissions raKeepQos raUser,
     Reverse (Axi4ReadData dom 'KeepResponse iw rdUser (Index (depth+1))))
    (Df dom dat)
-- TODO look at strobe
-- TODO for keepSize, verify the size
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
  -- (last write address given was respondAddress, number of times left to send status output, status output, data output, amount of space left, next place to read from, next place to write to)
  s0 = let numFree = maxBound
           nextRW = numFree * 0
           -- extremely janky, but necessary for type checking;
           --   forces nextRead and nextWrite to have the same type (that is, Index (depth+1)) as numFree
           --   (since ghc wouldn't be able to tell their type otherwise)
           -- nextRW is initialized to 0, although it could take on any value and the buffer would still work
       in
           (False, 0, S2M_NoReadData, NoData, numFree, nextRW, nextRW)

  -- when reset is on, output blank/default and don't change any state
  fullStateMachine (_,True,_,_,_,_,_) = pure (0, Nothing, S2M_WriteAddress{_awready = False}, S2M_WriteData{_wready = False}, S2M_ReadAddress{_arready = False}, S2M_NoReadData, NoData)
  fullStateMachine (brRead,False,addrM2S,dataM2S,statusAddrM2S,statusAckM2S,Ack ack) = do
    sendData brRead
    -- fix some outputs before they get changed for next time later on
    (_, _, _, dataOut, _, brReadAddr, _) <- get
    ackA <- processWriteAddress addrM2S
    (brWrite, ackB) <- pushInpData dataM2S
    ackC <- processReadAddress statusAddrM2S
    sendStatus
    (_, _, statusOut, _, _, _, _) <- get
    clearStatus statusAckM2S
    clearData ack
    pure (brReadAddr, brWrite, ackA, ackB, ackC, statusOut, dataOut)

  -- decide which data to send, given block ram read value
  sendData brRead = do
    (a,b,c,currOtp,numFree,nextRead,g) <- get
    case (currOtp,numFree == maxBound) of
      (NoData, False) -> put (a, b, c, Data brRead, numFree+1, incIdxLooping nextRead, g) -- pop
      _ -> pure ()

  -- log whether we're the target of the next data write; return ack
  processWriteAddress M2S_NoWriteAddress = pure (S2M_WriteAddress{_awready = False})
  processWriteAddress addrM2S | _awburst addrM2S /= BmFixed = pure (S2M_WriteAddress{_awready = True})
  processWriteAddress addrM2S = do
    (_,b,c,d,e,f,g) <- get
    put (_awaddr addrM2S == respondAddress, b, c, d, e, f, g)
    pure (S2M_WriteAddress{_awready = True})

  -- push input data onto the stack
  pushInpData M2S_NoWriteData = pure (Nothing, S2M_WriteData{_wready = False})
  pushInpData inpDat = do
    (shouldRead,b,c,d,numFree,f,nextWrite) <- get
    -- we only want to output _wready = false if we're the recpient of the writes AND our buffer is full
    -- we only want to push if we're the recpient of the writes AND our buffer has space available
    if (not shouldRead || numFree == 0) then pure (Nothing, S2M_WriteData{_wready = not shouldRead}) else do
      put (False,b,c,d,numFree-1,f,incIdxLooping nextWrite)
      pure (Just (nextWrite, unpack $ resize $ _wdata inpDat), S2M_WriteData{_wready = True})

  -- if state is being asked for, log the burst length requested
  processReadAddress M2S_NoReadAddress = pure (S2M_ReadAddress { _arready = False })
  processReadAddress addrM2S | _arburst addrM2S /= BmFixed = pure (S2M_ReadAddress{ _arready = True })
  processReadAddress addrM2S = do
    (a,burstLenLeft,c,d,e,f,g) <- get
    when (Just (_araddr addrM2S) == statusAddress) $ put (a,burstLenLeft+(_arlen addrM2S),c,d,e,f,g)
    pure (S2M_ReadAddress { _arready = True })

  -- write down what status message we're sending (so it doesn't change between clock cycles)
  sendStatus = do
    (a,burstLenLeft,statusOut,d,numFree,f,g) <- get
    -- case (our status is requested, we aren't already giving it) of
    case (burstLenLeft == 0, statusOut) of
      (False, S2M_NoReadData) -> put (a,burstLenLeft-1,S2M_ReadData { _rdata = numFree, _rid = 0, _rresp = ROkay, _rlast = burstLenLeft == 1, _ruser = rdUser },d,numFree,f,g)
      _ -> pure ()

  -- when we're acknowledged, clear our output for next clock cycle (it stays the same this cycle)
  clearStatus ack = when (_rready ack) $ do
    (a,b,_,d,e,f,g) <- get
    put (a, b, S2M_NoReadData, d, e, f, g)

  clearData ack = when ack $ do
    (a,b,c,_,e,f,g) <- get
    put (a, b, c, NoData, e, f, g)

  -- we have Index (depth+1) but we only want to access blockram up to depth-1
  incIdxLooping idx = if idx >= (maxBound-1) then 0 else idx+1

-- | Axi4 to Df sink
-- * Reading from the given address pops an item from the fifo and returns Right
-- * Reading from statusAddress returns Left, and how much free space is left in the buffer
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
-- TODO for keepSize, verify the size
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
  -- (amount left in read burst, axi read data (amt left or data), amount of space left, next place to read from, next place to write to, fifo)
  s0 = let numFree = maxBound
           nextRW = numFree * 0
           -- extremely janky, but necessary for type checking;
           --   forces nextRead and nextWrite to have the same type (that is, Index (depth+1)) as numFree
           --   (since ghc wouldn't be able to tell their type otherwise)
           -- nextRW is initialized to 0, although it could take on any value and the buffer would still work
       in
           (Left 0, S2M_NoReadData, numFree, nextRW, nextRW)

  -- when reset is on, output blank/default and don't change any state
  fullStateMachine (_,True,_,_,_) = pure (0, Nothing, Ack False, S2M_ReadAddress{_arready = False}, S2M_NoReadData{})
  fullStateMachine (brRead,False,inpDat,addrM2S,dataM2S) = do
    ackB <- processAddr addrM2S
    sendData brRead
    -- fix some outputs before they get changed for next time later on
    (_, dataS2M, _, brReadAddr, _) <- get
    clearData dataM2S
    (brWrite, ackA) <- pushInpData inpDat
    pure (brReadAddr, brWrite, ackA, ackB, dataS2M)

  -- log whether our status/fifo elements are being asked for; return ack
  processAddr M2S_NoReadAddress = pure (S2M_ReadAddress{_arready = False})
  processAddr addrM2S | _arburst addrM2S /= BmFixed = do
    (_,b,c,d,e) <- get
    put (Left 0, b, c, d, e)
    pure (S2M_ReadAddress{_arready = True})
  processAddr addrM2S = do
    (_,b,c,d,e) <- get
    put (pureProcessAddr (_araddr addrM2S) (_arlen addrM2S), b, c, d, e)
    pure (S2M_ReadAddress{_arready = True})

  pureProcessAddr addr n | addr == respondAddress = Right n
  pureProcessAddr addr n | Just addr == statusAddress = Left n
  pureProcessAddr _ _ = Left 0

  -- decide which data to send, given block ram read value
  sendData brRead = do
    (burstLenLeft,currOtp,numFree,nextRead,e) <- get
    -- case (do we want status or fifo element?, are we outputting anything currently?, do we have anything in our buffer?) of
    case (burstLenLeft, currOtp, numFree == maxBound) of
      (Left 0, _, _) -> pure ()
      (Right 0, _, _) -> pure ()
      (Left n, S2M_NoReadData, _) ->
        put (Left (n-1),
             S2M_ReadData { _rid = 0, _rdata = Left numFree, _rresp = ROkay, _rlast = n == 1, _ruser = rdUserStatus },
             numFree,
             nextRead,
             e)
      (Right n, S2M_NoReadData, False) ->
        put (Right (n-1),
             S2M_ReadData { _rid = 0, _rdata = Right brRead, _rresp = ROkay, _rlast = n == 1, _ruser = rdUserRead },
             numFree+1,
             incIdxLooping nextRead,
             e)
      (Right n, S2M_NoReadData, True) ->
        put (Right (n-1),
             S2M_ReadData { _rid = 0, _rdata = Left numFree, _rresp = RSlaveError, _rlast = n == 1, _ruser = rdUserErr },
             numFree,
             nextRead,
             e)
      _ -> pure ()

  -- when we're acknowledged, clear our output for next clock cycle (it stays the same this cycle)
  clearData dataM2S = when (_rready dataM2S) $ do
    (a,_,c,d,e) <- get
    put (a,S2M_NoReadData,c,d,e)

  -- push input data onto the stack
  pushInpData NoData = pure (Nothing, Ack False)
  pushInpData (Data inpDat) = do
    (a,b,numFree,d,nextWrite) <- get
    if numFree == 0 then pure (Nothing, Ack False) else do
      put (a,b,numFree-1,d,incIdxLooping nextWrite)
      pure (Just (nextWrite, inpDat), Ack True)

  -- we have Index (depth+1) but we only want to access blockram up to depth-1
  incIdxLooping idx = if idx >= (maxBound-1) then 0 else idx+1
