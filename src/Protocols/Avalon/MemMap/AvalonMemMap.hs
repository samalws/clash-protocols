{-|
Types and instance declarations for the Avalon memory mapped protocol
(http://www1.cs.columbia.edu/~sedwards/classes/2009/4840/mnl_avalon_spec.pdf).
Non-required fields can be easily toggled by the user.
The @data@ and @outputenable@ fields are not supported since we would need bidirectional data ports.
The @resetrequest@ field is also not supported since this does not get transferred around,
but rather gets send "outwards" to whoever is controlling the reset signal of the circuit.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Avalon.MemMap.AvalonMemMap where

-- base
import           Prelude hiding (not, (&&), (||), repeat, (!!), foldl, unzip, head)

import           Control.Arrow ((***))
import           Control.DeepSeq (NFData)
import qualified Data.Maybe as Maybe
import           Data.Proxy
-- import           Data.Type.Ord (OrdCond, Compare)

-- clash-prelude
import           Clash.Prelude hiding (take, concat, length)
import qualified Clash.Prelude as C

-- testing related
import           Hedgehog.Internal.Property (failWith)
import           Text.Show.Pretty (ppShow)

-- me
import           Protocols.Internal
import           Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike
import           Protocols.Hedgehog.Internal

-- | TODO comment
type family OrdIsEq (o :: Ordering) where
  OrdIsEq 'EQ = 'True
  OrdIsEq 'LT = 'False
  OrdIsEq 'GT = 'False

-- | TODO comment
type family (a :: Bool) ||? (b :: Bool) where
  'True ||? 'True = 'True
  'True ||? 'False = 'True
  'False ||? 'True = 'True
  'False ||? 'False = 'False

-- | TODO comment
type a ==? b = OrdIsEq (CmpNat a b)

-- | TODO comment
type EqOrZero a b = (a ==? b) ||? (a ==? 0)


-- Config needed for both master and slave interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
data AvalonMMSharedConfig
  =  AvalonMMSharedConfig
  { addrWidth         :: Nat
  , keepRead          :: Bool
  , keepWrite         :: Bool
  , byteEnableWidth   :: Nat
  , burstCountWidth   :: Nat
  , keepReadDataValid :: Bool
  , keepEndOfPacket   :: Bool
  , keepIrq           :: Bool
  }

-- Config specific to Avalon MM slave interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
-- An @AvalonMMSharedConfig@ is also included for the rest of the fields.
data AvalonMMSlaveConfig
  =  AvalonMMSlaveConfig
  { writeByteEnableWidth   :: Nat
  , keepChipSelect         :: Bool
  , keepBeginTransfer      :: Bool
  , keepWaitRequest        :: Bool
  , keepBeginBurstTransfer :: Bool
  , keepReadyForData       :: Bool
  , keepDataAvailable      :: Bool
  , sShared                :: AvalonMMSharedConfig
  }

-- Config specific to Avalon MM master interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
-- An @AvalonMMSharedConfig@ is also included for the rest of the fields.
data AvalonMMMasterConfig
  =  AvalonMMMasterConfig
  { keepFlush      :: Bool
  , irqListWidth   :: Nat
  , irqNumberWidth :: Nat
  , mShared        :: AvalonMMSharedConfig
  }

-- Grab record fields at the type level:

type family AddrWidth (c :: AvalonMMSharedConfig) where
  AddrWidth ('AvalonMMSharedConfig a _ _ _ _ _ _ _) = a

type family KeepRead (c :: AvalonMMSharedConfig) where
  KeepRead ('AvalonMMSharedConfig _ a _ _ _ _ _ _) = a

type family KeepWrite (c :: AvalonMMSharedConfig) where
  KeepWrite ('AvalonMMSharedConfig _ _ a _ _ _ _ _) = a

type family ByteEnableWidth (c :: AvalonMMSharedConfig) where
  ByteEnableWidth ('AvalonMMSharedConfig _ _ _ a _ _ _ _) = a

type family BurstCountWidth (c :: AvalonMMSharedConfig) where
  BurstCountWidth ('AvalonMMSharedConfig _ _ _ _ a _ _ _) = a

type family KeepReadDataValid (c :: AvalonMMSharedConfig) where
  KeepReadDataValid ('AvalonMMSharedConfig _ _ _ _ _ a _ _) = a

type family KeepEndOfPacket (c :: AvalonMMSharedConfig) where
  KeepEndOfPacket ('AvalonMMSharedConfig _ _ _ _ _ _ a _) = a

type family KeepIrq (c :: AvalonMMSharedConfig) where
  KeepIrq ('AvalonMMSharedConfig _ _ _ _ _ _ _ a) = a


type family WriteByteEnableWidth (c :: AvalonMMSlaveConfig) where
  WriteByteEnableWidth ('AvalonMMSlaveConfig a _ _ _ _ _ _ _) = a

type family KeepChipSelect (c :: AvalonMMSlaveConfig) where
  KeepChipSelect ('AvalonMMSlaveConfig _ a _ _ _ _ _ _) = a

type family KeepBeginTransfer (c :: AvalonMMSlaveConfig) where
  KeepBeginTransfer ('AvalonMMSlaveConfig _ _ a _ _ _ _ _) = a

type family KeepWaitRequest (c :: AvalonMMSlaveConfig) where
  KeepWaitRequest ('AvalonMMSlaveConfig _ _ _ a _ _ _ _) = a

type family KeepBeginBurstTransfer (c :: AvalonMMSlaveConfig) where
  KeepBeginBurstTransfer ('AvalonMMSlaveConfig _ _ _ _ a _ _ _) = a

type family KeepReadyForData (c :: AvalonMMSlaveConfig) where
  KeepReadyForData ('AvalonMMSlaveConfig _ _ _ _ _ a _ _) = a

type family KeepDataAvailable (c :: AvalonMMSlaveConfig) where
  KeepDataAvailable ('AvalonMMSlaveConfig _ _ _ _ _ _ a _) = a

type family SShared (c :: AvalonMMSlaveConfig) where
  SShared ('AvalonMMSlaveConfig _ _ _ _ _ _ _ a) = a


type family KeepFlush (c :: AvalonMMMasterConfig) where
  KeepFlush ('AvalonMMMasterConfig a _ _ _) = a

type family IrqListWidth (c :: AvalonMMMasterConfig) where
  IrqListWidth ('AvalonMMMasterConfig _ a _ _) = a

type family IrqNumberWidth (c :: AvalonMMMasterConfig) where
  IrqNumberWidth ('AvalonMMMasterConfig _ _ a _) = a

type family MShared (c :: AvalonMMMasterConfig) where
  MShared ('AvalonMMMasterConfig _ _ _ a) = a


-- Class representing a well-behaved shared config.
-- This class holds for every possible @AvalonMMSharedConfig@,
-- but we need to write out the class anyway so that GHC holds.
class
  ( KnownNat      (AddrWidth         config)
  , KeepTypeClass (KeepRead          config)
  , KeepTypeClass (KeepWrite         config)
  , MaybeZeroNat  (ByteEnableWidth   config)
  , MaybeZeroNat  (BurstCountWidth   config)
  , KeepTypeClass (KeepReadDataValid config)
  , KeepTypeClass (KeepEndOfPacket   config)
  , KeepTypeClass (KeepIrq           config)
  ) => GoodMMSharedConfig config
instance
  ( KnownNat      (AddrWidth         config)
  , KeepTypeClass (KeepRead          config)
  , KeepTypeClass (KeepWrite         config)
  , MaybeZeroNat  (ByteEnableWidth   config)
  , MaybeZeroNat  (BurstCountWidth   config)
  , KeepTypeClass (KeepReadDataValid config)
  , KeepTypeClass (KeepEndOfPacket   config)
  , KeepTypeClass (KeepIrq           config)
  ) => GoodMMSharedConfig config

-- Class representing a well-behaved slave config.
-- This class holds for every possible @AvalonMMSlaveConfig@,
-- but we need to write out the class anyway so that GHC holds.
class
  ( MaybeZeroNat  (WriteByteEnableWidth   config)
  , KeepTypeClass (KeepChipSelect         config)
  , KeepTypeClass (KeepBeginTransfer      config)
  , KeepTypeClass (KeepWaitRequest        config)
  , KeepTypeClass (KeepBeginBurstTransfer config)
  , KeepTypeClass (KeepReadyForData       config)
  , KeepTypeClass (KeepDataAvailable      config)
  , GoodMMSharedConfig (SShared           config)
  ) => GoodMMSlaveConfig config
instance
  ( MaybeZeroNat  (WriteByteEnableWidth   config)
  , KeepTypeClass (KeepChipSelect         config)
  , KeepTypeClass (KeepBeginTransfer      config)
  , KeepTypeClass (KeepWaitRequest        config)
  , KeepTypeClass (KeepBeginBurstTransfer config)
  , KeepTypeClass (KeepReadyForData       config)
  , KeepTypeClass (KeepDataAvailable      config)
  , GoodMMSharedConfig (SShared           config)
  ) => GoodMMSlaveConfig config

-- Class representing a well-behaved master config.
-- This class holds for every possible @AvalonMMMasterConfig@,
-- but we need to write out the class anyway so that GHC holds.
class
  ( KeepTypeClass (KeepFlush      config)
  , KnownNat      (IrqListWidth   config)
  , KnownNat      (IrqNumberWidth config)
  , GoodMMSharedConfig (MShared   config)
  ) => GoodMMMasterConfig config
instance
  ( KeepTypeClass (KeepFlush      config)
  , KnownNat      (IrqListWidth   config)
  , KnownNat      (IrqNumberWidth config)
  , GoodMMSharedConfig (MShared   config)
  ) => GoodMMMasterConfig config


-- Data coming out of an Avalon MM master port.
-- All fields are optional and can be toggled using the config.
data AvalonMasterOut config writeDataType
  =  AvalonMasterOut
  { mo_addr        :: Unsigned (AddrWidth       (MShared config))
  , mo_read        :: KeepType (KeepRead        (MShared config)) Bool
  , mo_write       :: KeepType (KeepWrite       (MShared config)) Bool
  , mo_byteEnable  :: Unsigned (ByteEnableWidth (MShared config))
  , mo_burstCount  :: Unsigned (BurstCountWidth (MShared config))
  , mo_flush       :: KeepType (KeepFlush                config) Bool
  , mo_writeData   :: writeDataType
  }
  deriving Generic

deriving instance (GoodMMMasterConfig config,
                   NFDataX writeDataType)
                   => NFDataX (AvalonMasterOut config writeDataType)
deriving instance (GoodMMMasterConfig config,
                   NFData writeDataType)
                   => NFData (AvalonMasterOut config writeDataType)
deriving instance (GoodMMMasterConfig config,
                   ShowX writeDataType)
                   => ShowX (AvalonMasterOut config writeDataType)
deriving instance (GoodMMMasterConfig config,
                   Show writeDataType)
                   => Show (AvalonMasterOut config writeDataType)
deriving instance (GoodMMMasterConfig config,
                   Eq writeDataType)
                   => Eq (AvalonMasterOut config writeDataType)


-- Data coming into an Avalon MM master port.
-- Almost all fields are optional and can be toggled using the config.
-- WaitRequest is mandatory.
data AvalonMasterIn config readDataType
  =  AvalonMasterIn
  { mi_waitRequest   :: Bool
  , mi_readDataValid :: KeepType (KeepReadDataValid (MShared config)) Bool
  , mi_endOfPacket   :: KeepType (KeepEndOfPacket   (MShared config)) Bool
  , mi_irq           :: KeepType (KeepIrq           (MShared config)) Bool
  , mi_irqList       :: Unsigned (IrqListWidth               config)
  , mi_irqNumber     :: Unsigned (IrqNumberWidth             config)
  , mi_readData      :: readDataType
  }
  deriving Generic

deriving instance (GoodMMMasterConfig config,
                   NFDataX readDataType)
                   => NFDataX (AvalonMasterIn config readDataType)
deriving instance (GoodMMMasterConfig config,
                   NFData readDataType)
                   => NFData (AvalonMasterIn config readDataType)
deriving instance (GoodMMMasterConfig config,
                   ShowX readDataType)
                   => ShowX (AvalonMasterIn config readDataType)
deriving instance (GoodMMMasterConfig config,
                   Show readDataType)
                   => Show (AvalonMasterIn config readDataType)
deriving instance (GoodMMMasterConfig config,
                   Eq readDataType)
                   => Eq (AvalonMasterIn config readDataType)


-- Data coming out of an Avalon MM slave port.
-- All fields are optional and can be toggled using the config.
data AvalonSlaveOut config readDataType
  =  AvalonSlaveOut
  { so_readDataValid :: KeepType (KeepReadDataValid (SShared config)) Bool
  , so_endOfPacket   :: KeepType (KeepEndOfPacket   (SShared config)) Bool
  , so_irq           :: KeepType (KeepIrq           (SShared config)) Bool
  , so_waitRequest   :: KeepType (KeepWaitRequest            config) Bool
  , so_readyForData  :: KeepType (KeepReadyForData           config) Bool
  , so_dataAvailable :: KeepType (KeepDataAvailable          config) Bool
  , so_readData      :: readDataType
  }
  deriving Generic

deriving instance (GoodMMSlaveConfig config,
                   NFDataX readDataType)
                   => NFDataX (AvalonSlaveOut config readDataType)
deriving instance (GoodMMSlaveConfig config,
                   NFData readDataType)
                   => NFData (AvalonSlaveOut config readDataType)
deriving instance (GoodMMSlaveConfig config,
                   ShowX readDataType)
                   => ShowX (AvalonSlaveOut config readDataType)
deriving instance (GoodMMSlaveConfig config,
                   Show readDataType)
                   => Show (AvalonSlaveOut config readDataType)
deriving instance (GoodMMSlaveConfig config,
                   Eq readDataType)
                   => Eq (AvalonSlaveOut config readDataType)


-- Data coming into an Avalon MM slave port.
-- All fields are optional and can be toggled using the config.
data AvalonSlaveIn config writeDataType
  =  AvalonSlaveIn
  { si_addr               :: Unsigned (AddrWidth              (SShared config))
  , si_read               :: KeepType (KeepRead               (SShared config)) Bool
  , si_write              :: KeepType (KeepWrite              (SShared config)) Bool
  , si_byteEnable         :: Unsigned (ByteEnableWidth        (SShared config))
  , si_burstCount         :: Unsigned (BurstCountWidth        (SShared config))
  , si_writeByteEnable    :: Unsigned (WriteByteEnableWidth            config)
  , si_chipSelect         :: KeepType (KeepChipSelect                  config) Bool
  , si_beginTransfer      :: KeepType (KeepBeginTransfer               config) Bool
  , si_beginBurstTransfer :: KeepType (KeepBeginBurstTransfer          config) Bool
  , si_writeData          :: writeDataType
  }
  deriving Generic

deriving instance (GoodMMSlaveConfig config,
                   NFDataX writeDataType)
                   => NFDataX (AvalonSlaveIn config writeDataType)
deriving instance (GoodMMSlaveConfig config,
                   NFData writeDataType)
                   => NFData (AvalonSlaveIn config writeDataType)
deriving instance (GoodMMSlaveConfig config,
                   ShowX writeDataType)
                   => ShowX (AvalonSlaveIn config writeDataType)
deriving instance (GoodMMSlaveConfig config,
                   Show writeDataType)
                   => Show (AvalonSlaveIn config writeDataType)
deriving instance (GoodMMSlaveConfig config,
                   Eq writeDataType)
                   => Eq (AvalonSlaveIn config writeDataType)

-- Interconnect fabric, which can be used to tie together multiple masters and slaves.
-- Masters and slaves cannot contact each other directly; this fabric is needed in order to mediate,
--   since masters and slaves do not have the same data fields.
-- Parameters:
-- * slaveAddrFns: functions indicating whether a given address refers to a slave
-- * irqNums: IRQ numbers for each slave
-- * fixedWaitTime: SNat representing the length of the fixed wait-state (0 if there is none)
-- TODO support (slaveA,slaveB) where a and b have different config
avalonInterconnectFabric ::
  ( HiddenClockResetEnable dom
  , KnownNat fixedWaitTime
  , KnownNat nMaster
  , KnownNat nSlave
  , nSlave ~ (decNSlave + 1)
  , GoodMMMasterConfig masterConfig
  , GoodMMSlaveConfig  slaveConfig
  , AddrWidth (MShared masterConfig) ~ AddrWidth (SShared slaveConfig)
  , EqOrZero (WriteByteEnableWidth slaveConfig)      (ByteEnableWidth (MShared masterConfig)) ~ 'True
  , EqOrZero (ByteEnableWidth (SShared slaveConfig)) (ByteEnableWidth (MShared masterConfig)) ~ 'True
  , BurstCountWidth (MShared masterConfig) ~ BurstCountWidth (SShared slaveConfig)
  )
  => Vec nSlave (Unsigned (AddrWidth (SShared slaveConfig)) -> Bool)
  -> Vec nSlave (Unsigned (IrqNumberWidth masterConfig))
  -> SNat fixedWaitTime
  -> Circuit
     (Vec nMaster (AvalonMMMaster dom               masterConfig readDataType writeDataType))
     (Vec nSlave  (AvalonMMSlave  dom fixedWaitTime slaveConfig  readDataType writeDataType))
avalonInterconnectFabric slaveAddrFns irqNums fixedWaitTime = Circuit cktFn where
  -- We use a mealy machine, since state is necessary to keep track of which master is connected to which slave.
  cktFn (inpA, inpB) = (unbundle otpA, unbundle otpB) where (otpA, otpB) = unbundle $ mealy transFn s0 $ bundle (bundle inpA, bundle inpB)

  -- (sm: which slave was connected to which master last clock cycle, xferSt: state for each master-to-slave connection)
  -- xferSt is indexed by slave
  -- xferSt: Vec (Maybe (ctr1: num waitrequest=false&read=true - num readdatavalid=true, ctr2: xfers left in burst (dec on readdatavalid=true OR waitrequest=false&write=true), ctr3: fixed wait time left (dec always, loop around on good message), ready for transfer (becomes True on waitrequest=false and ctr3=0, False on good message)))
  -- xferState[slave] = Nothing indicates that slave is not connected to any master
  s0 = (repeat Nothing, repeat Nothing)

  -- transition function, called every clock cycle
  -- takes in old state and input, returns new state and output
  transFn (smOld, xferSt) (mo, so) = ((sm, xferSt'), (mi, si)) where
    -- figure out which slave gets paired with which master, and vice versa
    (ms, sm) = masterSlavePairings mo smOld xferSt
    -- get the interrupt request number
    mirq = minIrq so
    -- get the interrupt list (n slaves produce n bools; then resize, padding with zeros)
    irqList = fromKeepTypeDef False . so_irq <$> so
    -- set IRQ-related fields of a master-in message using the values calculated above
    setIrq miMsg = miMsg { mi_irq = toKeepType (Maybe.isJust mirq), mi_irqList = unpack $ resize $ pack irqList, mi_irqNumber = Maybe.fromMaybe 0 mirq }
    -- calculate all master-in messages
    mi = setIrq . maybe mmMasterInNoData (\n -> convSoMi (so !! n) (xferSt !! n)) <$> ms
    -- calculate all slave-in messages
    si = maybe (const mmSlaveInNoData) (\n -> convMoSi (mo !! n)) <$> sm <*> xferSt
    -- calculate the next xferStates
    xferSt' = modifySt <$> (fmap (mo !!) <$> sm) <*> so <*> xferSt

  -- out of all slaves with IRQ turned on, return the smallest IRQ number
  minIrq so = fold minJust $ irqNum <$> so <*> irqNums where
    minJust (Just a) (Just b) | a < b = Just a
    minJust (Just a) Nothing = Just a
    minJust _ b = b

    irqNum soMsg num = if fromKeepTypeDef False (so_irq soMsg) then Just num else Nothing

  -- figure out which master is paired with which slave (ms) and vice versa (sm)
  -- given current master-out messages; previous sm value; and all the xferStates
  masterSlavePairings mo smOld xferSt = (ms, sm) where
    -- for old sm values, determine if they're still transmitting
    smOld' = (\smOldElem addrFn xferStI -> smOldElem >>= keepSM addrFn xferStI) <$> smOld <*> slaveAddrFns <*> xferSt
    -- a transmission is still going if the xferState is Just or if the master is still asking to connect
    keepSM addrFn xferStI idx = if (moGood addrFn (mo !! idx)) || Maybe.isJust xferStI then Just idx else Nothing
    -- get new slave-to-master connections in case a slave is disconnectd and a master wants to connect to it
    smCurr = (\addrFn -> findIndex (moGood addrFn) mo) <$> slaveAddrFns
    -- given addrFn, does master-out message want to connect to this address?
    moGood addrFn moMsg = moIsOn moMsg && addrFn (mo_addr moMsg)
    -- make slave-to-master pairings, preferring existing connections
    sm = (<|>) <$> smOld' <*> smCurr
    -- figure out master-to-slave pairings based on sm
    ms = flip elemIndex sm . Just <$> iterateI (+ 1) 0

  -- mo wants to read or write
  moIsOn mo = (fromKeepTypeDef True (mo_read mo) || fromKeepTypeDef True (mo_write mo)) && (0 /= fromMaybeEmptyNum 1 (mo_byteEnable mo))
  -- mo wants to read
  moIsRead mo = moIsOn mo && fromKeepTypeDef True (mo_read mo) && not (fromKeepTypeDef False (mo_write mo))
  -- mo wants to write
  moIsWrite mo = moIsOn mo && fromKeepTypeDef True (mo_write mo) && not (fromKeepTypeDef False (mo_read mo))

  -- modify one xferSt value, given one master-out message and one slave-out message
  -- if there is no master connected, our state should be Nothing
  modifySt Nothing _ _ = Nothing
  -- if there is a master connected, give a default value of xferSt if needed, and then call on modifySt' to modify it
  modifySt (Just mo) so st = modifySt' mo so (Maybe.fromMaybe (0 :: Unsigned 8,
                                                               mo_burstCount mo,
                                                               _0 fixedWaitTime,
                                                               False) st)
  modifySt' mo so (ctr1, ctr2, ctr3, readyForTransfer) = modifySt'' (optDecCtr so $ optIncCtr1 mo so ctr1,
                                                                     optDecCtr2 mo so ctr2,
                                                                     modifyCtr3 mo ctr3,
                                                                     modifyReadyForTransfer mo so ctr3 readyForTransfer)
  -- increment ctr1 if we're reading and waitrequest=false
  optIncCtr1 mo so ctr1 = if shouldIncCtr1 mo so then ctr1+1 else ctr1
  shouldIncCtr1 mo so = moIsRead mo && not (fromKeepTypeDef False (so_waitRequest so))
  -- decrement ctr2 if (we're writing and waitrequest=false) or readdatavalid=true
  optDecCtr2 mo so ctr2 = if (moIsWrite mo && not (fromKeepTypeDef False (so_waitRequest so)) && ctr2 /= 0) then ctr2-1 else optDecCtr so ctr2
  -- decrement ctr if readdatavalid=true
  optDecCtr so ctr = if (ctr /= 0) && (fromKeepTypeDef True $ so_readDataValid so) then ctr-1 else ctr
  -- always decrement ctr3; loop around to maxBound if mo is sending something
  -- this is for fixed wait state interfaces
  modifyCtr3 mo 0 = if moIsOn mo then maxBound else 0
  modifyCtr3 _ n = n-1
  modifyReadyForTransfer mo so ctr3 readyForTransfer
    | not (fromKeepTypeDef False (so_waitRequest so)) && ctr3 == 0 = True
    | moIsOn mo = False
    | otherwise = readyForTransfer
  -- finally, kill the xferSt if all the counters are at 0
  modifySt'' (0, 0, 0, _) = Nothing
  modifySt'' st = Just st
  -- hack to get a "0" value of the right type
  _0 :: (KnownNat n) => SNat n -> Index (n+1)
  _0 _ = 0

  -- given slave-out message and xferSt, generate master-in message
  convSoMi so st
    = AvalonMasterIn
    { mi_waitRequest   = Maybe.maybe True (\(ctr1,_,ctr3,_) -> ctr1 < maxBound && ctr3 == 0) st && (fromKeepTypeDef False (so_waitRequest so))
    , mi_readDataValid = convKeepType False (so_readDataValid so)
    , mi_endOfPacket   = convKeepType False (so_endOfPacket so)
    , mi_irq           = errorX "interconnect fabric: this value gets overwritten later"
    , mi_irqList       = errorX "interconnect fabric: this value gets overwritten later"
    , mi_irqNumber     = errorX "interconnect fabric: this value gets overwritten later"
    , mi_readData      = so_readData so
    }

  -- given master-out message and xferSt, generate slave-in message
  convMoSi mo st
    = AvalonSlaveIn
    { si_addr               = mo_addr mo
    , si_read               = toKeepType $ fromKeepTypeDef True (mo_read  mo) && not (fromKeepTypeDef False (mo_write mo))
    , si_write              = toKeepType $ fromKeepTypeDef True (mo_write mo) && not (fromKeepTypeDef False (mo_read  mo))
    , si_writeByteEnable    = resize $ if (fromKeepTypeDef True (mo_write mo)) then mo_byteEnable mo else 0
    , si_burstCount         = mo_burstCount mo
    , si_chipSelect         = toKeepType True
    , si_byteEnable         = resize $ mo_byteEnable mo
    , si_beginTransfer      = toKeepType $ moIsOn mo && (Maybe.maybe True (\(_,_,_,readyForMsg) -> readyForMsg) st)
    , si_beginBurstTransfer = toKeepType $ Maybe.isNothing st
    , si_writeData          = mo_writeData mo
    }

-- Interconnect fabric, but there's only one master and one slave.
-- Vecs are removed for convenience.
avalonInterconnectFabricSingleMember ::
  ( HiddenClockResetEnable dom
  , KnownNat fixedWaitTime
  , GoodMMMasterConfig masterConfig
  , GoodMMSlaveConfig  slaveConfig
  , AddrWidth (MShared masterConfig) ~ AddrWidth (SShared slaveConfig)
  , EqOrZero (WriteByteEnableWidth slaveConfig)      (ByteEnableWidth (MShared masterConfig)) ~ 'True
  , EqOrZero (ByteEnableWidth (SShared slaveConfig)) (ByteEnableWidth (MShared masterConfig)) ~ 'True
  , BurstCountWidth (MShared masterConfig) ~ BurstCountWidth (SShared slaveConfig)
  )
  => (Unsigned (AddrWidth (SShared slaveConfig)) -> Bool)
  -> Unsigned (IrqNumberWidth masterConfig)
  -> SNat fixedWaitTime
  -> Circuit
     (AvalonMMMaster dom               masterConfig readDataType writeDataType)
     (AvalonMMSlave  dom fixedWaitTime slaveConfig  readDataType writeDataType)
avalonInterconnectFabricSingleMember slaveAddrFn irqNum fixedWaitTime
  = Circuit ((head *** head) . toSignals (avalonInterconnectFabric (singleton slaveAddrFn) (singleton irqNum) fixedWaitTime) . (singleton *** singleton))


-- Convert a boolean value to an @AvalonSlaveOut@ structure.
-- The structure gives no read data, no IRQ, etc.
-- Fields relating to "acknowledging" a write are controlled by the bool input.
boolToMMSlaveAck :: (GoodMMSlaveConfig config) => Bool -> AvalonSlaveOut config readDataType
boolToMMSlaveAck ack
  = AvalonSlaveOut
    { so_waitRequest   = toKeepType (not ack)
    , so_readDataValid = toKeepType False
    , so_readyForData  = toKeepType ack
    , so_dataAvailable = toKeepType False
    , so_endOfPacket   = toKeepType False
    , so_irq           = toKeepType False
    , so_readData      = errorX "No readData for boolToAck"
    }

-- Convert a boolean value to an @AvalonMasterIn@ structure.
-- The structure gives no read data, no IRQ, etc.
-- The @waitRequest@ field is controlled by the (negated) boolean input.
boolToMMMasterAck :: (GoodMMMasterConfig config) => Bool -> AvalonMasterIn config readDataType
boolToMMMasterAck ack
  = AvalonMasterIn
  { mi_waitRequest   = not ack
  , mi_readDataValid = toKeepType False
  , mi_endOfPacket   = toKeepType False
  , mi_irq           = toKeepType False
  , mi_irqList       = 0
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No readData for boolToAck"
  }

-- An @AvalonMasterIn@ containing no read data, but not giving a wait request or an IRQ.
mmMasterInNoData :: (GoodMMMasterConfig config) => AvalonMasterIn config readDataType
mmMasterInNoData
  = AvalonMasterIn
  { mi_waitRequest   = False
  , mi_readDataValid = toKeepType False
  , mi_endOfPacket   = toKeepType False
  , mi_irq           = toKeepType False
  , mi_irqList       = 0
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No read data defined"
  }

-- An @AvalonSlaveIn@ containing no write data, and indicating that no transmission is currently occurring.
mmSlaveInNoData :: (GoodMMSlaveConfig config) => AvalonSlaveIn config writeDataType
mmSlaveInNoData
  = AvalonSlaveIn
  { si_chipSelect         = toKeepType False
  , si_addr               = 0
  , si_read               = toKeepType False
  , si_write              = toKeepType False
  , si_byteEnable         = 0
  , si_writeByteEnable    = 0
  , si_beginTransfer      = toKeepType False
  , si_burstCount         = 0
  , si_beginBurstTransfer = toKeepType False
  , si_writeData          = errorX "No writeData for noData"
  }

-- An @AvalonMasterOut@ containing no write data, and indicating that no transmission is currently occurring.
mmMasterOutNoData :: (GoodMMMasterConfig config) => AvalonMasterOut config writeDataType
mmMasterOutNoData
  = AvalonMasterOut
  { mo_addr        = 0
  , mo_read        = toKeepType False
  , mo_write       = toKeepType False
  , mo_byteEnable  = 0
  , mo_burstCount  = 0
  , mo_flush       = toKeepType False
  , mo_writeData   = errorX "No writeData for noData"
  }

-- Grab the "acknowledgement" value from an @AvalonSlaveOut@.
-- Reasonable defaults are provided for optional fields.
mmSlaveOutToBool :: (GoodMMSlaveConfig config) => AvalonSlaveOut config readDataType -> Bool
mmSlaveOutToBool so = fromKeepTypeDef True (so_readyForData so) && not (fromKeepTypeDef False (so_waitRequest so))

-- Grab the "acknowledgement" value from an @AvalonMasterIn@.
-- Reasonable defaults are provided for optional fields.
mmMasterInToBool :: (GoodMMMasterConfig config) => AvalonMasterIn config readDataType -> Bool
mmMasterInToBool = not . mi_waitRequest

-- Default @AvalonSlaveIn@ whose fields indicate that a write transaction is occurring.
-- The @writeData@ field needs to be filled in with data.
mmSlaveInSendingData :: (GoodMMSlaveConfig config) => AvalonSlaveIn config writeDataType
mmSlaveInSendingData
  = AvalonSlaveIn
  { si_chipSelect         = toKeepType True
  , si_addr               = 0
  , si_read               = toKeepType False
  , si_write              = toKeepType True
  , si_byteEnable         = bitCoerce $ repeat True
  , si_writeByteEnable    = bitCoerce $ repeat True
  , si_beginTransfer      = toKeepType False
  , si_burstCount         = 0
  , si_beginBurstTransfer = toKeepType False
  , si_writeData          = errorX "No writeData for mmSlaveInSendingData"
  }

-- Default @AvalonMasterOut@ whose fields indicate that a write transaction is occurring.
-- The @writeData@ field needs to be filled in with data.
mmMasterOutSendingData :: (GoodMMMasterConfig config) => AvalonMasterOut config writeDataType
mmMasterOutSendingData
  = AvalonMasterOut
  { mo_addr        = 0
  , mo_read        = toKeepType False
  , mo_write       = toKeepType True
  , mo_byteEnable  = bitCoerce $ repeat True
  , mo_burstCount  = 1
  , mo_flush       = toKeepType False
  , mo_writeData   = errorX "No writeData for mmMasterOutSendingData"
  }


-- Grab the data from an @AvalonSlaveIn@, if there is any.
mmSlaveInToMaybe :: (GoodMMSlaveConfig config) => AvalonSlaveIn config writeDataType -> Maybe writeDataType
mmSlaveInToMaybe si = if cond then Just (si_writeData si) else Nothing where
  cond =  fromKeepTypeDef True (si_chipSelect si)
       && fromKeepTypeDef True (si_write si)
       && not (fromKeepTypeDef False (si_read si))
       && 0 /= fromMaybeEmptyNum 1 (si_byteEnable si)
       && 0 /= fromMaybeEmptyNum 1 (si_writeByteEnable si)

-- Grab the data from an @AvalonMasterOut@, if there is any.
mmMasterOutToMaybe :: (GoodMMMasterConfig config) => AvalonMasterOut config writeDataType -> Maybe writeDataType
mmMasterOutToMaybe mo = if cond then Just (mo_writeData mo) else Nothing where
  cond =  fromKeepTypeDef True (mo_write mo)
       && not (fromKeepTypeDef False (mo_read mo))
       && 0 /= fromMaybeEmptyNum 1 (mo_byteEnable mo)

-- TODO rename master to whatever they changed it to
-- TODO support fixed wait time in instances below

-- Datatype for the master end of the Avalon memory-mapped protocol.
data AvalonMMMaster (dom :: Domain) (config :: AvalonMMMasterConfig) (readDataType :: Type) (writeDataType :: Type) = AvalonMMMaster

-- Datatype for the slave end of the Avalon memory-mapped protocol.
data AvalonMMSlave (dom :: Domain) (fixedWaitTime :: Nat) (config :: AvalonMMSlaveConfig) (readDataType :: Type) (writeDataType :: Type) = AvalonMMSlave

instance Protocol (AvalonMMMaster dom config readDataType writeDataType) where
  type Fwd (AvalonMMMaster dom config readDataType writeDataType) = Signal dom (AvalonMasterOut config writeDataType)
  type Bwd (AvalonMMMaster dom config readDataType writeDataType) = Signal dom (AvalonMasterIn  config readDataType)

instance Protocol (AvalonMMSlave dom fixedWaitTime config readDataType writeDataType) where
  type Fwd (AvalonMMSlave dom fixedWaitTime config readDataType writeDataType) = Signal dom (AvalonSlaveIn  config writeDataType)
  type Bwd (AvalonMMSlave dom fixedWaitTime config readDataType writeDataType) = Signal dom (AvalonSlaveOut config readDataType)

instance (GoodMMSlaveConfig config, KeepWaitRequest config ~ 'True) => Backpressure (AvalonMMSlave dom 0 config readDataType writeDataType) where
  boolsToBwd _ = C.fromList_lazy . fmap boolToMMSlaveAck

instance (GoodMMMasterConfig config) => Backpressure (AvalonMMMaster dom config readDataType writeDataType) where
  boolsToBwd _ = C.fromList_lazy . fmap boolToMMMasterAck

instance (KnownDomain dom, GoodMMSlaveConfig config) => Simulate (AvalonMMSlave dom 0 config readDataType writeDataType) where
  type SimulateFwdType (AvalonMMSlave dom 0 config readDataType writeDataType) = [AvalonSlaveIn  config writeDataType]
  type SimulateBwdType (AvalonMMSlave dom 0 config readDataType writeDataType) = [AvalonSlaveOut config readDataType]
  type SimulateChannels (AvalonMMSlave dom 0 config readDataType writeDataType) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = C.sample_lazy
  sigToSimBwd _ = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) = DfLike.stall Proxy conf stallAck stalls

instance (KnownDomain dom, GoodMMMasterConfig config) => Simulate (AvalonMMMaster dom config readDataType writeDataType) where
  type SimulateFwdType (AvalonMMMaster dom config readDataType writeDataType) = [AvalonMasterOut config writeDataType]
  type SimulateBwdType (AvalonMMMaster dom config readDataType writeDataType) = [AvalonMasterIn config readDataType]
  type SimulateChannels (AvalonMMMaster dom config readDataType writeDataType) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = C.sample_lazy
  sigToSimBwd _ = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) = DfLike.stall Proxy conf stallAck stalls

instance (KnownDomain dom, GoodMMSlaveConfig config, KeepWaitRequest config ~ 'True) => Drivable (AvalonMMSlave dom 0 config readDataType writeDataType) where
  type ExpectType (AvalonMMSlave dom 0 config readDataType writeDataType) = [writeDataType]

  toSimulateType Proxy = fmap (\dat -> mmSlaveInSendingData { si_writeData = dat })
  fromSimulateType Proxy = Maybe.mapMaybe mmSlaveInToMaybe

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy

instance (KnownDomain dom, GoodMMMasterConfig config) => Drivable (AvalonMMMaster dom config readDataType writeDataType) where
  type ExpectType (AvalonMMMaster dom config readDataType writeDataType) = [writeDataType]

  toSimulateType Proxy = fmap (\dat -> mmMasterOutSendingData { mo_writeData = dat })
  fromSimulateType Proxy = Maybe.mapMaybe mmMasterOutToMaybe

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy

instance (KnownDomain dom, GoodMMSlaveConfig config) => DfLike dom (AvalonMMSlave dom 0 config readDataType) writeDataType where
  type Data (AvalonMMSlave dom 0 config readDataType) writeDataType = AvalonSlaveIn config writeDataType
  type Payload writeDataType = writeDataType
  type Ack (AvalonMMSlave dom 0 config readDataType) writeDataType = AvalonSlaveOut config readDataType

  getPayload = const $ mmSlaveInToMaybe

  setPayload _ _ si (Just b) = si { si_writeData = b }
  setPayload _ _ _ Nothing = mmSlaveInNoData

  noData _ = mmSlaveInNoData

  boolToAck _ = boolToMMSlaveAck
  ackToBool _ = mmSlaveOutToBool

instance (KnownDomain dom, GoodMMMasterConfig config) => DfLike dom (AvalonMMMaster dom config readDataType) writeDataType where
  type Data (AvalonMMMaster dom config readDataType) writeDataType = AvalonMasterOut config writeDataType
  type Payload writeDataType = writeDataType
  type Ack (AvalonMMMaster dom config readDataType) writeDataType = AvalonMasterIn config readDataType

  getPayload = const $ mmMasterOutToMaybe

  setPayload _ _ mo (Just b) = mo { mo_writeData = b }
  setPayload _ _ _ Nothing = mmMasterOutNoData

  noData _ = mmMasterOutNoData

  boolToAck _ = boolToMMMasterAck
  ackToBool _ = mmMasterInToBool

instance (KnownDomain dom,
          GoodMMSlaveConfig config,
          KeepWaitRequest config ~ 'True,
          Show writeDataType,
          ShowX writeDataType,
          NFData writeDataType,
          NFDataX writeDataType,
          Eq writeDataType,
          Show readDataType,
          ShowX readDataType,
          NFData readDataType,
          NFDataX readDataType,
          Eq readDataType)
          => Test (AvalonMMSlave dom 0 config readDataType writeDataType) where

  expectToLengths Proxy = pure . length

  -- directly copied from Df instance, with minor changes made
  expectN Proxy (ExpectOptions{eoEmptyTail, eoTimeout}) (C.head -> nExpected) sampled = do
    go (Maybe.fromMaybe maxBound eoTimeout) nExpected sampled
   where
    catDatas [] = []
    catDatas (m2s:xs) = Maybe.maybe (catDatas xs) (:catDatas xs) (mmSlaveInToMaybe m2s)

    go _imeout _n [] =
      -- This really should not happen, protocols should produce data indefinitely
      error "unexpected end of signal"
    go _imeout 0 rest = do
      -- Check for superfluous output from protocol
      case catDatas (take eoEmptyTail rest) of
        [] -> pure (take nExpected (catDatas sampled))
        superfluous ->
          let err = "Circuit produced more output than expected:" in
          failWith Nothing (err <> "\n\n" <> ppShow superfluous)
    go timeout n _ | timeout <= 0 =
      failWith Nothing $ concat
        [ "Circuit did not produce enough output. Expected "
        , show n, " more values. Sampled only " <> show (nExpected - n) <> ":\n\n"
        , ppShow (take (nExpected - n) (catDatas sampled)) ]

    go timeout n (a:as) | Maybe.isNothing (mmSlaveInToMaybe a) = do
      -- Circuit did not output valid cycle, just continue
      go (pred timeout) n as
    go _ n (_:as) =
      -- Circuit produced a valid cycle, reset timeout
      go (Maybe.fromMaybe maxBound eoTimeout) (pred n) as

instance (KnownDomain dom,
          GoodMMMasterConfig config,
          Show writeDataType,
          ShowX writeDataType,
          NFData writeDataType,
          NFDataX writeDataType,
          Eq writeDataType,
          Show readDataType,
          ShowX readDataType,
          NFData readDataType,
          NFDataX readDataType,
          Eq readDataType)
          => Test (AvalonMMMaster dom config readDataType writeDataType) where

  expectToLengths Proxy = pure . length

  -- directly copied from Df instance, with minor changes made
  expectN Proxy (ExpectOptions{eoEmptyTail, eoTimeout}) (C.head -> nExpected) sampled = do
    go (Maybe.fromMaybe maxBound eoTimeout) nExpected sampled
   where
    catDatas [] = []
    catDatas (m2s:xs) = Maybe.maybe (catDatas xs) (:catDatas xs) (mmMasterOutToMaybe m2s)

    go _imeout _n [] =
      -- This really should not happen, protocols should produce data indefinitely
      error "unexpected end of signal"
    go _imeout 0 rest = do
      -- Check for superfluous output from protocol
      case catDatas (take eoEmptyTail rest) of
        [] -> pure (take nExpected (catDatas sampled))
        superfluous ->
          let err = "Circuit produced more output than expected:" in
          failWith Nothing (err <> "\n\n" <> ppShow superfluous)
    go timeout n _ | timeout <= 0 =
      failWith Nothing $ concat
        [ "Circuit did not produce enough output. Expected "
        , show n, " more values. Sampled only " <> show (nExpected - n) <> ":\n\n"
        , ppShow (take (nExpected - n) (catDatas sampled)) ]

    go timeout n (a:as) | Maybe.isNothing (mmMasterOutToMaybe a) = do
      -- Circuit did not output valid cycle, just continue
      go (pred timeout) n as
    go _ n (_:as) =
      -- Circuit produced a valid cycle, reset timeout
      go (Maybe.fromMaybe maxBound eoTimeout) (pred n) as
