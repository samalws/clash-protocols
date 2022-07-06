{-|
Types and instance declarations for the Avalon memory mapped protocol
(http://www1.cs.columbia.edu/~sedwards/classes/2009/4840/mnl_avalon_spec.pdf).
Non-required fields can be easily toggled by the user.
The @data@ and @outputenable@ fields are not supported since we would need bidirectional data ports.
The @resetrequest@ field is also not supported since this does not get transferred around,
but rather gets send "outwards" to whoever is controlling the reset signal of the circuit.
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, TypeFamilyDependencies, UndecidableInstances #-}

module Protocols.Avalon.MemMap.AvalonMemMap where

-- base
import           Control.DeepSeq (NFData)
import           Prelude hiding (not, (&&), (||), repeat, (!!), foldl, unzip, head)

import           Control.Arrow ((***))
import qualified Data.Maybe as Maybe
import           Data.Proxy

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


-- @KeepBool@ allows for optional boolean data.
-- Depending on the value of @keep@, the data can either be included or left out.
-- When left out, the data is represented instead as type @()@.
type family KeepBool (keep :: Bool) = t | t -> keep where
  KeepBool 'True = Bool
  KeepBool 'False = ()

-- We want to define operations on @KeepBool@ that work for both possibilities
-- (@keep = 'True@ and @keep = 'False@), but we can't pattern match directly.
-- Instead we need to define a class and instantiate the class for both @'True@ and @'False@.
-- We also provide requirements for @NFDataX@ etc. so we don't have to write these out later.
class (NFDataX (KeepBool keep), NFData (KeepBool keep), Show (KeepBool keep), ShowX (KeepBool keep), Eq (KeepBool keep)) => KeepBoolClass (keep :: Bool) where
  -- Convert an optional bool to a normal bool, given a default value if the field is not included.
  fromKeepBool :: Bool -> KeepBool keep -> Bool
  -- Convert a normal bool to an optional bool. Either preserves the value or returns @()@.
  toKeepBool   :: Bool -> KeepBool keep

instance KeepBoolClass 'True where
  fromKeepBool _ = id
  toKeepBool = id

instance KeepBoolClass 'False where
  fromKeepBool b = const b
  toKeepBool = const ()

-- Convert one optional field to another,
-- keeping the bool value the same if possible.
-- If not possible, a default argument is provided.
convKeepBool :: (KeepBoolClass a, KeepBoolClass b) => Bool -> KeepBool a -> KeepBool b
convKeepBool b = toKeepBool . fromKeepBool b


-- Another class that holds for all possible values.
-- In this case, we want to grab a number value of width @n@ if @n@ is nonzero.
-- Otherwise we provide a default value.
class (KnownNat n) => MaybeZeroNat (n :: Nat) where
  -- Grab a number value of width @n@ if @n@ is nonzero.
  -- Otherwise use a default value.
  -- The output number is size @n+1@ to simplify the definition
  -- and allow for the @n=0@ case to provide a number.
  fromMaybeEmptyNum :: Unsigned (n+1) -> Unsigned n -> Unsigned (n+1)

instance MaybeZeroNat 0 where
  fromMaybeEmptyNum n _ = n

instance (1 <= n, KnownNat n) => MaybeZeroNat n where
  fromMaybeEmptyNum _ m = resize m


-- Class representing @n=m || n=0@.
-- TODO should we write this out directly instead?
--      Is there a type level @||@?
class EqOrZero (n :: Nat) (m :: Nat)
instance EqOrZero 0 m
instance EqOrZero m m


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
  , KeepBoolClass (KeepRead          config)
  , KeepBoolClass (KeepWrite         config)
  , MaybeZeroNat  (ByteEnableWidth   config)
  , MaybeZeroNat  (BurstCountWidth   config)
  , KeepBoolClass (KeepReadDataValid config)
  , KeepBoolClass (KeepEndOfPacket   config)
  , KeepBoolClass (KeepIrq           config)
  ) => GoodMMSharedConfig config
instance
  ( KnownNat      (AddrWidth         config)
  , KeepBoolClass (KeepRead          config)
  , KeepBoolClass (KeepWrite         config)
  , MaybeZeroNat  (ByteEnableWidth   config)
  , MaybeZeroNat  (BurstCountWidth   config)
  , KeepBoolClass (KeepReadDataValid config)
  , KeepBoolClass (KeepEndOfPacket   config)
  , KeepBoolClass (KeepIrq           config)
  ) => GoodMMSharedConfig config

-- Class representing a well-behaved slave config.
-- This class holds for every possible @AvalonMMSlaveConfig@,
-- but we need to write out the class anyway so that GHC holds.
class
  ( MaybeZeroNat  (WriteByteEnableWidth   config)
  , KeepBoolClass (KeepChipSelect         config)
  , KeepBoolClass (KeepBeginTransfer      config)
  , KeepBoolClass (KeepWaitRequest        config)
  , KeepBoolClass (KeepBeginBurstTransfer config)
  , KeepBoolClass (KeepReadyForData       config)
  , KeepBoolClass (KeepDataAvailable      config)
  , GoodMMSharedConfig (SShared           config)
  ) => GoodMMSlaveConfig config
instance
  ( MaybeZeroNat  (WriteByteEnableWidth   config)
  , KeepBoolClass (KeepChipSelect         config)
  , KeepBoolClass (KeepBeginTransfer      config)
  , KeepBoolClass (KeepWaitRequest        config)
  , KeepBoolClass (KeepBeginBurstTransfer config)
  , KeepBoolClass (KeepReadyForData       config)
  , KeepBoolClass (KeepDataAvailable      config)
  , GoodMMSharedConfig (SShared           config)
  ) => GoodMMSlaveConfig config

-- Class representing a well-behaved master config.
-- This class holds for every possible @AvalonMMMasterConfig@,
-- but we need to write out the class anyway so that GHC holds.
class
  ( KeepBoolClass (KeepFlush      config)
  , KnownNat      (IrqListWidth   config)
  , KnownNat      (IrqNumberWidth config)
  , GoodMMSharedConfig (MShared   config)
  ) => GoodMMMasterConfig config
instance
  ( KeepBoolClass (KeepFlush      config)
  , KnownNat      (IrqListWidth   config)
  , KnownNat      (IrqNumberWidth config)
  , GoodMMSharedConfig (MShared   config)
  ) => GoodMMMasterConfig config


-- Data coming out of an Avalon MM master port.
-- All fields are optional and can be toggled using the config.
data AvalonMasterOut config writeDataType
  =  AvalonMasterOut
  { mo_addr        :: Unsigned (AddrWidth       (MShared config))
  , mo_read        :: KeepBool (KeepRead        (MShared config))
  , mo_write       :: KeepBool (KeepWrite       (MShared config))
  , mo_byteEnable  :: Unsigned (ByteEnableWidth (MShared config))
  , mo_burstCount  :: Unsigned (BurstCountWidth (MShared config))
  , mo_flush       :: KeepBool (KeepFlush                config)
  , mo_writeData   :: writeDataType
  }
  deriving (Generic, Bundle)

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
-- Alomst all fields are optional and can be toggled using the config.
-- WaitRequest is mandatory.
data AvalonMasterIn config readDataType
  =  AvalonMasterIn
  { mi_waitRequest   :: Bool
  , mi_readDataValid :: KeepBool (KeepReadDataValid (MShared config))
  , mi_endOfPacket   :: KeepBool (KeepEndOfPacket   (MShared config))
  , mi_irq           :: KeepBool (KeepIrq           (MShared config))
  , mi_irqList       :: Unsigned (IrqListWidth               config)
  , mi_irqNumber     :: Unsigned (IrqNumberWidth             config)
  , mi_readData      :: readDataType
  }
  deriving (Generic, Bundle)

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
  { so_readDataValid :: KeepBool (KeepReadDataValid (SShared config))
  , so_endOfPacket   :: KeepBool (KeepEndOfPacket   (SShared config))
  , so_irq           :: KeepBool (KeepIrq           (SShared config))
  , so_waitRequest   :: KeepBool (KeepWaitRequest            config)
  , so_readyForData  :: KeepBool (KeepReadyForData           config)
  , so_dataAvailable :: KeepBool (KeepDataAvailable          config)
  , so_readData      :: readDataType
  }
  deriving (Generic, Bundle)

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
  , si_read               :: KeepBool (KeepRead               (SShared config))
  , si_write              :: KeepBool (KeepWrite              (SShared config))
  , si_byteEnable         :: Unsigned (ByteEnableWidth        (SShared config))
  , si_burstCount         :: Unsigned (BurstCountWidth        (SShared config))
  , si_writeByteEnable    :: Unsigned (WriteByteEnableWidth            config)
  , si_chipSelect         :: KeepBool (KeepChipSelect                  config)
  , si_beginTransfer      :: KeepBool (KeepBeginTransfer               config)
  , si_beginBurstTransfer :: KeepBool (KeepBeginBurstTransfer          config)
  , si_writeData          :: writeDataType
  }
  deriving (Generic, Bundle)

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
  , EqOrZero (WriteByteEnableWidth slaveConfig)      (ByteEnableWidth (MShared masterConfig))
  , EqOrZero (ByteEnableWidth (SShared slaveConfig)) (ByteEnableWidth (MShared masterConfig))
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
    irqList = fromKeepBool False . so_irq <$> so
    -- set IRQ-related fields of a master-in message using the values calculated above
    setIrq miMsg = miMsg { mi_irq = toKeepBool (Maybe.isJust mirq), mi_irqList = unpack $ resize $ pack irqList, mi_irqNumber = Maybe.fromMaybe 0 mirq }
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

    irqNum soMsg num = if fromKeepBool False (so_irq soMsg) then Just num else Nothing

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
  moIsOn mo = (fromKeepBool True (mo_read mo) || fromKeepBool True (mo_write mo)) && (0 /= fromMaybeEmptyNum 1 (mo_byteEnable mo))
  -- mo wants to read
  moIsRead mo = moIsOn mo && fromKeepBool True (mo_read mo) && not (fromKeepBool False (mo_write mo))
  -- mo wants to write
  moIsWrite mo = moIsOn mo && fromKeepBool True (mo_write mo) && not (fromKeepBool False (mo_read mo))

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
  shouldIncCtr1 mo so = moIsRead mo && not (fromKeepBool False (so_waitRequest so))
  -- decrement ctr2 if (we're writing and waitrequest=false) or readdatavalid=true
  optDecCtr2 mo so ctr2 = if (moIsWrite mo && not (fromKeepBool False (so_waitRequest so)) && ctr2 /= 0) then ctr2-1 else optDecCtr so ctr2
  -- decrement ctr if readdatavalid=true
  optDecCtr so ctr = if (ctr /= 0) && (fromKeepBool True $ so_readDataValid so) then ctr-1 else ctr
  -- always decrement ctr3; loop around to maxBound if mo is sending something
  -- this is for fixed wait state interfaces
  modifyCtr3 mo 0 = if moIsOn mo then maxBound else 0
  modifyCtr3 _ n = n-1
  modifyReadyForTransfer mo so ctr3 readyForTransfer
    | not (fromKeepBool False (so_waitRequest so)) && ctr3 == 0 = True
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
    { mi_waitRequest   = Maybe.maybe True (\(ctr1,_,ctr3,_) -> ctr1 < maxBound && ctr3 == 0) st && (fromKeepBool False (so_waitRequest so))
    , mi_readDataValid = convKeepBool False (so_readDataValid so)
    , mi_endOfPacket   = convKeepBool False (so_endOfPacket so)
    , mi_irq           = errorX "interconnect fabric: this value gets overwritten later"
    , mi_irqList       = errorX "interconnect fabric: this value gets overwritten later"
    , mi_irqNumber     = errorX "interconnect fabric: this value gets overwritten later"
    , mi_readData      = so_readData so
    }

  -- given master-out message and xferSt, generate slave-in message
  convMoSi mo st
    = AvalonSlaveIn
    { si_addr               = mo_addr mo
    , si_read               = toKeepBool $ fromKeepBool True (mo_read  mo) && not (fromKeepBool False (mo_write mo))
    , si_write              = toKeepBool $ fromKeepBool True (mo_write mo) && not (fromKeepBool False (mo_read  mo))
    , si_writeByteEnable    = resize $ if (fromKeepBool True (mo_write mo)) then mo_byteEnable mo else 0
    , si_burstCount         = mo_burstCount mo
    , si_chipSelect         = toKeepBool True
    , si_byteEnable         = resize $ mo_byteEnable mo
    , si_beginTransfer      = toKeepBool $ moIsOn mo && (Maybe.maybe True (\(_,_,_,readyForMsg) -> readyForMsg) st)
    , si_beginBurstTransfer = toKeepBool $ Maybe.isNothing st
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
  , EqOrZero (WriteByteEnableWidth slaveConfig)      (ByteEnableWidth (MShared masterConfig))
  , EqOrZero (ByteEnableWidth (SShared slaveConfig)) (ByteEnableWidth (MShared masterConfig))
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
    { so_waitRequest   = toKeepBool (not ack)
    , so_readDataValid = toKeepBool False
    , so_readyForData  = toKeepBool ack
    , so_dataAvailable = toKeepBool False
    , so_endOfPacket   = toKeepBool False
    , so_irq           = toKeepBool False
    , so_readData      = errorX "No readData for boolToAck"
    }

-- Convert a boolean value to an @AvalonMasterIn@ structure.
-- The structure gives no read data, no IRQ, etc.
-- The @waitRequest@ field is controlled by the (negated) boolean input.
boolToMMMasterAck :: (GoodMMMasterConfig config) => Bool -> AvalonMasterIn config readDataType
boolToMMMasterAck ack
  = AvalonMasterIn
  { mi_waitRequest   = not ack
  , mi_readDataValid = toKeepBool False
  , mi_endOfPacket   = toKeepBool False
  , mi_irq           = toKeepBool False
  , mi_irqList       = 0
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No readData for boolToAck"
  }

-- An @AvalonMasterIn@ containing no read data, but not giving a wait request or an IRQ.
mmMasterInNoData :: (GoodMMMasterConfig config) => AvalonMasterIn config readDataType
mmMasterInNoData
  = AvalonMasterIn
  { mi_waitRequest   = False
  , mi_readDataValid = toKeepBool False
  , mi_endOfPacket   = toKeepBool False
  , mi_irq           = toKeepBool False
  , mi_irqList       = 0
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No read data defined"
  }

-- An @AvalonSlaveIn@ containing no write data, and indicating that no transmission is currently occurring.
mmSlaveInNoData :: (GoodMMSlaveConfig config) => AvalonSlaveIn config writeDataType
mmSlaveInNoData
  = AvalonSlaveIn
  { si_chipSelect         = toKeepBool False
  , si_addr               = 0
  , si_read               = toKeepBool False
  , si_write              = toKeepBool False
  , si_byteEnable         = 0
  , si_writeByteEnable    = 0
  , si_beginTransfer      = toKeepBool False
  , si_burstCount         = 0
  , si_beginBurstTransfer = toKeepBool False
  , si_writeData          = errorX "No writeData for noData"
  }

-- An @AvalonMasterOut@ containing no write data, and indicating that no transmission is currently occurring.
mmMasterOutNoData :: (GoodMMMasterConfig config) => AvalonMasterOut config writeDataType
mmMasterOutNoData
  = AvalonMasterOut
  { mo_addr        = 0
  , mo_read        = toKeepBool False
  , mo_write       = toKeepBool False
  , mo_byteEnable  = 0
  , mo_burstCount  = 0
  , mo_flush       = toKeepBool False
  , mo_writeData   = errorX "No writeData for noData"
  }

-- Grab the "acknowledgement" value from an @AvalonSlaveOut@.
-- Reasonable defaults are provided for optional fields.
mmSlaveOutToBool :: (GoodMMSlaveConfig config) => AvalonSlaveOut config readDataType -> Bool
mmSlaveOutToBool so = fromKeepBool True (so_readyForData so) && not (fromKeepBool False (so_waitRequest so))

-- Grab the "acknowledgement" value from an @AvalonMasterIn@.
-- Reasonable defaults are provided for optional fields.
mmMasterInToBool :: (GoodMMMasterConfig config) => AvalonMasterIn config readDataType -> Bool
mmMasterInToBool = not . mi_waitRequest

-- Default @AvalonSlaveIn@ whose fields indicate that a write transaction is occurring.
-- The @writeData@ field needs to be filled in with data.
mmSlaveInSendingData :: (GoodMMSlaveConfig config) => AvalonSlaveIn config writeDataType
mmSlaveInSendingData
  = AvalonSlaveIn
  { si_chipSelect         = toKeepBool True
  , si_addr               = 0
  , si_read               = toKeepBool False
  , si_write              = toKeepBool True
  , si_byteEnable         = bitCoerce $ repeat True
  , si_writeByteEnable    = bitCoerce $ repeat True
  , si_beginTransfer      = toKeepBool False
  , si_burstCount         = 0
  , si_beginBurstTransfer = toKeepBool False
  , si_writeData          = errorX "No writeData for mmSlaveInSendingData"
  }

-- Default @AvalonMasterOut@ whose fields indicate that a write transaction is occurring.
-- The @writeData@ field needs to be filled in with data.
mmMasterOutSendingData :: (GoodMMMasterConfig config) => AvalonMasterOut config writeDataType
mmMasterOutSendingData
  = AvalonMasterOut
  { mo_addr        = 0
  , mo_read        = toKeepBool False
  , mo_write       = toKeepBool True
  , mo_byteEnable  = bitCoerce $ repeat True
  , mo_burstCount  = 1
  , mo_flush       = toKeepBool False
  , mo_writeData   = errorX "No writeData for mmMasterOutSendingData"
  }


-- Grab the data from an @AvalonSlaveIn@, if there is any.
mmSlaveInToMaybe :: (GoodMMSlaveConfig config) => AvalonSlaveIn config writeDataType -> Maybe writeDataType
mmSlaveInToMaybe si = if cond then Just (si_writeData si) else Nothing where
  cond =  fromKeepBool True (si_chipSelect si)
       && fromKeepBool True (si_write si)
       && not (fromKeepBool False (si_read si))
       && 0 /= fromMaybeEmptyNum 1 (si_byteEnable si)
       && 0 /= fromMaybeEmptyNum 1 (si_writeByteEnable si)

-- Grab the data from an @AvalonMasterOut@, if there is any.
mmMasterOutToMaybe :: (GoodMMMasterConfig config) => AvalonMasterOut config writeDataType -> Maybe writeDataType
mmMasterOutToMaybe mo = if cond then Just (mo_writeData mo) else Nothing where
  cond =  fromKeepBool True (mo_write mo)
       && not (fromKeepBool False (mo_read mo))
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
