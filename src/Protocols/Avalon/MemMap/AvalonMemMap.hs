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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Avalon.MemMap.AvalonMemMap where

-- base
-- import           Prelude hiding (not, (&&), (||), repeat, (!!), foldl, unzip, head)
import qualified Prelude as P

import           Control.Arrow ((***))
import           Control.Monad.State (put, gets)
import           Control.DeepSeq (NFData)
import qualified Data.Maybe as Maybe
import           Data.Proxy
-- import           Data.Type.Ord (OrdCond, Compare)

-- clash-prelude
import           Clash.Prelude hiding (take, concat, length)
import qualified Clash.Prelude as C

-- me
import           Protocols.Internal
import qualified Protocols.DfConv as DfConv
import qualified Protocols.Df as Df
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


-- Config needed for both manager and subordinate interfaces.
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

-- Config specific to Avalon MM subordinate interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
-- An @AvalonMMSharedConfig@ is also included for the rest of the fields.
data AvalonMMSubordinateConfig
  =  AvalonMMSubordinateConfig
  { writeByteEnableWidth   :: Nat
  , keepChipSelect         :: Bool
  , keepBeginTransfer      :: Bool
  , keepWaitRequest        :: Bool
  , keepBeginBurstTransfer :: Bool
  , keepReadyForData       :: Bool
  , keepDataAvailable      :: Bool
  , sShared                :: AvalonMMSharedConfig
  }

-- Config specific to Avalon MM manager interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
-- An @AvalonMMSharedConfig@ is also included for the rest of the fields.
data AvalonMMManagerConfig
  =  AvalonMMManagerConfig
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


type family WriteByteEnableWidth (c :: AvalonMMSubordinateConfig) where
  WriteByteEnableWidth ('AvalonMMSubordinateConfig a _ _ _ _ _ _ _) = a

type family KeepChipSelect (c :: AvalonMMSubordinateConfig) where
  KeepChipSelect ('AvalonMMSubordinateConfig _ a _ _ _ _ _ _) = a

type family KeepBeginTransfer (c :: AvalonMMSubordinateConfig) where
  KeepBeginTransfer ('AvalonMMSubordinateConfig _ _ a _ _ _ _ _) = a

type family KeepWaitRequest (c :: AvalonMMSubordinateConfig) where
  KeepWaitRequest ('AvalonMMSubordinateConfig _ _ _ a _ _ _ _) = a

type family KeepBeginBurstTransfer (c :: AvalonMMSubordinateConfig) where
  KeepBeginBurstTransfer ('AvalonMMSubordinateConfig _ _ _ _ a _ _ _) = a

type family KeepReadyForData (c :: AvalonMMSubordinateConfig) where
  KeepReadyForData ('AvalonMMSubordinateConfig _ _ _ _ _ a _ _) = a

type family KeepDataAvailable (c :: AvalonMMSubordinateConfig) where
  KeepDataAvailable ('AvalonMMSubordinateConfig _ _ _ _ _ _ a _) = a

type family SShared (c :: AvalonMMSubordinateConfig) where
  SShared ('AvalonMMSubordinateConfig _ _ _ _ _ _ _ a) = a


type family KeepFlush (c :: AvalonMMManagerConfig) where
  KeepFlush ('AvalonMMManagerConfig a _ _ _) = a

type family IrqListWidth (c :: AvalonMMManagerConfig) where
  IrqListWidth ('AvalonMMManagerConfig _ a _ _) = a

type family IrqNumberWidth (c :: AvalonMMManagerConfig) where
  IrqNumberWidth ('AvalonMMManagerConfig _ _ a _) = a

type family MShared (c :: AvalonMMManagerConfig) where
  MShared ('AvalonMMManagerConfig _ _ _ a) = a


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

-- Class representing a well-behaved subordinate config.
-- This class holds for every possible @AvalonMMSubordinateConfig@,
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
  ) => GoodMMSubordinateConfig config
instance
  ( MaybeZeroNat  (WriteByteEnableWidth   config)
  , KeepTypeClass (KeepChipSelect         config)
  , KeepTypeClass (KeepBeginTransfer      config)
  , KeepTypeClass (KeepWaitRequest        config)
  , KeepTypeClass (KeepBeginBurstTransfer config)
  , KeepTypeClass (KeepReadyForData       config)
  , KeepTypeClass (KeepDataAvailable      config)
  , GoodMMSharedConfig (SShared           config)
  ) => GoodMMSubordinateConfig config

-- Class representing a well-behaved manager config.
-- This class holds for every possible @AvalonMMManagerConfig@,
-- but we need to write out the class anyway so that GHC holds.
class
  ( KeepTypeClass (KeepFlush      config)
  , KnownNat      (IrqListWidth   config)
  , KnownNat      (IrqNumberWidth config)
  , GoodMMSharedConfig (MShared   config)
  ) => GoodMMManagerConfig config
instance
  ( KeepTypeClass (KeepFlush      config)
  , KnownNat      (IrqListWidth   config)
  , KnownNat      (IrqNumberWidth config)
  , GoodMMSharedConfig (MShared   config)
  ) => GoodMMManagerConfig config


-- Data coming out of an Avalon MM manager port.
-- All fields are optional and can be toggled using the config.
data AvalonManagerOut config writeDataType
  =  AvalonManagerOut
  { mo_addr        :: Unsigned (AddrWidth       (MShared config))
  , mo_read        :: KeepType (KeepRead        (MShared config)) Bool
  , mo_write       :: KeepType (KeepWrite       (MShared config)) Bool
  , mo_byteEnable  :: Unsigned (ByteEnableWidth (MShared config))
  , mo_burstCount  :: Unsigned (BurstCountWidth (MShared config))
  , mo_flush       :: KeepType (KeepFlush                config) Bool
  , mo_writeData   :: writeDataType
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMManagerConfig config,
                   NFDataX writeDataType)
                   => NFDataX (AvalonManagerOut config writeDataType)
deriving instance (GoodMMManagerConfig config,
                   NFData writeDataType)
                   => NFData (AvalonManagerOut config writeDataType)
deriving instance (GoodMMManagerConfig config,
                   ShowX writeDataType)
                   => ShowX (AvalonManagerOut config writeDataType)
deriving instance (GoodMMManagerConfig config,
                   Show writeDataType)
                   => Show (AvalonManagerOut config writeDataType)
deriving instance (GoodMMManagerConfig config,
                   Eq writeDataType)
                   => Eq (AvalonManagerOut config writeDataType)


-- Data coming into an Avalon MM manager port.
-- Almost all fields are optional and can be toggled using the config.
-- WaitRequest is mandatory.
data AvalonManagerIn config readDataType
  =  AvalonManagerIn
  { mi_waitRequest   :: Bool
  , mi_readDataValid :: KeepType (KeepReadDataValid (MShared config)) Bool
  , mi_endOfPacket   :: KeepType (KeepEndOfPacket   (MShared config)) Bool
  , mi_irq           :: KeepType (KeepIrq           (MShared config)) Bool
  , mi_irqList       :: Unsigned (IrqListWidth               config)
  , mi_irqNumber     :: Unsigned (IrqNumberWidth             config)
  , mi_readData      :: readDataType
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMManagerConfig config,
                   NFDataX readDataType)
                   => NFDataX (AvalonManagerIn config readDataType)
deriving instance (GoodMMManagerConfig config,
                   NFData readDataType)
                   => NFData (AvalonManagerIn config readDataType)
deriving instance (GoodMMManagerConfig config,
                   ShowX readDataType)
                   => ShowX (AvalonManagerIn config readDataType)
deriving instance (GoodMMManagerConfig config,
                   Show readDataType)
                   => Show (AvalonManagerIn config readDataType)
deriving instance (GoodMMManagerConfig config,
                   Eq readDataType)
                   => Eq (AvalonManagerIn config readDataType)


-- TODO
data AvalonManagerWriteImpt config writeDataType
  =  AvalonManagerWriteImpt
  { mwi_addr        :: Unsigned (AddrWidth       (MShared config))
  , mwi_byteEnable  :: Unsigned (ByteEnableWidth (MShared config))
  , mwi_burstCount  :: Unsigned (BurstCountWidth (MShared config))
  , mwi_flush       :: KeepType (KeepFlush                config) Bool
  , mwi_writeData   :: writeDataType
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMManagerConfig config,
                   NFDataX writeDataType)
                   => NFDataX (AvalonManagerWriteImpt config writeDataType)
deriving instance (GoodMMManagerConfig config,
                   NFData writeDataType)
                   => NFData (AvalonManagerWriteImpt config writeDataType)
deriving instance (GoodMMManagerConfig config,
                   ShowX writeDataType)
                   => ShowX (AvalonManagerWriteImpt config writeDataType)
deriving instance (GoodMMManagerConfig config,
                   Show writeDataType)
                   => Show (AvalonManagerWriteImpt config writeDataType)
deriving instance (GoodMMManagerConfig config,
                   Eq writeDataType)
                   => Eq (AvalonManagerWriteImpt config writeDataType)


-- TODO
data AvalonManagerReadReqImpt config
  =  AvalonManagerReadReqImpt
  { mrri_addr        :: Unsigned (AddrWidth       (MShared config))
  , mrri_byteEnable  :: Unsigned (ByteEnableWidth (MShared config))
  , mrri_burstCount  :: Unsigned (BurstCountWidth (MShared config))
  , mrri_flush       :: KeepType (KeepFlush                config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMManagerConfig config)
                   => NFDataX (AvalonManagerReadReqImpt config)
deriving instance (GoodMMManagerConfig config)
                   => NFData (AvalonManagerReadReqImpt config)
deriving instance (GoodMMManagerConfig config)
                   => ShowX (AvalonManagerReadReqImpt config)
deriving instance (GoodMMManagerConfig config)
                   => Show (AvalonManagerReadReqImpt config)
deriving instance (GoodMMManagerConfig config)
                   => Eq (AvalonManagerReadReqImpt config)


-- TODO
data AvalonManagerReadImpt config readDataType
  =  AvalonManagerReadImpt
  { mri_endOfPacket   :: KeepType (KeepEndOfPacket   (MShared config)) Bool
  , mri_readData      :: readDataType
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMManagerConfig config,
                   NFDataX readDataType)
                   => NFDataX (AvalonManagerReadImpt config readDataType)
deriving instance (GoodMMManagerConfig config,
                   NFData readDataType)
                   => NFData (AvalonManagerReadImpt config readDataType)
deriving instance (GoodMMManagerConfig config,
                   ShowX readDataType)
                   => ShowX (AvalonManagerReadImpt config readDataType)
deriving instance (GoodMMManagerConfig config,
                   Show readDataType)
                   => Show (AvalonManagerReadImpt config readDataType)
deriving instance (GoodMMManagerConfig config,
                   Eq readDataType)
                   => Eq (AvalonManagerReadImpt config readDataType)


-- Data coming out of an Avalon MM subordinate port.
-- All fields are optional and can be toggled using the config.
data AvalonSubordinateOut config readDataType
  =  AvalonSubordinateOut
  { so_readDataValid :: KeepType (KeepReadDataValid (SShared config)) Bool
  , so_endOfPacket   :: KeepType (KeepEndOfPacket   (SShared config)) Bool
  , so_irq           :: KeepType (KeepIrq           (SShared config)) Bool
  , so_waitRequest   :: KeepType (KeepWaitRequest            config) Bool
  , so_readyForData  :: KeepType (KeepReadyForData           config) Bool
  , so_dataAvailable :: KeepType (KeepDataAvailable          config) Bool
  , so_readData      :: readDataType
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSubordinateConfig config,
                   NFDataX readDataType)
                   => NFDataX (AvalonSubordinateOut config readDataType)
deriving instance (GoodMMSubordinateConfig config,
                   NFData readDataType)
                   => NFData (AvalonSubordinateOut config readDataType)
deriving instance (GoodMMSubordinateConfig config,
                   ShowX readDataType)
                   => ShowX (AvalonSubordinateOut config readDataType)
deriving instance (GoodMMSubordinateConfig config,
                   Show readDataType)
                   => Show (AvalonSubordinateOut config readDataType)
deriving instance (GoodMMSubordinateConfig config,
                   Eq readDataType)
                   => Eq (AvalonSubordinateOut config readDataType)


-- Data coming into an Avalon MM subordinate port.
-- All fields are optional and can be toggled using the config.
data AvalonSubordinateIn config writeDataType
  = AvalonSubordinateIn
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
  deriving (Generic, Bundle)

deriving instance (GoodMMSubordinateConfig config,
                   NFDataX writeDataType)
                   => NFDataX (AvalonSubordinateIn config writeDataType)
deriving instance (GoodMMSubordinateConfig config,
                   NFData writeDataType)
                   => NFData (AvalonSubordinateIn config writeDataType)
deriving instance (GoodMMSubordinateConfig config,
                   ShowX writeDataType)
                   => ShowX (AvalonSubordinateIn config writeDataType)
deriving instance (GoodMMSubordinateConfig config,
                   Show writeDataType)
                   => Show (AvalonSubordinateIn config writeDataType)
deriving instance (GoodMMSubordinateConfig config,
                   Eq writeDataType)
                   => Eq (AvalonSubordinateIn config writeDataType)


-- TODO
data AvalonSubordinateWriteImpt config writeDataType
  = AvalonSubordinateWriteImpt
  { swi_addr               :: Unsigned (AddrWidth              (SShared config))
  , swi_byteEnable         :: Unsigned (ByteEnableWidth        (SShared config))
  , swi_burstCount         :: Unsigned (BurstCountWidth        (SShared config))
  , swi_beginTransfer      :: KeepType (KeepBeginTransfer               config) Bool
  , swi_beginBurstTransfer :: KeepType (KeepBeginBurstTransfer          config) Bool
  , swi_writeData          :: writeDataType
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSubordinateConfig config,
                   NFDataX writeDataType)
                   => NFDataX (AvalonSubordinateWriteImpt config writeDataType)
deriving instance (GoodMMSubordinateConfig config,
                   NFData writeDataType)
                   => NFData (AvalonSubordinateWriteImpt config writeDataType)
deriving instance (GoodMMSubordinateConfig config,
                   ShowX writeDataType)
                   => ShowX (AvalonSubordinateWriteImpt config writeDataType)
deriving instance (GoodMMSubordinateConfig config,
                   Show writeDataType)
                   => Show (AvalonSubordinateWriteImpt config writeDataType)
deriving instance (GoodMMSubordinateConfig config,
                   Eq writeDataType)
                   => Eq (AvalonSubordinateWriteImpt config writeDataType)


-- TODO
data AvalonSubordinateReadReqImpt config
  = AvalonSubordinateReadReqImpt
  { srri_addr               :: Unsigned (AddrWidth              (SShared config))
  , srri_byteEnable         :: Unsigned (ByteEnableWidth        (SShared config))
  , srri_burstCount         :: Unsigned (BurstCountWidth        (SShared config))
  , srri_beginTransfer      :: KeepType (KeepBeginTransfer               config) Bool
  , srri_beginBurstTransfer :: KeepType (KeepBeginBurstTransfer          config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSubordinateConfig config)
                   => NFDataX (AvalonSubordinateReadReqImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => NFData (AvalonSubordinateReadReqImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => ShowX (AvalonSubordinateReadReqImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => Show (AvalonSubordinateReadReqImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => Eq (AvalonSubordinateReadReqImpt config)


-- TODO irq? readyForData? dataAvailable?
data AvalonSubordinateReadImpt config readDataType
  = AvalonSubordinateReadImpt
  { sri_endOfPacket   :: KeepType (KeepEndOfPacket   (SShared config)) Bool
  , sri_readData      :: readDataType
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSubordinateConfig config,
                   NFDataX readDataType)
                   => NFDataX (AvalonSubordinateReadImpt config readDataType)
deriving instance (GoodMMSubordinateConfig config,
                   NFData readDataType)
                   => NFData (AvalonSubordinateReadImpt config readDataType)
deriving instance (GoodMMSubordinateConfig config,
                   ShowX readDataType)
                   => ShowX (AvalonSubordinateReadImpt config readDataType)
deriving instance (GoodMMSubordinateConfig config,
                   Show readDataType)
                   => Show (AvalonSubordinateReadImpt config readDataType)
deriving instance (GoodMMSubordinateConfig config,
                   Eq readDataType)
                   => Eq (AvalonSubordinateReadImpt config readDataType)

-- Interconnect fabric, which can be used to tie together multiple managers and subordinates.
-- managers and subordinates cannot contact each other directly; this fabric is needed in order to mediate,
--   since managers and subordinates do not have the same data fields.
-- Parameters:
-- * subordinateAddrFns: functions indicating whether a given address refers to a subordinate
-- * irqNums: IRQ numbers for each subordinate
-- * fixedWaitTime: SNat representing the length of the fixed wait-state (0 if there is none) TODO this ought to be part of config
-- TODO support (subordinateA,subordinateB) where a and b have different config
avalonInterconnectFabric ::
  ( HiddenClockResetEnable dom
  , KnownNat fixedWaitTime
  , KnownNat nmanager
  , KnownNat nSubordinate
  , nSubordinate ~ (decNSubordinate + 1)
  , GoodMMManagerConfig managerConfig
  , GoodMMSubordinateConfig  subordinateConfig
  , AddrWidth (MShared managerConfig) ~ AddrWidth (SShared subordinateConfig)
  , EqOrZero (WriteByteEnableWidth subordinateConfig)      (ByteEnableWidth (MShared managerConfig)) ~ 'True
  , EqOrZero (ByteEnableWidth (SShared subordinateConfig)) (ByteEnableWidth (MShared managerConfig)) ~ 'True
  , BurstCountWidth (MShared managerConfig) ~ BurstCountWidth (SShared subordinateConfig)
  )
  => Vec nSubordinate (Unsigned (AddrWidth (SShared subordinateConfig)) -> Bool)
  -> Vec nSubordinate (Unsigned (IrqNumberWidth managerConfig))
  -> SNat fixedWaitTime
  -> Circuit
     (Vec nmanager (AvalonMMManager dom               managerConfig readDataType writeDataType))
     (Vec nSubordinate  (AvalonMMSubordinate  dom fixedWaitTime subordinateConfig  readDataType writeDataType))
avalonInterconnectFabric subordinateAddrFns irqNums fixedWaitTime = Circuit cktFn where
  -- We use a mealy machine, since state is necessary to keep track of which manager is connected to which subordinate.
  cktFn (inpA, inpB) = (unbundle otpA, unbundle otpB) where (otpA, otpB) = unbundle $ mealy transFn s0 $ bundle (bundle inpA, bundle inpB)

  -- (sm: which subordinate was connected to which manager last clock cycle, xferSt: state for each manager-to-subordinate connection)
  -- xferSt is indexed by subordinate
  -- xferSt: Vec (Maybe (ctr1: num waitrequest=false&read=true - num readdatavalid=true, ctr2: xfers left in burst (dec on readdatavalid=true OR waitrequest=false&write=true), ctr3: fixed wait time left (dec always, loop around on good message), ready for transfer (becomes True on waitrequest=false and ctr3=0, False on good message)))
  -- xferState[subordinate] = Nothing indicates that subordinate is not connected to any manager
  s0 = (repeat Nothing, repeat Nothing)

  -- transition function, called every clock cycle
  -- takes in old state and input, returns new state and output
  transFn (smOld, xferSt) (mo, so) = ((sm, xferSt'), (mi, si)) where
    -- figure out which subordinate gets paired with which manager, and vice versa
    (ms, sm) = managerSubordinatePairings mo smOld xferSt
    -- get the interrupt request number
    mirq = minIrq so
    -- get the interrupt list (n subordinates produce n bools; then resize, padding with zeros)
    irqList = fromKeepTypeDef False . so_irq <$> so
    -- set IRQ-related fields of a manager-in message using the values calculated above
    setIrq miMsg = miMsg { mi_irq = toKeepType (Maybe.isJust mirq), mi_irqList = unpack $ resize $ pack irqList, mi_irqNumber = Maybe.fromMaybe 0 mirq }
    -- calculate all manager-in messages
    mi = setIrq . maybe mmManagerInNoData (\n -> convSoMi (so !! n) (xferSt !! n)) <$> ms
    -- calculate all subordinate-in messages
    si = maybe (const mmSubordinateInNoData) (\n -> convMoSi (mo !! n)) <$> sm <*> xferSt
    -- calculate the next xferStates
    xferSt' = modifySt <$> (fmap (mo !!) <$> sm) <*> so <*> xferSt

  -- out of all subordinates with IRQ turned on, return the smallest IRQ number
  minIrq so = fold minJust $ irqNum <$> so <*> irqNums where
    minJust (Just a) (Just b) | a < b = Just a
    minJust (Just a) Nothing = Just a
    minJust _ b = b

    irqNum soMsg num = if fromKeepTypeDef False (so_irq soMsg) then Just num else Nothing

  -- figure out which manager is paired with which subordinate (ms) and vice versa (sm)
  -- given current manager-out messages; previous sm value; and all the xferStates
  managerSubordinatePairings mo smOld xferSt = (ms, sm) where
    -- for old sm values, determine if they're still transmitting
    smOld' = (\smOldElem addrFn xferStI -> smOldElem >>= keepSM addrFn xferStI) <$> smOld <*> subordinateAddrFns <*> xferSt
    -- a transmission is still going if the xferState is Just or if the manager is still asking to connect
    keepSM addrFn xferStI idx = if (moGood addrFn (mo !! idx)) || Maybe.isJust xferStI then Just idx else Nothing
    -- get new subordinate-to-manager connections in case a subordinate is disconnectd and a manager wants to connect to it
    smCurr = (\addrFn -> findIndex (moGood addrFn) mo) <$> subordinateAddrFns
    -- given addrFn, does manager-out message want to connect to this address?
    moGood addrFn moMsg = moIsOn moMsg && addrFn (mo_addr moMsg)
    -- make subordinate-to-manager pairings, preferring existing connections
    sm = (<|>) <$> smOld' <*> smCurr
    -- figure out manager-to-subordinate pairings based on sm
    ms = flip elemIndex sm . Just <$> iterateI (+ 1) 0

  -- mo wants to read or write
  moIsOn mo = (fromKeepTypeDef True (mo_read mo) || fromKeepTypeDef True (mo_write mo)) && (0 /= fromMaybeEmptyNum 1 (mo_byteEnable mo))
  -- mo wants to read
  moIsRead mo = moIsOn mo && fromKeepTypeDef True (mo_read mo) && not (fromKeepTypeDef False (mo_write mo))
  -- mo wants to write
  moIsWrite mo = moIsOn mo && fromKeepTypeDef True (mo_write mo) && not (fromKeepTypeDef False (mo_read mo))

  -- modify one xferSt value, given one manager-out message and one subordinate-out message
  -- if there is no manager connected, our state should be Nothing
  modifySt Nothing _ _ = Nothing
  -- if there is a manager connected, give a default value of xferSt if needed, and then call on modifySt' to modify it
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

  -- given subordinate-out message and xferSt, generate manager-in message
  convSoMi so st
    = AvalonManagerIn
    { mi_waitRequest   = Maybe.maybe True (\(ctr1,_,ctr3,_) -> ctr1 < maxBound && ctr3 == 0) st && (fromKeepTypeDef False (so_waitRequest so))
    , mi_readDataValid = convKeepType False (so_readDataValid so)
    , mi_endOfPacket   = convKeepType False (so_endOfPacket so)
    , mi_irq           = errorX "interconnect fabric: this value gets overwritten later"
    , mi_irqList       = errorX "interconnect fabric: this value gets overwritten later"
    , mi_irqNumber     = errorX "interconnect fabric: this value gets overwritten later"
    , mi_readData      = so_readData so
    }

  -- given manager-out message and xferSt, generate subordinate-in message
  convMoSi mo st
    = AvalonSubordinateIn
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

-- Interconnect fabric, but there's only one manager and one subordinate.
-- Vecs are removed for convenience.
avalonInterconnectFabricSingleMember ::
  ( HiddenClockResetEnable dom
  , KnownNat fixedWaitTime
  , GoodMMManagerConfig managerConfig
  , GoodMMSubordinateConfig  subordinateConfig
  , AddrWidth (MShared managerConfig) ~ AddrWidth (SShared subordinateConfig)
  , EqOrZero (WriteByteEnableWidth subordinateConfig)      (ByteEnableWidth (MShared managerConfig)) ~ 'True
  , EqOrZero (ByteEnableWidth (SShared subordinateConfig)) (ByteEnableWidth (MShared managerConfig)) ~ 'True
  , BurstCountWidth (MShared managerConfig) ~ BurstCountWidth (SShared subordinateConfig)
  )
  => (Unsigned (AddrWidth (SShared subordinateConfig)) -> Bool)
  -> Unsigned (IrqNumberWidth managerConfig)
  -> SNat fixedWaitTime
  -> Circuit
     (AvalonMMManager dom               managerConfig readDataType writeDataType)
     (AvalonMMSubordinate  dom fixedWaitTime subordinateConfig  readDataType writeDataType)
avalonInterconnectFabricSingleMember subordinateAddrFn irqNum fixedWaitTime
  = Circuit ((head *** head) . toSignals (avalonInterconnectFabric (singleton subordinateAddrFn) (singleton irqNum) fixedWaitTime) . (singleton *** singleton))


-- Convert a boolean value to an @AvalonSubordinateOut@ structure.
-- The structure gives no read data, no IRQ, etc.
-- Fields relating to "acknowledging" a write are controlled by the bool input.
boolToMMSubordinateAck :: (GoodMMSubordinateConfig config) => Bool -> AvalonSubordinateOut config readDataType
boolToMMSubordinateAck ack
  = AvalonSubordinateOut
    { so_waitRequest   = toKeepType (not ack)
    , so_readDataValid = toKeepType False
    , so_readyForData  = toKeepType ack
    , so_dataAvailable = toKeepType False
    , so_endOfPacket   = toKeepType False
    , so_irq           = toKeepType False
    , so_readData      = errorX "No readData for boolToAck"
    }

-- TODO
mmSubordinateReadDat :: (GoodMMSubordinateConfig config) => AvalonSubordinateReadImpt config readDataType -> AvalonSubordinateOut config readDataType
mmSubordinateReadDat dat
  = AvalonSubordinateOut
    { so_waitRequest   = toKeepType False
    , so_readDataValid = toKeepType True
    , so_readyForData  = toKeepType False
    , so_dataAvailable = toKeepType False
    , so_endOfPacket   = sri_endOfPacket dat
    , so_irq           = toKeepType False
    , so_readData      = sri_readData dat
    }

-- TODO
mmSubordinateOutToReadData :: (GoodMMSubordinateConfig config) => AvalonSubordinateOut config readDataType -> Maybe readDataType
mmSubordinateOutToReadData so
  = if (fromKeepTypeDef True (so_readDataValid so) && not (fromKeepTypeDef False (so_waitRequest so))) then Just (so_readData so) else Nothing

mmSubordinateOutToReadImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateOut config readDataType -> Maybe (AvalonSubordinateReadImpt config readDataType)
mmSubordinateOutToReadImpt (AvalonSubordinateOut{..})
  = if cond then Just AvalonSubordinateReadImpt
  { sri_endOfPacket = so_endOfPacket
  , sri_readData    = so_readData
  } else Nothing
  where
  cond = fromKeepTypeDef True so_readDataValid && not (fromKeepTypeDef False so_waitRequest)

mmManagerInToReadImpt :: (GoodMMManagerConfig config) => AvalonManagerIn config readDataType -> Maybe (AvalonManagerReadImpt config readDataType)
mmManagerInToReadImpt (AvalonManagerIn{..})
  = if cond then Just AvalonManagerReadImpt
  { mri_endOfPacket = mi_endOfPacket
  , mri_readData    = mi_readData
  } else Nothing
  where
  cond = not mi_waitRequest -- TODO anything else?

mmSubordinateInToWriteImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config writeDataType -> Maybe (AvalonSubordinateWriteImpt config writeDataType)
mmSubordinateInToWriteImpt (AvalonSubordinateIn{..})
  = if cond then Just AvalonSubordinateWriteImpt
  { swi_addr               = si_addr
  , swi_byteEnable         = si_byteEnable
  , swi_beginTransfer      = si_beginTransfer
  , swi_burstCount         = si_burstCount
  , swi_beginBurstTransfer = si_beginBurstTransfer
  , swi_writeData          = si_writeData
  } else Nothing
  where
  cond =  fromKeepTypeDef True si_chipSelect
       && fromKeepTypeDef True si_write
       && not (fromKeepTypeDef False si_read)
       && 0 /= fromMaybeEmptyNum 1 si_byteEnable
       && 0 /= fromMaybeEmptyNum 1 si_writeByteEnable

mmSubordinateInToReadReqImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config writeDataType -> Maybe (AvalonSubordinateReadReqImpt config)
mmSubordinateInToReadReqImpt (AvalonSubordinateIn{..})
  = if cond then Just AvalonSubordinateReadReqImpt
  { srri_addr               = si_addr
  , srri_byteEnable         = si_byteEnable
  , srri_beginTransfer      = si_beginTransfer
  , srri_burstCount         = si_burstCount
  , srri_beginBurstTransfer = si_beginBurstTransfer
  } else Nothing
  where
  cond =  fromKeepTypeDef True si_chipSelect
       && fromKeepTypeDef True si_read
       && not (fromKeepTypeDef False si_write)

mmManagerOutToWriteImpt :: (GoodMMManagerConfig config) => AvalonManagerOut config writeDataType -> Maybe (AvalonManagerWriteImpt config writeDataType)
mmManagerOutToWriteImpt (AvalonManagerOut{..})
  = if cond then Just AvalonManagerWriteImpt
  { mwi_addr       = mo_addr
  , mwi_byteEnable = mo_byteEnable
  , mwi_burstCount = mo_burstCount
  , mwi_flush      = mo_flush
  , mwi_writeData  = mo_writeData
  } else Nothing
  where
  cond =  fromKeepTypeDef True mo_write
       && not (fromKeepTypeDef False mo_read)
       && 0 /= fromMaybeEmptyNum 1 mo_byteEnable

mmManagerOutToReadReqImpt :: (GoodMMManagerConfig config) => AvalonManagerOut config writeDataType -> Maybe (AvalonManagerReadReqImpt config)
mmManagerOutToReadReqImpt (AvalonManagerOut{..})
  = if cond then Just AvalonManagerReadReqImpt
  { mrri_addr       = mo_addr
  , mrri_byteEnable = mo_byteEnable
  , mrri_burstCount = mo_burstCount
  , mrri_flush      = mo_flush
  } else Nothing
  where
  cond =  fromKeepTypeDef True mo_read
       && not (fromKeepTypeDef False mo_write)
       && 0 /= fromMaybeEmptyNum 1 mo_byteEnable

-- TODO comment
mmWriteImptToSubordinateIn :: (GoodMMSubordinateConfig config) => AvalonSubordinateWriteImpt config writeDataType -> AvalonSubordinateIn config writeDataType
mmWriteImptToSubordinateIn (AvalonSubordinateWriteImpt{..})
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType True
  , si_addr               = swi_addr
  , si_read               = toKeepType False
  , si_write              = toKeepType True
  , si_byteEnable         = swi_byteEnable
  , si_writeByteEnable    = bitCoerce $ repeat True
  , si_beginTransfer      = swi_beginTransfer
  , si_burstCount         = swi_burstCount
  , si_beginBurstTransfer = swi_beginBurstTransfer
  , si_writeData          = swi_writeData
  }

-- TODO comment
mmReadReqImptToSubordinateIn :: (GoodMMSubordinateConfig config) => AvalonSubordinateReadReqImpt config -> AvalonSubordinateIn config writeDataType
mmReadReqImptToSubordinateIn (AvalonSubordinateReadReqImpt{..})
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType False
  , si_addr               = srri_addr
  , si_read               = toKeepType True
  , si_write              = toKeepType False
  , si_byteEnable         = srri_byteEnable
  , si_writeByteEnable    = 0
  , si_beginTransfer      = srri_beginTransfer
  , si_burstCount         = srri_burstCount
  , si_beginBurstTransfer = srri_beginBurstTransfer
  , si_writeData          = errorX "No writeData for read req"
  }

-- TODO comment
mmWriteImptToManagerOut :: (GoodMMManagerConfig config) => AvalonManagerWriteImpt config writeDataType -> AvalonManagerOut config writeDataType
mmWriteImptToManagerOut (AvalonManagerWriteImpt{..})
  = AvalonManagerOut
  { mo_addr        = mwi_addr
  , mo_read        = toKeepType False
  , mo_write       = toKeepType True
  , mo_byteEnable  = mwi_byteEnable
  , mo_burstCount  = mwi_burstCount
  , mo_flush       = mwi_flush
  , mo_writeData   = mwi_writeData
  }

-- TODO comment
mmReadReqImptToManagerOut :: (GoodMMManagerConfig config) => AvalonManagerReadReqImpt config -> AvalonManagerOut config writeDataType
mmReadReqImptToManagerOut (AvalonManagerReadReqImpt{..})
  = AvalonManagerOut
  { mo_addr        = mrri_addr
  , mo_read        = toKeepType True
  , mo_write       = toKeepType False
  , mo_byteEnable  = mrri_byteEnable
  , mo_burstCount  = mrri_burstCount
  , mo_flush       = mrri_flush
  , mo_writeData   = errorX "No writeData for read req"
  }

-- Convert a boolean value to an @AvalonManagerIn@ structure.
-- The structure gives no read data, no IRQ, etc.
-- The @waitRequest@ field is controlled by the (negated) boolean input.
boolToMMManagerAck :: (GoodMMManagerConfig config) => Bool -> AvalonManagerIn config readDataType
boolToMMManagerAck ack
  = AvalonManagerIn
  { mi_waitRequest   = not ack
  , mi_readDataValid = toKeepType False
  , mi_endOfPacket   = toKeepType False
  , mi_irq           = toKeepType False
  , mi_irqList       = 0
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No readData for boolToAck"
  }

-- An @AvalonManagerIn@ containing no read data, but not giving a wait request or an IRQ.
mmManagerInNoData :: (GoodMMManagerConfig config) => AvalonManagerIn config readDataType
mmManagerInNoData
  = AvalonManagerIn
  { mi_waitRequest   = False
  , mi_readDataValid = toKeepType False
  , mi_endOfPacket   = toKeepType False
  , mi_irq           = toKeepType False
  , mi_irqList       = 0
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No read data defined"
  }

-- An @AvalonManagerIn@ TODO
mmManagerReadDat :: (GoodMMManagerConfig config) => AvalonManagerReadImpt config readDataType -> AvalonManagerIn config readDataType
mmManagerReadDat dat
  = AvalonManagerIn
  { mi_waitRequest   = False
  , mi_readDataValid = toKeepType True
  , mi_endOfPacket   = mri_endOfPacket dat
  , mi_irq           = toKeepType False
  , mi_irqList       = 0
  , mi_irqNumber     = 0
  , mi_readData      = mri_readData dat
  }

-- TODO
mmManagerInToReadData :: (GoodMMManagerConfig config) => AvalonManagerIn config readDataType -> Maybe readDataType
mmManagerInToReadData mi
  = if (fromKeepTypeDef True (mi_readDataValid mi) && not (mi_waitRequest mi)) then Just (mi_readData mi) else Nothing

-- An @AvalonSubordinateIn@ containing no write data, and indicating that no transmission is currently occurring.
mmSubordinateInNoData :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config writeDataType
mmSubordinateInNoData
  = AvalonSubordinateIn
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

-- An @AvalonSubordinateIn@ TODO
mmSubordinateInReadingData :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config writeDataType
mmSubordinateInReadingData
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType False
  , si_addr               = 0
  , si_read               = toKeepType True
  , si_write              = toKeepType False
  , si_byteEnable         = 0
  , si_writeByteEnable    = 0
  , si_beginTransfer      = toKeepType False
  , si_burstCount         = 0
  , si_beginBurstTransfer = toKeepType False
  , si_writeData          = errorX "No writeData for noData"
  }

-- An @AvalonManagerOut@ containing no write data, and indicating that no transmission is currently occurring.
mmManagerOutNoData :: (GoodMMManagerConfig config) => AvalonManagerOut config writeDataType
mmManagerOutNoData
  = AvalonManagerOut
  { mo_addr        = 0
  , mo_read        = toKeepType False
  , mo_write       = toKeepType False
  , mo_byteEnable  = 0
  , mo_burstCount  = 0
  , mo_flush       = toKeepType False
  , mo_writeData   = errorX "No writeData for noData"
  }

-- An @AvalonManagerOut@ TODO
mmManagerOutReadingData :: (GoodMMManagerConfig config) => AvalonManagerOut config writeDataType
mmManagerOutReadingData
  = AvalonManagerOut
  { mo_addr        = 0
  , mo_read        = toKeepType True
  , mo_write       = toKeepType False
  , mo_byteEnable  = 0
  , mo_burstCount  = 0
  , mo_flush       = toKeepType False
  , mo_writeData   = errorX "No writeData for noData"
  }

-- Grab the "acknowledgement" value from an @AvalonSubordinateOut@.
-- Reasonable defaults are provided for optional fields.
mmSubordinateOutToBool :: (GoodMMSubordinateConfig config) => AvalonSubordinateOut config readDataType -> Bool
mmSubordinateOutToBool so = fromKeepTypeDef True (so_readyForData so) && not (fromKeepTypeDef False (so_waitRequest so))

-- Grab the "acknowledgement" value from an @AvalonManagerIn@.
-- Reasonable defaults are provided for optional fields.
mmManagerInToBool :: (GoodMMManagerConfig config) => AvalonManagerIn config readDataType -> Bool
mmManagerInToBool = not . mi_waitRequest

-- Default @AvalonSubordinateIn@ whose fields indicate that a write transaction is occurring.
-- The @writeData@ field needs to be filled in with data.
mmSubordinateInSendingData :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config writeDataType
mmSubordinateInSendingData
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType True
  , si_addr               = 0
  , si_read               = toKeepType False
  , si_write              = toKeepType True
  , si_byteEnable         = bitCoerce $ repeat True
  , si_writeByteEnable    = bitCoerce $ repeat True
  , si_beginTransfer      = toKeepType False
  , si_burstCount         = 0
  , si_beginBurstTransfer = toKeepType False
  , si_writeData          = errorX "No writeData for mmSubordinateInSendingData"
  }

-- Default @AvalonManagerOut@ whose fields indicate that a write transaction is occurring.
-- The @writeData@ field needs to be filled in with data.
mmManagerOutSendingData :: (GoodMMManagerConfig config) => AvalonManagerOut config writeDataType
mmManagerOutSendingData
  = AvalonManagerOut
  { mo_addr        = 0
  , mo_read        = toKeepType False
  , mo_write       = toKeepType True
  , mo_byteEnable  = bitCoerce $ repeat True
  , mo_burstCount  = 1
  , mo_flush       = toKeepType False
  , mo_writeData   = errorX "No writeData for mmManagerOutSendingData"
  }


-- Grab the data from an @AvalonSubordinateIn@, if there is any.
mmSubordinateInToMaybe :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config writeDataType -> Maybe writeDataType
mmSubordinateInToMaybe si = if cond then Just (si_writeData si) else Nothing where
  cond =  fromKeepTypeDef True (si_chipSelect si)
       && fromKeepTypeDef True (si_write si)
       && not (fromKeepTypeDef False (si_read si))
       && 0 /= fromMaybeEmptyNum 1 (si_byteEnable si)
       && 0 /= fromMaybeEmptyNum 1 (si_writeByteEnable si)

-- Grab the data from an @AvalonManagerOut@, if there is any.
mmManagerOutToMaybe :: (GoodMMManagerConfig config) => AvalonManagerOut config writeDataType -> Maybe writeDataType
mmManagerOutToMaybe mo = if cond then Just (mo_writeData mo) else Nothing where
  cond =  fromKeepTypeDef True (mo_write mo)
       && not (fromKeepTypeDef False (mo_read mo))
       && 0 /= fromMaybeEmptyNum 1 (mo_byteEnable mo)

-- TODO support fixed wait time in instances below

-- Datatype for the manager end of the Avalon memory-mapped protocol.
data AvalonMMManager (dom :: Domain) (config :: AvalonMMManagerConfig) (readDataType :: Type) (writeDataType :: Type) = AvalonMMManager

-- Datatype for the subordinate end of the Avalon memory-mapped protocol.
data AvalonMMSubordinate (dom :: Domain) (fixedWaitTime :: Nat) (config :: AvalonMMSubordinateConfig) (readDataType :: Type) (writeDataType :: Type) = AvalonMMSubordinate

instance Protocol (AvalonMMManager dom config readDataType writeDataType) where
  type Fwd (AvalonMMManager dom config readDataType writeDataType) = Signal dom (AvalonManagerOut config writeDataType)
  type Bwd (AvalonMMManager dom config readDataType writeDataType) = Signal dom (AvalonManagerIn  config readDataType)

instance Protocol (AvalonMMSubordinate dom fixedWaitTime config readDataType writeDataType) where
  type Fwd (AvalonMMSubordinate dom fixedWaitTime config readDataType writeDataType) = Signal dom (AvalonSubordinateIn  config writeDataType)
  type Bwd (AvalonMMSubordinate dom fixedWaitTime config readDataType writeDataType) = Signal dom (AvalonSubordinateOut config readDataType)

instance (GoodMMSubordinateConfig config, KeepWaitRequest config ~ 'True) => Backpressure (AvalonMMSubordinate dom 0 config readDataType writeDataType) where
  boolsToBwd _ = C.fromList_lazy . fmap boolToMMSubordinateAck

instance (GoodMMManagerConfig config) => Backpressure (AvalonMMManager dom config readDataType writeDataType) where
  boolsToBwd _ = C.fromList_lazy . fmap boolToMMManagerAck

-- TODO keep waitrequest on when not receiving data?

instance (GoodMMSubordinateConfig config, NFDataX readDataType, NFDataX writeDataType) =>
  DfConv.DfConv   (Reverse (AvalonMMSubordinate dom 0 config readDataType writeDataType)) where
  type Dom        (Reverse (AvalonMMSubordinate dom 0 config readDataType writeDataType)) = dom
  type BwdPayload (Reverse (AvalonMMSubordinate dom 0 config readDataType writeDataType)) = Either (AvalonSubordinateReadReqImpt config) (AvalonSubordinateWriteImpt config writeDataType)
  type FwdPayload (Reverse (AvalonMMSubordinate dom 0 config readDataType writeDataType)) = AvalonSubordinateReadImpt config readDataType

  toDfCircuit _ = DfConv.toDfCircuitHelper s0 blankOtp stateFn where
    s0 = False
    blankOtp = boolToMMSubordinateAck False
    stateFn si dfAck dfDat = do
      dfAckSt <- gets (|| dfAck)
      let (toPut, toRet)
            = case ( mmSubordinateInToWriteImpt si {- write data -}
                   , mmSubordinateInToReadReqImpt si {- read request coming in -}
                   , dfAckSt || dfAck {- df acknowledged read request -}
                   , dfDat {- df sending read data -}
                   ) of
                (Just wi, _, _, _) -> (False, (boolToMMSubordinateAck dfAck, Just (Right wi), False))
                (Nothing, Just rri, True, Just rdat) -> (False, (mmSubordinateReadDat rdat, if dfAckSt then Nothing else Just (Left rri), True))
                (Nothing, Just rri, _, _) -> ((dfAckSt || dfAck), (boolToMMSubordinateAck False, if dfAckSt then Nothing else Just (Left rri), False))
                (Nothing, Nothing, _, _) -> (False, (boolToMMSubordinateAck False, Nothing, False))
      put toPut
      pure toRet

instance (GoodMMSubordinateConfig config, NFDataX readDataType, NFDataX writeDataType) =>
  DfConv.DfConv   (AvalonMMSubordinate dom 0 config readDataType writeDataType) where
  type Dom        (AvalonMMSubordinate dom 0 config readDataType writeDataType) = dom
  type BwdPayload (AvalonMMSubordinate dom 0 config readDataType writeDataType) = AvalonSubordinateReadImpt config readDataType
  type FwdPayload (AvalonMMSubordinate dom 0 config readDataType writeDataType) = Either (AvalonSubordinateReadReqImpt config) (AvalonSubordinateWriteImpt config writeDataType)

  toDfCircuit _ = DfConv.toDfCircuitHelper s0 blankOtp stateFn where
    s0 = Nothing
    blankOtp = mmSubordinateInNoData
    stateFn so dfAck dfDat = do
      readDatStored <- gets (<|> mmSubordinateOutToReadImpt so)
      let (toPut, toRetSi, toRetAck)
            = case ( readDatStored
                   , dfAck
                   , dfDat
                   ) of
            (Just _, True, _) -> (Nothing, mmSubordinateInNoData, False)
            (Just dat, False, _) -> (Just dat, mmSubordinateInNoData, False)
            (Nothing, _, Just (Right wi)) -> (Nothing, mmWriteImptToSubordinateIn wi, mmSubordinateOutToBool so)
            (Nothing, _, Just (Left ri)) -> (Nothing, mmReadReqImptToSubordinateIn ri, mmSubordinateOutToBool so)
            (Nothing, _, Nothing) -> (Nothing, mmSubordinateInNoData, False)
      put toPut
      pure (toRetSi, readDatStored, toRetAck)

instance (GoodMMManagerConfig config, NFDataX readDataType, NFDataX writeDataType) =>
  DfConv.DfConv   (AvalonMMManager dom config readDataType writeDataType) where
  type Dom        (AvalonMMManager dom config readDataType writeDataType) = dom
  type BwdPayload (AvalonMMManager dom config readDataType writeDataType) = AvalonManagerReadImpt config readDataType
  type FwdPayload (AvalonMMManager dom config readDataType writeDataType) = Either (AvalonManagerReadReqImpt config) (AvalonManagerWriteImpt config writeDataType)

  toDfCircuit _ = DfConv.toDfCircuitHelper s0 blankOtp stateFn where
    s0 = Nothing
    blankOtp = mmManagerOutNoData
    stateFn mi dfAck dfDat = do
      readDatStored <- gets (<|> mmManagerInToReadImpt mi)
      let (toPut, toRetMo, toRetAck)
            = case ( readDatStored
                   , dfAck
                   , dfDat
                   ) of
            (Just _, True, _) -> (Nothing, mmManagerOutNoData, False)
            (Just dat, False, _) -> (Just dat, mmManagerOutNoData, False)
            (Nothing, _, Just (Right wi)) -> (Nothing, mmWriteImptToManagerOut wi, mmManagerInToBool mi)
            (Nothing, _, Just (Left ri)) -> (Nothing, mmReadReqImptToManagerOut ri, mmManagerInToBool mi)
            (Nothing, _, Nothing) -> (Nothing, mmManagerOutNoData, False)
      put toPut
      pure (toRetMo, readDatStored, toRetAck)

instance (GoodMMManagerConfig config, NFDataX readDataType, NFDataX writeDataType) =>
  DfConv.DfConv   (Reverse (AvalonMMManager dom config readDataType writeDataType)) where
  type Dom        (Reverse (AvalonMMManager dom config readDataType writeDataType)) = dom
  type BwdPayload (Reverse (AvalonMMManager dom config readDataType writeDataType)) = Either (AvalonManagerReadReqImpt config) (AvalonManagerWriteImpt config writeDataType)
  type FwdPayload (Reverse (AvalonMMManager dom config readDataType writeDataType)) = AvalonManagerReadImpt config readDataType

  toDfCircuit _ = DfConv.toDfCircuitHelper s0 blankOtp stateFn where
    s0 = False
    blankOtp = boolToMMManagerAck False
    stateFn mo dfAck dfDat = do
      dfAckSt <- gets (|| dfAck)
      let (toPut, toRet)
            = case ( mmManagerOutToWriteImpt mo {- write data -}
                   , mmManagerOutToReadReqImpt mo {- read request coming in -}
                   , dfAckSt || dfAck {- df acknowledged read request -}
                   , dfDat {- df sending read data -}
                   ) of
            (Just wi, _, _, _) -> (False, (boolToMMManagerAck dfAck, Just (Right wi), False))
            (Nothing, Just ri, True, Just rdat) -> (False, (mmManagerReadDat rdat, if dfAckSt then Nothing else Just (Left ri), True))
            (Nothing, Just ri, _, _) -> ((dfAckSt || dfAck), (boolToMMManagerAck False, if dfAckSt then Nothing else Just (Left ri), False))
            (Nothing, Nothing, _, _) -> (False, (boolToMMManagerAck False, Nothing, False))
      put toPut
      pure toRet

instance (GoodMMManagerConfig config, NFDataX writeDataType, NFDataX readDataType, KnownDomain dom) =>
  Simulate (AvalonMMManager dom config readDataType writeDataType) where
  type SimulateFwdType (AvalonMMManager dom config readDataType writeDataType) = [AvalonManagerOut config writeDataType]
  type SimulateBwdType (AvalonMMManager dom config readDataType writeDataType) = [AvalonManagerIn config readDataType]
  type SimulateChannels (AvalonMMManager dom config readDataType writeDataType) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ = sample_lazy
  sigToSimBwd _ = sample_lazy

  stallC conf (head -> (stallAck, stalls))
    = withClockResetEnable clockGen resetGen enableGen
    $ (coerceCircuit :: Circuit (Reverse (Reverse a)) b -> Circuit a b)
    $ DfConv.stall Proxy Proxy conf stallAck stalls

instance (GoodMMManagerConfig config, NFDataX writeDataType, NFDataX readDataType, KnownDomain dom) =>
  Drivable (AvalonMMManager dom config readDataType writeDataType) where
  type ExpectType (AvalonMMManager dom config readDataType writeDataType)
    = [Either (AvalonManagerReadReqImpt config) (AvalonManagerWriteImpt config writeDataType)]

  toSimulateType Proxy = _ -- P.map (avalonStreamDataToM2S . Just)
  fromSimulateType Proxy = _ -- Maybe.mapMaybe mmManagerOutToMaybe

{-
  driveC conf vals
    = withClockResetEnable clockGen resetGen enableGen
    $ DfConv.drive Proxy conf (avalonStreamM2SToData <$> vals)
  sampleC conf ckt
    = withClockResetEnable clockGen resetGen enableGen
    $ fmap mmManagerOutToMaybe
    $ DfConv.sample Proxy conf
    $ (coerceCircuit :: Circuit a b -> Circuit a (Reverse (Reverse b)))
    $ ckt
-}

{-
instance
  ( GoodMMManagerConfig config
  , NFDataX writeDataType
  , NFData writeDataType
  , ShowX writeDataType
  , Show writeDataType
  , Eq writeDataType
  , NFDataX readDataType
  , NFData readDataType
  , ShowX readDataType
  , Show readDataType
  , Eq readDataType
  , KnownDomain dom ) =>
  Test (AvalonMMManager dom config readDataType writeDataType) where

  expectToLengths Proxy = pure . P.length
  expectN Proxy options nExpected sampled
    = expectN (Proxy @(Df.Df dom _)) options nExpected
    $ Df.maybeToData . mmManagerOutToMaybe <$> sampled

instance (GoodMMSubordinateConfig config, NFDataX writeDataType, NFDataX readDataType, KnownDomain dom) =>
  Simulate (AvalonMMSubordinate dom 0 config readDataType writeDataType) where
  type SimulateFwdType (AvalonMMSubordinate dom 0 config readDataType writeDataType) = [AvalonSubordinateIn config writeDataType]
  type SimulateBwdType (AvalonMMSubordinate dom 0 config readDataType writeDataType) = [AvalonSubordinateOut config readDataType]
  type SimulateChannels (AvalonMMSubordinate dom 0 config readDataType writeDataType) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ = sample_lazy
  sigToSimBwd _ = sample_lazy

  stallC conf (head -> (stallAck, stalls))
    = withClockResetEnable clockGen resetGen enableGen
    $ (coerceCircuit :: Circuit (Reverse (Reverse a)) b -> Circuit a b)
    $ DfConv.stall Proxy Proxy conf stallAck stalls

instance (KeepWaitRequest config ~ 'True, GoodMMSubordinateConfig config, NFDataX writeDataType, NFDataX readDataType, KnownDomain dom) =>
  Drivable (AvalonMMSubordinate dom 0 config readDataType writeDataType) where
  type ExpectType (AvalonMMSubordinate dom 0 config readDataType writeDataType)
    = [writeDataType]

  -- toSimulateType Proxy = P.map (avalonStreamDataToM2S . Just)
  fromSimulateType Proxy = Maybe.mapMaybe mmSubordinateInToMaybe

{-
  driveC conf vals
    = withClockResetEnable clockGen resetGen enableGen
    $ DfConv.drive Proxy conf (avalonStreamM2SToData <$> vals)
  sampleC conf ckt
    = withClockResetEnable clockGen resetGen enableGen
    $ fmap mmSubordinateInToMaybe
    $ DfConv.sample () conf
    $ (coerceCircuit :: Circuit a b -> Circuit a (Reverse (Reverse b)))
    $ ckt
-}

instance
  ( KeepWaitRequest config ~ 'True
  , GoodMMSubordinateConfig config
  , NFDataX writeDataType
  , NFData writeDataType
  , ShowX writeDataType
  , Show writeDataType
  , Eq writeDataType
  , NFDataX readDataType
  , NFData readDataType
  , ShowX readDataType
  , Show readDataType
  , Eq readDataType
  , KnownDomain dom ) =>
  Test (AvalonMMSubordinate dom 0 config readDataType writeDataType) where

  expectToLengths Proxy = pure . P.length
  expectN Proxy options nExpected sampled
    = expectN (Proxy @(Df.Df dom _)) options nExpected
    $ Df.maybeToData . mmSubordinateInToMaybe <$> sampled
-}
