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


-- TODO representing a well-behaved shared config.
-- This class holds for every possible @AvalonMMSharedConfig@,
-- but we need to write out the class anyway so that GHC holds.
type GoodMMSharedConfig config =
  ( KnownNat      (AddrWidth         config)
  , KeepTypeClass (KeepRead          config)
  , KeepTypeClass (KeepWrite         config)
  , MaybeZeroNat  (ByteEnableWidth   config)
  , MaybeZeroNat  (BurstCountWidth   config)
  , KeepTypeClass (KeepReadDataValid config)
  , KeepTypeClass (KeepEndOfPacket   config)
  , KeepTypeClass (KeepIrq           config)
  )

-- Class representing a well-behaved subordinate config.
-- This class holds for every possible @AvalonMMSubordinateConfig@,
-- but we need to write out the class anyway so that GHC holds.
type GoodMMSubordinateConfig config =
  ( MaybeZeroNat  (WriteByteEnableWidth   config)
  , KeepTypeClass (KeepChipSelect         config)
  , KeepTypeClass (KeepBeginTransfer      config)
  , KeepTypeClass (KeepWaitRequest        config)
  , KeepTypeClass (KeepBeginBurstTransfer config)
  , KeepTypeClass (KeepReadyForData       config)
  , KeepTypeClass (KeepDataAvailable      config)
  , GoodMMSharedConfig (SShared           config)
  )

-- Class representing a well-behaved manager config.
-- This class holds for every possible @AvalonMMManagerConfig@,
-- but we need to write out the class anyway so that GHC holds.
type GoodMMManagerConfig config =
  ( KeepTypeClass (KeepFlush      config)
  , KnownNat      (IrqListWidth   config)
  , KnownNat      (IrqNumberWidth config)
  , GoodMMSharedConfig (MShared   config)
  )


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

-- TODO irq, fixed wait time
interconnectFabric ::
  forall dom managerConfig subordinateConfig numManager numSubordinate readDataType writeDataType.
  ( GoodMMManagerConfig managerConfig
  , GoodMMSubordinateConfig subordinateConfig
  , MShared managerConfig ~ SShared subordinateConfig
  , HiddenClockResetEnable dom
  , KnownNat numManager
  , KnownNat numSubordinate
  ) =>
  (Unsigned (AddrWidth (SShared subordinateConfig)) -> Maybe (Index numSubordinate)) ->
  Circuit
    (Vec numManager (AvalonMMManager dom managerConfig readDataType writeDataType))
    (Vec numSubordinate (AvalonMMSubordinate dom 0 {- TODO (this is the wait time) -} subordinateConfig readDataType writeDataType))
interconnectFabric addrFn
  =  undoDoubleReverse (DfConv.interconnect (repeat dfA) (repeat dfC) reqFn)
  |> vecCircuits (repeat (DfConv.mapBoth dfC dfB undefined undefined))
 where
  dfA = Proxy @(AvalonMMManager dom managerConfig readDataType writeDataType)
  dfB = Proxy @(AvalonMMSubordinate dom 0 subordinateConfig readDataType writeDataType)
  dfC = Proxy @(Df.Df dom _, Reverse (Df.Df dom _))
  reqFn (AvalonManagerOut{..})
    | not (fromKeepTypeDef True mo_read || fromKeepTypeDef True mo_write) = Nothing
    | otherwise = addrFn mo_addr
  undoDoubleReverse :: Circuit (Vec x (Reverse (Reverse p))) q -> Circuit (Vec x p) q
  undoDoubleReverse = coerceCircuit
{-
  { mo_addr        = mwi_addr
  , mo_read        = toKeepType False
  , mo_write       = toKeepType True
  , mo_byteEnable  = mwi_byteEnable
  , mo_burstCount  = mwi_burstCount
  , mo_flush       = mwi_flush
  , mo_writeData   = mwi_writeData
-}

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

-- NOTE: Unfortunately, we can't write a 'Drivable' instance (and, by extension, a 'Test' instance) for 'AvalonMMManager' or 'AvalonMMSubordinate'.
-- This is because they have both write data sent one way and read data sent the other. By writing a 'Drivable' instance (meant for protocols
-- with unidirectional data), we would have to ignore one or the other.
--
-- Tests can still be made for Avalon MM circuits, by attaching 'DfConv.dfToDfConvInp' and/or 'DfConv.dfToDfConvOtp' on both ends of the circuit.
-- Then you will end up with unidirectional data on both sides, choosing yourself whether to ignore either the read data or the write data.
--
-- TODO it would be nice to drive a constant value in the other direction, can we change dfToDfConvInp or make a circuit that drives the read line
-- with a constant?
-- Also can we make an @instance DfConv (Df dom a, Reverse (Df dom b))@? This should be the canonical 'DfConv' instance but we never wrote it.
