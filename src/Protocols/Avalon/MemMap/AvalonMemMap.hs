{-|
Types and instance declarations for the Avalon memory mapped protocol.
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, RecordWildCards, TypeFamilyDependencies, UndecidableInstances #-}

module Protocols.Avalon.MemMap.AvalonMemMap where

-- base
import           Control.DeepSeq (NFData)
import           Prelude hiding (not, (&&), repeat, (!!))

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


type family KeepBool (keep :: Bool) = t | t -> keep where
  KeepBool 'True = Bool
  KeepBool 'False = ()

class (NFDataX (KeepBool keep), NFData (KeepBool keep), Show (KeepBool keep), ShowX (KeepBool keep), Eq (KeepBool keep)) => KeepBoolClass (keep :: Bool) where
  fromKeepBool :: Bool -> KeepBool keep -> Bool
  toKeepBool   :: Bool -> KeepBool keep

instance KeepBoolClass 'True where
  fromKeepBool _ = id
  toKeepBool = id

instance KeepBoolClass 'False where
  fromKeepBool b = const b
  toKeepBool = const ()

convKeepBool :: (KeepBoolClass a, KeepBoolClass b) => Bool -> KeepBool a -> KeepBool b
convKeepBool b = toKeepBool . fromKeepBool b


data AvalonMMSharedConfig
  =  AvalonMMSharedConfig
  { addrWidth            :: Nat
  , keepRead             :: Bool
  , keepWrite            :: Bool
  , byteEnableWidth      :: Nat
  , burstCountWidth      :: Nat
  , keepWaitRequest      :: Bool
  , keepReadDataValid    :: Bool
  , keepEndOfPacket      :: Bool
  , keepIrq              :: Bool
  }

data AvalonMMSlaveConfig
  =  AvalonMMSlaveConfig
  { writeByteEnableWidth   :: Nat
  , keepBeginTransfer      :: Bool
  , keepBeginBurstTransfer :: Bool
  , keepReadyForData       :: Bool
  , keepDataAvailable      :: Bool
  }

data AvalonMMMasterConfig
  =  AvalonMMMasterConfig
  { keepFlush      :: Bool
  , irqNumberWidth :: Nat
  }


type family AddrWidth (c :: AvalonMMSharedConfig) where
  AddrWidth ('AvalonMMSharedConfig a _ _ _ _ _ _ _ _) = a

type family KeepRead (c :: AvalonMMSharedConfig) where
  KeepRead ('AvalonMMSharedConfig _ a _ _ _ _ _ _ _) = a

type family KeepWrite (c :: AvalonMMSharedConfig) where
  KeepWrite ('AvalonMMSharedConfig _ _ a _ _ _ _ _ _) = a

type family ByteEnableWidth (c :: AvalonMMSharedConfig) where
  ByteEnableWidth ('AvalonMMSharedConfig _ _ _ a _ _ _ _ _) = a

type family BurstCountWidth (c :: AvalonMMSharedConfig) where
  BurstCountWidth ('AvalonMMSharedConfig _ _ _ _ a _ _ _ _) = a

type family KeepWaitRequest (c :: AvalonMMSharedConfig) where
  KeepWaitRequest ('AvalonMMSharedConfig _ _ _ _ _ a _ _ _) = a

type family KeepReadDataValid (c :: AvalonMMSharedConfig) where
  KeepReadDataValid ('AvalonMMSharedConfig _ _ _ _ _ _ a _ _) = a

type family KeepEndOfPacket (c :: AvalonMMSharedConfig) where
  KeepEndOfPacket ('AvalonMMSharedConfig _ _ _ _ _ _ _ a _) = a

type family KeepIrq (c :: AvalonMMSharedConfig) where
  KeepIrq ('AvalonMMSharedConfig _ _ _ _ _ _ _ _ a) = a


type family WriteByteEnableWidth (c :: AvalonMMSlaveConfig) where
  WriteByteEnableWidth ('AvalonMMSlaveConfig a _ _ _ _) = a

type family KeepBeginTransfer (c :: AvalonMMSlaveConfig) where
  KeepBeginTransfer ('AvalonMMSlaveConfig _ a _ _ _) = a

type family KeepBeginBurstTransfer (c :: AvalonMMSlaveConfig) where
  KeepBeginBurstTransfer ('AvalonMMSlaveConfig _ _ a _ _) = a

type family KeepReadyForData (c :: AvalonMMSlaveConfig) where
  KeepReadyForData ('AvalonMMSlaveConfig _ _ _ a _) = a

type family KeepDataAvailable (c :: AvalonMMSlaveConfig) where
  KeepDataAvailable ('AvalonMMSlaveConfig _ _ _ _ a) = a


type family KeepFlush (c :: AvalonMMMasterConfig) where
  KeepFlush ('AvalonMMMasterConfig a _) = a

type family IrqNumberWidth (c :: AvalonMMMasterConfig) where
  IrqNumberWidth ('AvalonMMMasterConfig _ a) = a


class
  ( KnownNat      (AddrWidth         config)
  , KeepBoolClass (KeepRead          config)
  , KeepBoolClass (KeepWrite         config)
  , KnownNat      (ByteEnableWidth   config)
  , KnownNat      (BurstCountWidth   config)
  , KeepBoolClass (KeepWaitRequest   config)
  , KeepBoolClass (KeepReadDataValid config)
  , KeepBoolClass (KeepEndOfPacket   config)
  , KeepBoolClass (KeepIrq           config)
  ) => GoodMMSharedConfig config
instance
  ( KnownNat      (AddrWidth         config)
  , KeepBoolClass (KeepRead          config)
  , KeepBoolClass (KeepWrite         config)
  , KnownNat      (ByteEnableWidth   config)
  , KnownNat      (BurstCountWidth   config)
  , KeepBoolClass (KeepWaitRequest   config)
  , KeepBoolClass (KeepReadDataValid config)
  , KeepBoolClass (KeepEndOfPacket   config)
  , KeepBoolClass (KeepIrq           config)
  ) => GoodMMSharedConfig config

class
  ( KnownNat      (WriteByteEnableWidth   config)
  , KeepBoolClass (KeepBeginTransfer      config)
  , KeepBoolClass (KeepBeginBurstTransfer config)
  , KeepBoolClass (KeepReadyForData       config)
  , KeepBoolClass (KeepDataAvailable      config)
  ) => GoodMMSlaveConfig config
instance
  ( KnownNat      (WriteByteEnableWidth   config)
  , KeepBoolClass (KeepBeginTransfer      config)
  , KeepBoolClass (KeepBeginBurstTransfer config)
  , KeepBoolClass (KeepReadyForData       config)
  , KeepBoolClass (KeepDataAvailable      config)
  ) => GoodMMSlaveConfig config

class
  ( KeepBoolClass (KeepFlush      config)
  , KnownNat      (IrqNumberWidth config)
  ) => GoodMMMasterConfig config
instance
  ( KeepBoolClass (KeepFlush      config)
  , KnownNat      (IrqNumberWidth config)
  ) => GoodMMMasterConfig config


data AvalonMasterOut sharedConfig masterConfig writeDataType
  =  AvalonMasterOut
  { mo_addr        :: Unsigned (AddrWidth       sharedConfig)
  , mo_read        :: KeepBool (KeepRead        sharedConfig)
  , mo_write       :: KeepBool (KeepWrite       sharedConfig)
  , mo_byteEnable  :: Unsigned (ByteEnableWidth sharedConfig)
  , mo_burstCount  :: Unsigned (BurstCountWidth sharedConfig)
  , mo_waitRequest :: KeepBool (KeepWaitRequest sharedConfig)
  , mo_flush       :: KeepBool (KeepFlush       masterConfig)
  , mo_writeData   :: writeDataType
  }
  deriving Generic

deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMMasterConfig masterConfig,
                   NFDataX            writeDataType) => NFDataX (AvalonMasterOut sharedConfig masterConfig writeDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMMasterConfig masterConfig,
                   NFData             writeDataType) => NFData  (AvalonMasterOut sharedConfig masterConfig writeDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMMasterConfig masterConfig,
                   ShowX              writeDataType) => ShowX   (AvalonMasterOut sharedConfig masterConfig writeDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMMasterConfig masterConfig,
                   Show               writeDataType) => Show    (AvalonMasterOut sharedConfig masterConfig writeDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMMasterConfig masterConfig,
                   Eq                 writeDataType) => Eq      (AvalonMasterOut sharedConfig masterConfig writeDataType)


data AvalonMasterIn sharedConfig masterConfig readDataType
  =  AvalonMasterIn
  { mi_waitRequest   :: KeepBool (KeepWaitRequest   sharedConfig)
  , mi_readDataValid :: KeepBool (KeepReadDataValid sharedConfig)
  , mi_endOfPacket   :: KeepBool (KeepEndOfPacket   sharedConfig)
  , mi_irq           :: KeepBool (KeepIrq           sharedConfig)
  , mi_irqNumber     :: Unsigned (IrqNumberWidth    masterConfig)
  , mi_readData      :: readDataType
  }
  deriving Generic

deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMMasterConfig masterConfig,
                   NFDataX            readDataType) => NFDataX (AvalonMasterIn sharedConfig masterConfig readDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMMasterConfig masterConfig,
                   NFData             readDataType) => NFData  (AvalonMasterIn sharedConfig masterConfig readDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMMasterConfig masterConfig,
                   ShowX              readDataType) => ShowX   (AvalonMasterIn sharedConfig masterConfig readDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMMasterConfig masterConfig,
                   Show               readDataType) => Show    (AvalonMasterIn sharedConfig masterConfig readDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMMasterConfig masterConfig,
                   Eq                 readDataType) => Eq      (AvalonMasterIn sharedConfig masterConfig readDataType)


data AvalonSlaveOut sharedConfig slaveConfig readDataType
  =  AvalonSlaveOut
  { so_waitRequest        :: KeepBool (KeepWaitRequest   sharedConfig)
  , so_readDataValid      :: KeepBool (KeepReadDataValid sharedConfig)
  , so_endOfPacket        :: KeepBool (KeepEndOfPacket   sharedConfig)
  , so_irq                :: KeepBool (KeepIrq           sharedConfig)
  , so_readyForData       :: KeepBool (KeepReadyForData  slaveConfig)
  , so_dataAvailable      :: KeepBool (KeepDataAvailable slaveConfig)
  , so_readData           :: readDataType
  }
  deriving Generic

deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMSlaveConfig  slaveConfig,
                   NFDataX            readDataType) => NFDataX (AvalonSlaveOut sharedConfig slaveConfig readDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMSlaveConfig  slaveConfig,
                   NFData             readDataType) => NFData  (AvalonSlaveOut sharedConfig slaveConfig readDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMSlaveConfig  slaveConfig,
                   ShowX              readDataType) => ShowX   (AvalonSlaveOut sharedConfig slaveConfig readDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMSlaveConfig  slaveConfig,
                   Show               readDataType) => Show    (AvalonSlaveOut sharedConfig slaveConfig readDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMSlaveConfig  slaveConfig,
                   Eq                 readDataType) => Eq      (AvalonSlaveOut sharedConfig slaveConfig readDataType)


data AvalonSlaveIn sharedConfig slaveConfig writeDataType
  =  AvalonSlaveIn
  { si_addr               :: Unsigned (AddrWidth              sharedConfig)
  , si_read               :: KeepBool (KeepRead               sharedConfig)
  , si_write              :: KeepBool (KeepWrite              sharedConfig)
  , si_byteEnable         :: Unsigned (ByteEnableWidth        sharedConfig)
  , si_burstCount         :: Unsigned (BurstCountWidth        sharedConfig)
  , si_chipSelect         :: Bool
  , si_writeByteEnable    :: Unsigned (WriteByteEnableWidth   slaveConfig)
  , si_beginTransfer      :: KeepBool (KeepBeginTransfer      slaveConfig)
  , si_beginBurstTransfer :: KeepBool (KeepBeginBurstTransfer slaveConfig)
  , si_writeData          :: writeDataType
  }
  deriving Generic

deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMSlaveConfig  slaveConfig,
                   NFDataX            writeDataType) => NFDataX (AvalonSlaveIn sharedConfig slaveConfig writeDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMSlaveConfig  slaveConfig,
                   NFData             writeDataType) => NFData  (AvalonSlaveIn sharedConfig slaveConfig writeDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMSlaveConfig  slaveConfig,
                   ShowX              writeDataType) => ShowX   (AvalonSlaveIn sharedConfig slaveConfig writeDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMSlaveConfig  slaveConfig,
                   Show               writeDataType) => Show    (AvalonSlaveIn sharedConfig slaveConfig writeDataType)
deriving instance (GoodMMSharedConfig sharedConfig,
                   GoodMMSlaveConfig  slaveConfig,
                   Eq                 writeDataType) => Eq      (AvalonSlaveIn sharedConfig slaveConfig writeDataType)


-- | Type for Avalon memory mapped protocol.
-- We do not support the bidirectional port @data@
data AvalonMM
     (dom           :: Domain)
     (sharedConfig  :: AvalonMMSharedConfig)
     (masterConfig  :: AvalonMMMasterConfig)
     (slaveConfig   :: AvalonMMSlaveConfig)
     (readDataType  :: Type)
     (writeDataType :: Type)


avalonInterconnectFabric ::
  ( HiddenClockResetEnable dom
  , KnownNat nMaster
  , KnownNat nSlave
  , GoodMMSharedConfig sharedConfigM
  , GoodMMSharedConfig sharedConfigS
  , GoodMMMasterConfig masterConfig
  , GoodMMSlaveConfig  slaveConfig
  , AddrWidth sharedConfigM ~ AddrWidth sharedConfigS
  , ByteEnableWidth sharedConfigM ~ ByteEnableWidth sharedConfigS
  -- , ByteEnableWidth sharedConfigM ~ WriteByteEnableWidth slaveConfig -- TODO or writebyteenablewidth == 0
  , BurstCountWidth sharedConfigM ~ BurstCountWidth sharedConfigS
  )
  => Vec nSlave (Unsigned (AddrWidth sharedConfigM) -> Bool)
  -> Vec nSlave (Unsigned (IrqNumberWidth masterConfig))
  -> (Vec nMaster (Signal dom (AvalonMasterOut sharedConfigM masterConfig writeDataType)),
      Vec nSlave  (Signal dom (AvalonSlaveOut  sharedConfigS slaveConfig  readDataType)))
  -> (Vec nMaster (Signal dom (AvalonMasterIn  sharedConfigM masterConfig readDataType)),
      Vec nSlave  (Signal dom (AvalonSlaveIn   sharedConfigS slaveConfig  writeDataType)))
avalonInterconnectFabric slaveAddrFns irqNums (inpA, inpB) = (unbundle otpA, unbundle otpB) where
  (otpA, otpB) = unbundle $ mealy transFn s0 $ bundle (bundle inpA, bundle inpB)

  s0 = ()
  transFn () (mo, so) = let (ms, sm) = masterSlavePairings mo in ((), (maybe blankMi (\n -> convSoMi (so !! n) (irqNums !! n)) <$> ms, maybe blankSi (convMoSi . (mo !!)) <$> sm))

  -- TODO what if 2 masters put in same slave addr
  masterSlavePairings mo = ((\moMsg -> findIndex ($ mo_addr moMsg) slaveAddrFns) <$> mo, (\addrFn -> findIndex (addrFn . mo_addr) mo) <$> slaveAddrFns)

  convSoMi so irqNum
    = AvalonMasterIn
    { mi_waitRequest   = convKeepBool False (so_waitRequest so) -- TODO
    , mi_readDataValid = convKeepBool False (so_readDataValid so)
    , mi_endOfPacket   = convKeepBool False (so_endOfPacket so)
    , mi_irq           = convKeepBool False (so_irq so)
    , mi_irqNumber     = if (fromKeepBool False (so_irq so)) then irqNum else 0 -- TODO what if someone else's irq is on
    , mi_readData      = so_readData so
    }

  convMoSi mo
    =  AvalonSlaveIn
    { si_addr               = mo_addr mo
    , si_read               = toKeepBool $ fromKeepBool True (mo_read  mo) && not (fromKeepBool False (mo_write mo))
    , si_write              = toKeepBool $ fromKeepBool True (mo_write mo) && not (fromKeepBool False (mo_read  mo))
    , si_writeByteEnable    = resize $ if (fromKeepBool True (mo_write mo)) then mo_byteEnable mo else 0
    , si_burstCount         = mo_burstCount mo
    , si_chipSelect         = toKeepBool True
    , si_byteEnable         = mo_byteEnable mo
    , si_beginTransfer      = errorX "TODO"
    , si_beginBurstTransfer = errorX "TODO"
    , si_writeData          = mo_writeData mo
    }

blankMi :: (GoodMMSharedConfig sharedConfig, GoodMMMasterConfig masterConfig) => AvalonMasterIn sharedConfig masterConfig writeDataType
blankMi
  = AvalonMasterIn
  { mi_waitRequest   = toKeepBool False
  , mi_readDataValid = toKeepBool False
  , mi_endOfPacket   = toKeepBool False
  , mi_irq           = toKeepBool False
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No read data defined"
  }

blankSi :: (GoodMMSharedConfig sharedConfig, GoodMMSlaveConfig slaveConfig) => AvalonSlaveIn sharedConfig slaveConfig writeDataType
blankSi
  =  AvalonSlaveIn
  { si_addr               = 0
  , si_read               = toKeepBool False
  , si_write              = toKeepBool False
  , si_writeByteEnable    = 0
  , si_burstCount         = 0
  , si_chipSelect         = toKeepBool False
  , si_byteEnable         = 0
  , si_beginTransfer      = toKeepBool False
  , si_beginBurstTransfer = toKeepBool False
  , si_writeData          = errorX "No write data defined"
  }
