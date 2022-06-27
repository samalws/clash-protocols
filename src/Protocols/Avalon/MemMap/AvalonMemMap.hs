{-|
Types and instance declarations for the Avalon memory mapped protocol.
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, TypeFamilyDependencies, UndecidableInstances #-}

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
  { addrWidth         :: Nat
  , keepRead          :: Bool
  , keepWrite         :: Bool
  , byteEnableWidth   :: Nat
  , burstCountWidth   :: Nat
  , keepWaitRequest   :: Bool
  , keepReadDataValid :: Bool
  , keepEndOfPacket   :: Bool
  , keepIrq           :: Bool
  }

data AvalonMMSlaveConfig
  =  AvalonMMSlaveConfig
  { writeByteEnableWidth   :: Nat
  , keepBeginTransfer      :: Bool
  , keepBeginBurstTransfer :: Bool
  , keepReadyForData       :: Bool
  , keepDataAvailable      :: Bool
  , sShared                :: AvalonMMSharedConfig
  }

data AvalonMMMasterConfig
  =  AvalonMMMasterConfig
  { keepFlush      :: Bool
  , irqNumberWidth :: Nat
  , mShared        :: AvalonMMSharedConfig
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
  WriteByteEnableWidth ('AvalonMMSlaveConfig a _ _ _ _ _) = a

type family KeepBeginTransfer (c :: AvalonMMSlaveConfig) where
  KeepBeginTransfer ('AvalonMMSlaveConfig _ a _ _ _ _) = a

type family KeepBeginBurstTransfer (c :: AvalonMMSlaveConfig) where
  KeepBeginBurstTransfer ('AvalonMMSlaveConfig _ _ a _ _ _) = a

type family KeepReadyForData (c :: AvalonMMSlaveConfig) where
  KeepReadyForData ('AvalonMMSlaveConfig _ _ _ a _ _) = a

type family KeepDataAvailable (c :: AvalonMMSlaveConfig) where
  KeepDataAvailable ('AvalonMMSlaveConfig _ _ _ _ a _) = a

type family SShared (c :: AvalonMMSlaveConfig) where
  SShared ('AvalonMMSlaveConfig _ _ _ _ _ a) = a


type family KeepFlush (c :: AvalonMMMasterConfig) where
  KeepFlush ('AvalonMMMasterConfig a _ _) = a

type family IrqNumberWidth (c :: AvalonMMMasterConfig) where
  IrqNumberWidth ('AvalonMMMasterConfig _ a _) = a

type family MShared (c :: AvalonMMMasterConfig) where
  MShared ('AvalonMMMasterConfig _ _ a) = a


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
  , GoodMMSharedConfig (SShared           config)
  ) => GoodMMSlaveConfig config
instance
  ( KnownNat      (WriteByteEnableWidth   config)
  , KeepBoolClass (KeepBeginTransfer      config)
  , KeepBoolClass (KeepBeginBurstTransfer config)
  , KeepBoolClass (KeepReadyForData       config)
  , KeepBoolClass (KeepDataAvailable      config)
  , GoodMMSharedConfig (SShared           config)
  ) => GoodMMSlaveConfig config

class
  ( KeepBoolClass (KeepFlush      config)
  , KnownNat      (IrqNumberWidth config)
  , GoodMMSharedConfig (MShared   config)
  ) => GoodMMMasterConfig config
instance
  ( KeepBoolClass (KeepFlush      config)
  , KnownNat      (IrqNumberWidth config)
  , GoodMMSharedConfig (MShared   config)
  ) => GoodMMMasterConfig config


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


data AvalonMasterIn config readDataType
  =  AvalonMasterIn
  { mi_waitRequest   :: KeepBool (KeepWaitRequest   (MShared config))
  , mi_readDataValid :: KeepBool (KeepReadDataValid (MShared config))
  , mi_endOfPacket   :: KeepBool (KeepEndOfPacket   (MShared config))
  , mi_irq           :: KeepBool (KeepIrq           (MShared config))
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


data AvalonSlaveOut config readDataType
  =  AvalonSlaveOut
  { so_waitRequest   :: KeepBool (KeepWaitRequest   (SShared config))
  , so_readDataValid :: KeepBool (KeepReadDataValid (SShared config))
  , so_endOfPacket   :: KeepBool (KeepEndOfPacket   (SShared config))
  , so_irq           :: KeepBool (KeepIrq           (SShared config))
  , so_readyForData  :: KeepBool (KeepReadyForData           config)
  , so_dataAvailable :: KeepBool (KeepDataAvailable          config)
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


data AvalonSlaveIn config writeDataType
  =  AvalonSlaveIn
  { si_addr               :: Unsigned (AddrWidth              (SShared config))
  , si_read               :: KeepBool (KeepRead               (SShared config))
  , si_write              :: KeepBool (KeepWrite              (SShared config))
  , si_byteEnable         :: Unsigned (ByteEnableWidth        (SShared config))
  , si_burstCount         :: Unsigned (BurstCountWidth        (SShared config))
  , si_chipSelect         :: Bool
  , si_writeByteEnable    :: Unsigned (WriteByteEnableWidth            config)
  , si_beginTransfer      :: KeepBool (KeepBeginTransfer               config)
  , si_beginBurstTransfer :: KeepBool (KeepBeginBurstTransfer          config)
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


blankMi :: (GoodMMMasterConfig config) => AvalonMasterIn config readDataType
blankMi
  = AvalonMasterIn
  { mi_waitRequest   = toKeepBool False
  , mi_readDataValid = toKeepBool False
  , mi_endOfPacket   = toKeepBool False
  , mi_irq           = toKeepBool False
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No read data defined"
  }

blankSi :: (GoodMMSlaveConfig config) => AvalonSlaveIn config writeDataType
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

avalonInterconnectFabric ::
  ( HiddenClockResetEnable dom
  , KnownNat nMaster
  , KnownNat nSlave
  , GoodMMMasterConfig masterConfig
  , GoodMMSlaveConfig  slaveConfig
  , AddrWidth (MShared masterConfig) ~ AddrWidth (SShared slaveConfig)
  , ByteEnableWidth (MShared masterConfig) ~ ByteEnableWidth (SShared slaveConfig)
  -- , ByteEnableWidth (MShared masterConfig) ~ WriteByteEnableWidth slaveConfig -- TODO or writebyteenablewidth == 0
  , BurstCountWidth (MShared masterConfig) ~ BurstCountWidth (SShared slaveConfig)
  )
  => Vec nSlave (Unsigned (AddrWidth (SShared slaveConfig)) -> Bool)
  -> Vec nSlave (Unsigned (IrqNumberWidth masterConfig))
  -> (Vec nMaster (Signal dom (AvalonMasterOut masterConfig writeDataType)),
      Vec nSlave  (Signal dom (AvalonSlaveOut  slaveConfig  readDataType)))
  -> (Vec nMaster (Signal dom (AvalonMasterIn  masterConfig readDataType)),
      Vec nSlave  (Signal dom (AvalonSlaveIn   slaveConfig  writeDataType)))
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

boolToMMMasterAck :: (GoodMMMasterConfig config) => Bool -> AvalonMasterIn config readDataType
boolToMMMasterAck ack
  =  AvalonMasterIn
  { mi_waitRequest   = toKeepBool ack
  , mi_readDataValid = toKeepBool False
  , mi_endOfPacket   = toKeepBool False
  , mi_irq           = toKeepBool False
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No readData for boolToAck"
  }

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
-- TODO didnt i just define this above

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

mmSlaveOutToBool :: (GoodMMSlaveConfig config) => AvalonSlaveOut config readDataType -> Bool
mmSlaveOutToBool so = fromKeepBool True (so_readyForData so) && not (fromKeepBool False (so_waitRequest so))
-- TODO "SlaveOut" vs "so"

mmMasterInToBool :: (GoodMMMasterConfig config) => AvalonMasterIn config readDataType -> Bool
mmMasterInToBool = not . fromKeepBool False . mi_waitRequest

mmSlaveInSendingData :: (GoodMMSlaveConfig config) => AvalonSlaveIn config writeDataType
mmSlaveInSendingData
  = AvalonSlaveIn
  { si_chipSelect         = toKeepBool True
  , si_addr               = 0
  , si_read               = toKeepBool False
  , si_write              = toKeepBool True
  , si_byteEnable         = 0 -- TODO
  , si_writeByteEnable    = 0
  , si_beginTransfer      = toKeepBool False
  , si_burstCount         = 0
  , si_beginBurstTransfer = toKeepBool False
  , si_writeData          = errorX "No writeData for mmSlaveInSendingData"
  }

mmMasterOutSendingData :: (GoodMMMasterConfig config) => AvalonMasterOut config writeDataType
mmMasterOutSendingData
  = AvalonMasterOut
  { mo_addr        = 0
  , mo_read        = toKeepBool False
  , mo_write       = toKeepBool True
  , mo_byteEnable  = 0 -- TODO
  , mo_burstCount  = 0
  , mo_flush       = toKeepBool False
  , mo_writeData   = errorX "No writeData for mmMasterOutSendingData"
  }


-- | Grab the data from a master-to-slave message, if there is any
mmSlaveInToMaybe :: (GoodMMSlaveConfig config) => AvalonSlaveIn config writeDataType -> Maybe writeDataType
mmSlaveInToMaybe si = if cond then Just (si_writeData si) else Nothing where
  cond =  fromKeepBool True (si_chipSelect si)
       && fromKeepBool True (si_write si)
       && not (fromKeepBool False (si_read si))
  -- TODO look at byteenable

mmMasterOutToMaybe :: (GoodMMMasterConfig config) => AvalonMasterOut config writeDataType -> Maybe writeDataType
mmMasterOutToMaybe mo = if cond then Just (mo_writeData mo) else Nothing where
  cond =  fromKeepBool True (mo_write mo)
       && not (fromKeepBool False (mo_read mo))
  -- TODO look at byteenable


data AvalonMMMaster (dom :: Domain) (config :: AvalonMMMasterConfig) (readDataType :: Type) (writeDataType :: Type) = AvalonMMMaster

data AvalonMMSlave (dom :: Domain) (config :: AvalonMMSlaveConfig) (readDataType :: Type) (writeDataType :: Type) = AvalonMMSlave

instance Protocol (AvalonMMMaster dom config readDataType writeDataType) where
  type Fwd (AvalonMMMaster dom config readDataType writeDataType) = Signal dom (AvalonMasterOut config writeDataType)
  type Bwd (AvalonMMMaster dom config readDataType writeDataType) = Signal dom (AvalonMasterIn  config readDataType)

instance Protocol (AvalonMMSlave dom config readDataType writeDataType) where
  type Fwd (AvalonMMSlave dom config readDataType writeDataType) = Signal dom (AvalonSlaveIn  config writeDataType)
  type Bwd (AvalonMMSlave dom config readDataType writeDataType) = Signal dom (AvalonSlaveOut config readDataType)

instance (GoodMMSlaveConfig config) => Backpressure (AvalonMMSlave dom config readDataType writeDataType) where
  boolsToBwd _ = C.fromList_lazy . fmap boolToMMSlaveAck

instance (GoodMMMasterConfig config) => Backpressure (AvalonMMMaster dom config readDataType writeDataType) where
  boolsToBwd _ = C.fromList_lazy . fmap boolToMMMasterAck

instance (KnownDomain dom, GoodMMSlaveConfig config) => Simulate (AvalonMMSlave dom config readDataType writeDataType) where
  type SimulateFwdType (AvalonMMSlave dom config readDataType writeDataType) = [AvalonSlaveIn  config writeDataType]
  type SimulateBwdType (AvalonMMSlave dom config readDataType writeDataType) = [AvalonSlaveOut config readDataType]
  type SimulateChannels (AvalonMMSlave dom config readDataType writeDataType) = 1

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

instance (KnownDomain dom, GoodMMSlaveConfig config) => Drivable (AvalonMMSlave dom config readDataType writeDataType) where
  type ExpectType (AvalonMMSlave dom config readDataType writeDataType) = [writeDataType]

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

instance (KnownDomain dom, GoodMMSlaveConfig config) => DfLike dom (AvalonMMSlave dom config readDataType) writeDataType where
  type Data (AvalonMMSlave dom config readDataType) writeDataType = AvalonSlaveIn config writeDataType
  type Payload writeDataType = writeDataType
  type Ack (AvalonMMSlave dom config readDataType) writeDataType = AvalonSlaveOut config readDataType

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
          => Test (AvalonMMSlave dom config readDataType writeDataType) where

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
