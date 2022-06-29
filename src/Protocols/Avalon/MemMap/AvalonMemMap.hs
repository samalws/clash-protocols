{-|
Types and instance declarations for the Avalon memory mapped protocol.
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, TypeFamilyDependencies, UndecidableInstances #-}

module Protocols.Avalon.MemMap.AvalonMemMap where

-- base
import           Control.DeepSeq (NFData)
import           Prelude hiding (not, (&&), (||), repeat, (!!), foldl, unzip)

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


class (KnownNat n) => MaybeZeroNat (n :: Nat) where
  fromMaybeEmptyNum :: Unsigned (n+1) -> Unsigned n -> Unsigned (n+1)

instance MaybeZeroNat 0 where
  fromMaybeEmptyNum n _ = n

instance (1 <= n, KnownNat n) => MaybeZeroNat n where
  fromMaybeEmptyNum _ m = resize m


class EqOrZero (n :: Nat) (m :: Nat)
instance EqOrZero 0 m
instance EqOrZero m m


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

data AvalonMMMasterConfig
  =  AvalonMMMasterConfig
  { keepFlush      :: Bool
  , irqListWidth   :: Nat
  , irqNumberWidth :: Nat
  , mShared        :: AvalonMMSharedConfig
  }


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
  { mi_waitRequest   :: Bool
  , mi_readDataValid :: KeepBool (KeepReadDataValid (MShared config))
  , mi_endOfPacket   :: KeepBool (KeepEndOfPacket   (MShared config))
  , mi_irq           :: KeepBool (KeepIrq           (MShared config))
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
  , si_writeByteEnable    :: Unsigned (WriteByteEnableWidth            config)
  , si_chipSelect         :: KeepBool (KeepChipSelect                  config)
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


avalonInterconnectFabric ::
  ( HiddenClockResetEnable dom
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
  -> Circuit
     (Vec nMaster (AvalonMMMaster dom masterConfig readDataType writeDataType))
     (Vec nSlave  (AvalonMMSlave  dom slaveConfig  readDataType writeDataType))
avalonInterconnectFabric slaveAddrFns irqNums = Circuit cktFn where
  cktFn (inpA, inpB) = (unbundle otpA, unbundle otpB) where (otpA, otpB) = unbundle $ mealy transFn s0 $ bundle (bundle inpA, bundle inpB)

  s0 = (repeat Nothing, repeat Nothing)

  -- xferSt is indexed by slave
  transFn (smOld, xferSt) (mo, so) = ((sm, xferSt'), (mi, si)) where
    (ms, sm) = masterSlavePairings mo smOld xferSt
    mirq = minIrq so
    irqList = fromKeepBool False . so_irq <$> so
    setIrq miMsg = miMsg { mi_irq = toKeepBool (Maybe.isJust mirq), mi_irqList = unpack $ resize $ pack irqList, mi_irqNumber = Maybe.fromMaybe 0 mirq }
    mi = setIrq . maybe mmMasterInNoData (\n -> convSoMi (so !! n) (xferSt !! n)) <$> ms
    si = maybe (const mmSlaveInNoData) (\n -> convMoSi (mo !! n)) <$> sm <*> xferSt
    xferSt' = modifySt <$> (fmap (mo !!) <$> sm) <*> so <*> xferSt

  minIrq so = fold minJust $ irqNum <$> so <*> irqNums where
    minJust (Just a) (Just b) | a < b = Just a
    minJust (Just a) Nothing = Just a
    minJust _ b = b

    irqNum soMsg num = if fromKeepBool False (so_irq soMsg) then Just num else Nothing

  masterSlavePairings mo smOld xferSt = (ms, sm) where
    smOld' = (\smOldElem addrFn xferStI -> smOldElem >>= keepSM addrFn xferStI) <$> smOld <*> slaveAddrFns <*> xferSt
    keepSM addrFn xferStI idx = if (moIsOn (mo !! idx) && addrFn (mo_addr (mo !! idx))) || Maybe.isJust xferStI then Just idx else Nothing
    smCurr = (\addrFn -> findIndex (addrFn . mo_addr) mo) <$> slaveAddrFns
    sm = (<|>) <$> smOld' <*> smCurr
    ms = flip elemIndex sm . Just <$> iterateI (+ 1) 0

  moIsOn mo = (fromKeepBool True (mo_read mo) || fromKeepBool True (mo_write mo)) && (0 /= fromMaybeEmptyNum 1 (mo_byteEnable mo))

  modifySt Nothing _ _ = Nothing
  modifySt (Just mo) so st = modifySt' so (Maybe.fromMaybe (0 :: Unsigned 8,
                                           mo_burstCount mo,
                                           errorX "interconnect fabric: this gets overwritten later"
                                           ) st)
  modifySt' so (ctr, ctr2, _) = modifySt'' (optDecStCtr so $ if not (fromKeepBool False $ so_waitRequest so) then ctr+1 else ctr,
                                            optDecStCtr so ctr2,
                                            fromKeepBool False $ so_waitRequest so)
  optDecStCtr so ctr = if (ctr /= 0) && (fromKeepBool True $ so_readDataValid so) then ctr-1 else ctr
  modifySt'' (0, 0, _) = Nothing
  modifySt'' st = Just st

  convSoMi so st
    = AvalonMasterIn
    { mi_waitRequest   = toKeepBool $ (Maybe.maybe True (\(_,_,cnt) -> cnt < maxBound) st) && (fromKeepBool False (so_waitRequest so))
    , mi_readDataValid = convKeepBool False (so_readDataValid so)
    , mi_endOfPacket   = convKeepBool False (so_endOfPacket so)
    , mi_irq           = errorX "interconnect fabric: this value gets overwritten later"
    , mi_irqList       = errorX "interconnect fabric: this value gets overwritten later"
    , mi_irqNumber     = errorX "interconnect fabric: this value gets overwritten later"
    , mi_readData      = so_readData so
    }

  convMoSi mo st
    = AvalonSlaveIn
    { si_addr               = mo_addr mo
    , si_read               = toKeepBool $ fromKeepBool True (mo_read  mo) && not (fromKeepBool False (mo_write mo))
    , si_write              = toKeepBool $ fromKeepBool True (mo_write mo) && not (fromKeepBool False (mo_read  mo))
    , si_writeByteEnable    = resize $ if (fromKeepBool True (mo_write mo)) then mo_byteEnable mo else 0
    , si_burstCount         = mo_burstCount mo
    , si_chipSelect         = toKeepBool True
    , si_byteEnable         = resize $ mo_byteEnable mo
    , si_beginTransfer      = toKeepBool $ Maybe.maybe True (\(_,_,lastWaitReq) -> lastWaitReq) st
    , si_beginBurstTransfer = toKeepBool $ Maybe.isNothing st
    , si_writeData          = mo_writeData mo
    }

-- TODO support for fixed wait time


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
  { mi_waitRequest   = toKeepBool (not ack)
  , mi_readDataValid = toKeepBool False
  , mi_endOfPacket   = toKeepBool False
  , mi_irq           = toKeepBool False
  , mi_irqList       = 0
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No readData for boolToAck"
  }

mmMasterInNoData :: (GoodMMMasterConfig config) => AvalonMasterIn config readDataType
mmMasterInNoData
  = AvalonMasterIn
  { mi_waitRequest   = toKeepBool False
  , mi_readDataValid = toKeepBool False
  , mi_endOfPacket   = toKeepBool False
  , mi_irq           = toKeepBool False
  , mi_irqList       = 0
  , mi_irqNumber     = 0
  , mi_readData      = errorX "No read data defined"
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

mmMasterInToBool :: (GoodMMMasterConfig config) => AvalonMasterIn config readDataType -> Bool
mmMasterInToBool = not . fromKeepBool False . mi_waitRequest

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

mmMasterOutSendingData :: (GoodMMMasterConfig config) => AvalonMasterOut config writeDataType
mmMasterOutSendingData
  = AvalonMasterOut
  { mo_addr        = 0
  , mo_read        = toKeepBool False
  , mo_write       = toKeepBool True
  , mo_byteEnable  = bitCoerce $ repeat True
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
       && 0 /= fromMaybeEmptyNum 1 (si_byteEnable si)
       && 0 /= fromMaybeEmptyNum 1 (si_writeByteEnable si)

mmMasterOutToMaybe :: (GoodMMMasterConfig config) => AvalonMasterOut config writeDataType -> Maybe writeDataType
mmMasterOutToMaybe mo = if cond then Just (mo_writeData mo) else Nothing where
  cond =  fromKeepBool True (mo_write mo)
       && not (fromKeepBool False (mo_read mo))
       && 0 /= fromMaybeEmptyNum 1 (mo_byteEnable mo)

-- TODO rename master to whatever they changed it to
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
