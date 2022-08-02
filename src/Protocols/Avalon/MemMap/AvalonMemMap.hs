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

-- clash-prelude
import           Clash.Prelude hiding (take, concat, length)
import qualified Clash.Prelude as C

-- me
import           Protocols.Internal
import qualified Protocols.DfConv as DfConv
import qualified Protocols.Df as Df
import           Protocols.Hedgehog.Internal


-- Config needed for both manager and subordinate interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
data AvalonMMSharedConfig
  =  AvalonMMSharedConfig
  { dataWidth         :: Nat
  , keepReadData      :: Bool
  , keepWriteData     :: Bool
  , addrWidth         :: Nat
  , keepRead          :: Bool
  , keepWrite         :: Bool
  , byteEnableWidth   :: Nat
  , keepByteEnable    :: Bool
  , burstCountWidth   :: Nat
  , keepBurstCount    :: Bool
  , keepReadDataValid :: Bool
  , keepEndOfPacket   :: Bool
  }

-- Config specific to Avalon MM subordinate interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
-- An @AvalonMMSharedConfig@ is also included for the rest of the fields.
data AvalonMMSubordinateConfig
  =  AvalonMMSubordinateConfig
  { keepAddr               :: Bool
  , keepWriteByteEnable    :: Bool
  , keepChipSelect         :: Bool
  , keepBeginTransfer      :: Bool
  , keepWaitRequest        :: Bool
  , keepBeginBurstTransfer :: Bool
  , keepReadyForData       :: Bool
  , keepDataAvailable      :: Bool
  , keepIrq                :: Bool
  , sShared                :: AvalonMMSharedConfig
  }

-- Config specific to Avalon MM manager interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
-- An @AvalonMMSharedConfig@ is also included for the rest of the fields.
data AvalonMMManagerConfig
  =  AvalonMMManagerConfig
  { keepFlush      :: Bool
  , keepIrqList    :: Bool
  , keepIrqNumber  :: Bool
  , mShared        :: AvalonMMSharedConfig
  }

-- Grab record fields at the type level:

type family DataWidth (c :: AvalonMMSharedConfig) where
  DataWidth ('AvalonMMSharedConfig a _ _ _ _ _ _ _ _ _ _ _) = a

type family KeepReadData (c :: AvalonMMSharedConfig) where
  KeepReadData ('AvalonMMSharedConfig _ a _ _ _ _ _ _ _ _ _ _) = a

type family KeepWriteData (c :: AvalonMMSharedConfig) where
  KeepWriteData ('AvalonMMSharedConfig _ _ a _ _ _ _ _ _ _ _ _) = a

type family AddrWidth (c :: AvalonMMSharedConfig) where
  AddrWidth ('AvalonMMSharedConfig _ _ _ a _ _ _ _ _ _ _ _) = a

type family KeepRead (c :: AvalonMMSharedConfig) where
  KeepRead ('AvalonMMSharedConfig _ _ _ _ a _ _ _ _ _ _ _) = a

type family KeepWrite (c :: AvalonMMSharedConfig) where
  KeepWrite ('AvalonMMSharedConfig _ _ _ _ _ a _ _ _ _ _ _) = a

type family ByteEnableWidth (c :: AvalonMMSharedConfig) where
  ByteEnableWidth ('AvalonMMSharedConfig _ _ _ _ _ _ a _ _ _ _ _) = a

type family KeepByteEnable (c :: AvalonMMSharedConfig) where
  KeepByteEnable ('AvalonMMSharedConfig _ _ _ _ _ _ _ a _ _ _ _) = a

type family BurstCountWidth (c :: AvalonMMSharedConfig) where
  BurstCountWidth ('AvalonMMSharedConfig _ _ _ _ _ _ _ _ a _ _ _) = a

type family KeepBurstCount (c :: AvalonMMSharedConfig) where
  KeepBurstCount ('AvalonMMSharedConfig _ _ _ _ _ _ _ _ _ a _ _) = a

type family KeepReadDataValid (c :: AvalonMMSharedConfig) where
  KeepReadDataValid ('AvalonMMSharedConfig _ _ _ _ _ _ _ _ _ _ a _) = a

type family KeepEndOfPacket (c :: AvalonMMSharedConfig) where
  KeepEndOfPacket ('AvalonMMSharedConfig _ _ _ _ _ _ _ _ _ _ _ a) = a


type family KeepAddr (c :: AvalonMMSubordinateConfig) where
  KeepAddr ('AvalonMMSubordinateConfig a _ _ _ _ _ _ _ _ _) = a

type family KeepWriteByteEnable (c :: AvalonMMSubordinateConfig) where
  KeepWriteByteEnable ('AvalonMMSubordinateConfig _ a _ _ _ _ _ _ _ _) = a

type family KeepChipSelect (c :: AvalonMMSubordinateConfig) where
  KeepChipSelect ('AvalonMMSubordinateConfig _ _ a _ _ _ _ _ _ _) = a

type family KeepBeginTransfer (c :: AvalonMMSubordinateConfig) where
  KeepBeginTransfer ('AvalonMMSubordinateConfig _ _ _ a _ _ _ _ _ _) = a

type family KeepWaitRequest (c :: AvalonMMSubordinateConfig) where
  KeepWaitRequest ('AvalonMMSubordinateConfig _ _ _ _ a _ _ _ _ _) = a

type family KeepBeginBurstTransfer (c :: AvalonMMSubordinateConfig) where
  KeepBeginBurstTransfer ('AvalonMMSubordinateConfig _ _ _ _ _ a _ _ _ _) = a

type family KeepReadyForData (c :: AvalonMMSubordinateConfig) where
  KeepReadyForData ('AvalonMMSubordinateConfig _ _ _ _ _ _ a _ _ _) = a

type family KeepDataAvailable (c :: AvalonMMSubordinateConfig) where
  KeepDataAvailable ('AvalonMMSubordinateConfig _ _ _ _ _ _ _ a _ _) = a

type family KeepIrq (c :: AvalonMMSubordinateConfig) where
  KeepIrq ('AvalonMMSubordinateConfig _ _ _ _ _ _ _ _ a _) = a

type family SShared (c :: AvalonMMSubordinateConfig) where
  SShared ('AvalonMMSubordinateConfig _ _ _ _ _ _ _ _ _ a) = a


type family KeepFlush (c :: AvalonMMManagerConfig) where
  KeepFlush ('AvalonMMManagerConfig a _ _ _) = a

type family KeepIrqList (c :: AvalonMMManagerConfig) where
  KeepIrqList ('AvalonMMManagerConfig _ a _ _) = a

type family KeepIrqNumber (c :: AvalonMMManagerConfig) where
  KeepIrqNumber ('AvalonMMManagerConfig _ _ a _) = a

type family MShared (c :: AvalonMMManagerConfig) where
  MShared ('AvalonMMManagerConfig _ _ _ a) = a


type family RemoveNonDfSubordinate (cfg :: AvalonMMSubordinateConfig) where
  RemoveNonDfSubordinate cfg
    = 'AvalonMMSubordinateConfig
      (KeepAddr cfg)
      (KeepWriteByteEnable cfg)
      (KeepChipSelect cfg)
      (KeepBeginTransfer cfg)
      (KeepWaitRequest cfg)
      (KeepBeginBurstTransfer cfg)
      'False
      'False
      'False
      (SShared cfg)

type family RemoveNonDfManager (cfg :: AvalonMMManagerConfig) where
  RemoveNonDfManager cfg
    = 'AvalonMMManagerConfig
      (KeepFlush cfg)
      'False
      'False
      (MShared cfg)


-- TODO representing a well-behaved shared config.
-- This class holds for every possible @AvalonMMSharedConfig@,
-- but we need to write out the class anyway so that GHC holds.
type GoodMMSharedConfig config =
  ( KnownNat      (DataWidth         config)
  , KeepTypeClass (KeepReadData      config)
  , KeepTypeClass (KeepWriteData     config)
  , KnownNat      (AddrWidth         config)
  , KeepTypeClass (KeepRead          config)
  , KeepTypeClass (KeepWrite         config)
  , KnownNat      (ByteEnableWidth   config)
  , KeepTypeClass (KeepByteEnable    config)
  , KnownNat      (BurstCountWidth   config)
  , KeepTypeClass (KeepBurstCount    config)
  , KeepTypeClass (KeepReadDataValid config)
  , KeepTypeClass (KeepEndOfPacket   config)

  , NFDataX (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , NFData  (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , ShowX   (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , Show    (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , Eq      (KeepType (KeepReadData config) (Unsigned (DataWidth config)))

  , NFDataX (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , NFData  (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , ShowX   (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , Show    (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , Eq      (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))

  , NFDataX (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , NFData  (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , ShowX   (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , Show    (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , Eq      (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))

  , NFDataX (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , NFData  (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , ShowX   (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , Show    (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , Eq      (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  )

-- Class representing a well-behaved subordinate config.
-- This class holds for every possible @AvalonMMSubordinateConfig@,
-- but we need to write out the class anyway so that GHC holds.
type GoodMMSubordinateConfig config =
  ( KeepTypeClass (KeepAddr               config)
  , KeepTypeClass (KeepWriteByteEnable    config)
  , KeepTypeClass (KeepChipSelect         config)
  , KeepTypeClass (KeepBeginTransfer      config)
  , KeepTypeClass (KeepWaitRequest        config)
  , KeepTypeClass (KeepBeginBurstTransfer config)
  , KeepTypeClass (KeepReadyForData       config)
  , KeepTypeClass (KeepDataAvailable      config)
  , KeepTypeClass (KeepIrq                config)
  , GoodMMSharedConfig (SShared           config)

  , NFDataX (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , NFData  (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , ShowX   (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , Show    (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , Eq      (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))

  , NFDataX (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , NFData  (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , ShowX   (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , Show    (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , Eq      (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  )

-- Class representing a well-behaved manager config.
-- This class holds for every possible @AvalonMMManagerConfig@,
-- but we need to write out the class anyway so that GHC holds.
type GoodMMManagerConfig config =
  ( KeepTypeClass (KeepFlush     config)
  , KeepTypeClass (KeepIrqList   config)
  , KeepTypeClass (KeepIrqNumber config)
  , GoodMMSharedConfig (MShared  config)

  , NFDataX (KeepType (KeepIrqList config) (Unsigned 32))
  , NFData  (KeepType (KeepIrqList config) (Unsigned 32))
  , ShowX   (KeepType (KeepIrqList config) (Unsigned 32))
  , Show    (KeepType (KeepIrqList config) (Unsigned 32))
  , Eq      (KeepType (KeepIrqList config) (Unsigned 32))

  , NFDataX (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , NFData  (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , ShowX   (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , Show    (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , Eq      (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  )


-- Data coming out of an Avalon MM manager port.
-- All fields are optional and can be toggled using the config.
data AvalonManagerOut config
  =  AvalonManagerOut
  { mo_writeData   :: KeepType (KeepWriteData  (MShared config)) (Unsigned (DataWidth (MShared config)))
  , mo_addr        :: Unsigned (AddrWidth      (MShared config))
  , mo_read        :: KeepType (KeepRead       (MShared config)) Bool
  , mo_write       :: KeepType (KeepWrite      (MShared config)) Bool
  , mo_byteEnable  :: KeepType (KeepByteEnable (MShared config)) (Unsigned (ByteEnableWidth (MShared config)))
  , mo_burstCount  :: KeepType (KeepBurstCount (MShared config)) (Unsigned (BurstCountWidth (MShared config)))
  , mo_flush       :: KeepType (KeepFlush               config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMManagerConfig config)
                   => NFDataX (AvalonManagerOut config)
deriving instance (GoodMMManagerConfig config)
                   => NFData (AvalonManagerOut config)
deriving instance (GoodMMManagerConfig config)
                   => ShowX (AvalonManagerOut config)
deriving instance (GoodMMManagerConfig config)
                   => Show (AvalonManagerOut config)
deriving instance (GoodMMManagerConfig config)
                   => Eq (AvalonManagerOut config)


-- Data coming into an Avalon MM manager port.
-- Almost all fields are optional and can be toggled using the config.
-- WaitRequest is mandatory.
data AvalonManagerIn config
  =  AvalonManagerIn
  { mi_readData      :: KeepType (KeepReadData      (MShared config)) (Unsigned (DataWidth (MShared config)))
  , mi_waitRequest   :: Bool
  , mi_readDataValid :: KeepType (KeepReadDataValid (MShared config)) Bool
  , mi_endOfPacket   :: KeepType (KeepEndOfPacket   (MShared config)) Bool
  , mi_irqList       :: KeepType (KeepIrqList                config) (Unsigned 32)
  , mi_irqNumber     :: KeepType (KeepIrqNumber              config) (Maybe (Unsigned 6))
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMManagerConfig config)
                   => NFDataX (AvalonManagerIn config)
deriving instance (GoodMMManagerConfig config)
                   => NFData (AvalonManagerIn config)
deriving instance (GoodMMManagerConfig config)
                   => ShowX (AvalonManagerIn config)
deriving instance (GoodMMManagerConfig config)
                   => Show (AvalonManagerIn config)
deriving instance (GoodMMManagerConfig config)
                   => Eq (AvalonManagerIn config)

-- TODO
data AvalonManagerWriteImpt config
  =  AvalonManagerWriteImpt
  { mwi_writeData   :: KeepType (KeepWriteData  (MShared config)) (Unsigned (DataWidth (MShared config)))
  , mwi_addr        :: Unsigned (AddrWidth      (MShared config))
  , mwi_byteEnable  :: KeepType (KeepByteEnable (MShared config)) (Unsigned (ByteEnableWidth (MShared config)))
  , mwi_burstCount  :: KeepType (KeepBurstCount (MShared config)) (Unsigned (BurstCountWidth (MShared config)))
  , mwi_flush       :: KeepType (KeepFlush               config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMManagerConfig config)
                   => NFDataX (AvalonManagerWriteImpt config)
deriving instance (GoodMMManagerConfig config)
                   => NFData (AvalonManagerWriteImpt config)
deriving instance (GoodMMManagerConfig config)
                   => ShowX (AvalonManagerWriteImpt config)
deriving instance (GoodMMManagerConfig config)
                   => Show (AvalonManagerWriteImpt config)
deriving instance (GoodMMManagerConfig config)
                   => Eq (AvalonManagerWriteImpt config)


-- TODO
data AvalonManagerReadReqImpt config
  =  AvalonManagerReadReqImpt
  { mrri_addr        :: Unsigned (AddrWidth      (MShared config))
  , mrri_byteEnable  :: KeepType (KeepByteEnable (MShared config)) (Unsigned (ByteEnableWidth (MShared config)))
  , mrri_burstCount  :: KeepType (KeepBurstCount (MShared config)) (Unsigned (BurstCountWidth (MShared config)))
  , mrri_flush       :: KeepType (KeepFlush               config) Bool
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
data AvalonManagerReadImpt config
  =  AvalonManagerReadImpt
  { mri_readData    :: KeepType (KeepReadData    (MShared config)) (Unsigned (DataWidth (MShared config)))
  , mri_endOfPacket :: KeepType (KeepEndOfPacket (MShared config)) Bool
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMManagerConfig config)
                   => NFDataX (AvalonManagerReadImpt config)
deriving instance (GoodMMManagerConfig config)
                   => NFData (AvalonManagerReadImpt config)
deriving instance (GoodMMManagerConfig config)
                   => ShowX (AvalonManagerReadImpt config)
deriving instance (GoodMMManagerConfig config)
                   => Show (AvalonManagerReadImpt config)
deriving instance (GoodMMManagerConfig config)
                   => Eq (AvalonManagerReadImpt config)


-- Data coming out of an Avalon MM subordinate port.
-- All fields are optional and can be toggled using the config.
data AvalonSubordinateOut config
  =  AvalonSubordinateOut
  { so_readData      :: KeepType (KeepReadData      (SShared config)) (Unsigned (DataWidth (SShared config)))
  , so_readDataValid :: KeepType (KeepReadDataValid (SShared config)) Bool
  , so_endOfPacket   :: KeepType (KeepEndOfPacket   (SShared config)) Bool
  , so_waitRequest   :: KeepType (KeepWaitRequest            config) Bool
  , so_readyForData  :: KeepType (KeepReadyForData           config) Bool
  , so_dataAvailable :: KeepType (KeepDataAvailable          config) Bool
  , so_irq           :: KeepType (KeepIrq                    config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSubordinateConfig config)
                   => NFDataX (AvalonSubordinateOut config)
deriving instance (GoodMMSubordinateConfig config)
                   => NFData (AvalonSubordinateOut config)
deriving instance (GoodMMSubordinateConfig config)
                   => ShowX (AvalonSubordinateOut config)
deriving instance (GoodMMSubordinateConfig config)
                   => Show (AvalonSubordinateOut config)
deriving instance (GoodMMSubordinateConfig config)
                   => Eq (AvalonSubordinateOut config)


-- Data coming into an Avalon MM subordinate port.
-- All fields are optional and can be toggled using the config.
data AvalonSubordinateIn config
  = AvalonSubordinateIn
  { si_writeData          :: KeepType (KeepWriteData          (SShared config)) (Unsigned (DataWidth (SShared config)))
  , si_addr               :: KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config)))
  , si_read               :: KeepType (KeepRead               (SShared config)) Bool
  , si_write              :: KeepType (KeepWrite              (SShared config)) Bool
  , si_byteEnable         :: KeepType (KeepByteEnable         (SShared config)) (Unsigned (ByteEnableWidth (SShared config)))
  , si_burstCount         :: KeepType (KeepBurstCount         (SShared config)) (Unsigned (BurstCountWidth (SShared config)))
  , si_writeByteEnable    :: KeepType (KeepWriteByteEnable             config)  (Unsigned (ByteEnableWidth (SShared config)))
  , si_chipSelect         :: KeepType (KeepChipSelect                  config) Bool
  , si_beginTransfer      :: KeepType (KeepBeginTransfer               config) Bool
  , si_beginBurstTransfer :: KeepType (KeepBeginBurstTransfer          config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSubordinateConfig config)
                   => NFDataX (AvalonSubordinateIn config)
deriving instance (GoodMMSubordinateConfig config)
                   => NFData (AvalonSubordinateIn config)
deriving instance (GoodMMSubordinateConfig config)
                   => ShowX (AvalonSubordinateIn config)
deriving instance (GoodMMSubordinateConfig config)
                   => Show (AvalonSubordinateIn config)
deriving instance (GoodMMSubordinateConfig config)
                   => Eq (AvalonSubordinateIn config)


-- TODO
data AvalonSubordinateWriteImpt config
  = AvalonSubordinateWriteImpt
  { swi_writeData          :: KeepType (KeepWriteData          (SShared config)) (Unsigned (DataWidth (SShared config)))
  , swi_addr               :: KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config)))
  , swi_byteEnable         :: KeepType (KeepByteEnable         (SShared config)) (Unsigned (ByteEnableWidth        (SShared config)))
  , swi_burstCount         :: KeepType (KeepBurstCount         (SShared config)) (Unsigned (BurstCountWidth        (SShared config)))
  , swi_beginTransfer      :: KeepType (KeepBeginTransfer               config) Bool
  , swi_beginBurstTransfer :: KeepType (KeepBeginBurstTransfer          config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSubordinateConfig config)
                   => NFDataX (AvalonSubordinateWriteImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => NFData (AvalonSubordinateWriteImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => ShowX (AvalonSubordinateWriteImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => Show (AvalonSubordinateWriteImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => Eq (AvalonSubordinateWriteImpt config)


-- TODO
data AvalonSubordinateReadReqImpt config
  = AvalonSubordinateReadReqImpt
  { srri_addr               :: KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config)))
  , srri_byteEnable         :: KeepType (KeepByteEnable         (SShared config)) (Unsigned (ByteEnableWidth        (SShared config)))
  , srri_burstCount         :: KeepType (KeepBurstCount         (SShared config)) (Unsigned (BurstCountWidth        (SShared config)))
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
data AvalonSubordinateReadImpt config
  = AvalonSubordinateReadImpt
  { sri_readData    :: KeepType (KeepReadData    (SShared config)) (Unsigned (DataWidth (SShared config)))
  , sri_endOfPacket :: KeepType (KeepEndOfPacket (SShared config)) Bool
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSubordinateConfig config)
                   => NFDataX (AvalonSubordinateReadImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => NFData (AvalonSubordinateReadImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => ShowX (AvalonSubordinateReadImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => Show (AvalonSubordinateReadImpt config)
deriving instance (GoodMMSubordinateConfig config)
                   => Eq (AvalonSubordinateReadImpt config)

-- TODO irq, fixed wait time
interconnectFabric ::
  forall dom managerConfig subordinateConfig numManager numSubordinate.
  ( GoodMMManagerConfig managerConfig
  , GoodMMSubordinateConfig subordinateConfig
  , MShared managerConfig ~ SShared subordinateConfig
  , HiddenClockResetEnable dom
  , KnownNat numManager
  , KnownNat numSubordinate
  ) =>
  (Unsigned (AddrWidth (SShared subordinateConfig)) -> Maybe (Index numSubordinate)) ->
  Circuit
    (Vec numManager (AvalonMMManager dom managerConfig))
    (Vec numSubordinate (AvalonMMSubordinate dom 0 {- TODO (this is the wait time) -} subordinateConfig))
interconnectFabric addrFn
  =  DfConv.interconnect Proxy dfMiddle reqFn
  |> vecCircuits (repeat (DfConv.mapBoth dfMiddle Proxy mapFwd mapBwd))
 where
  dfMiddle = Proxy @(Df.Df dom _, Reverse (Df.Df dom _))
  reqFn (AvalonManagerOut{..})
    | not (fromKeepTypeDef True mo_read || fromKeepTypeDef True mo_write) = Nothing
    | otherwise = addrFn mo_addr
  mapFwd (Left AvalonManagerReadReqImpt{..})
    = Left AvalonSubordinateReadReqImpt
    { srri_addr               = toKeepType mrri_addr
    , srri_byteEnable         = mrri_byteEnable
    , srri_burstCount         = mrri_burstCount
    , srri_beginTransfer      = undefined -- TODO
    , srri_beginBurstTransfer = undefined -- TODO
    }
  mapFwd (Right AvalonManagerWriteImpt{..})
    = Right AvalonSubordinateWriteImpt
    { swi_addr               = toKeepType mwi_addr
    , swi_byteEnable         = mwi_byteEnable
    , swi_burstCount         = mwi_burstCount
    , swi_beginTransfer      = undefined -- TODO
    , swi_beginBurstTransfer = undefined -- TODO
    , swi_writeData          = mwi_writeData
    }
  mapBwd AvalonSubordinateReadImpt{..}
    = AvalonManagerReadImpt
    { mri_endOfPacket = sri_endOfPacket
    , mri_readData    = sri_readData
    }

managerOutAddNonDf ::
{-
  ( GoodMMManagerConfig cfgA
  , GoodMMManagerConfig cfgB
  , KeepFlush cfgA ~ KeepFlush cfgB
  , MShared cfgA ~ MShared cfgB
  , KeepIrqList cfgA ~ 'False
  , KeepIrqNumber cfgA ~ 'False ) =>
  AvalonManagerOut cfgA ->
  AvalonManagerOut cfgB
-}
  GoodMMManagerConfig cfg =>
  AvalonManagerOut (RemoveNonDfManager cfg) ->
  AvalonManagerOut cfg
managerOutAddNonDf AvalonManagerOut{..} = AvalonManagerOut{..}

managerOutRemoveNonDf ::
{-
  ( GoodMMManagerConfig cfgA
  , GoodMMManagerConfig cfgB
  , KeepFlush cfgA ~ KeepFlush cfgB
  , MShared cfgA ~ MShared cfgB
  , KeepIrqList cfgA ~ 'False
  , KeepIrqNumber cfgA ~ 'False ) =>
  AvalonManagerOut cfgB ->
  AvalonManagerOut cfgA
-}
  GoodMMManagerConfig cfg =>
  AvalonManagerOut cfg ->
  AvalonManagerOut (RemoveNonDfManager cfg)
managerOutRemoveNonDf AvalonManagerOut{..} = AvalonManagerOut{..}

managerInAddNonDf ::
{-
  ( GoodMMManagerConfig cfgA
  , GoodMMManagerConfig cfgB
  , KeepFlush cfgA ~ KeepFlush cfgB
  , MShared cfgA ~ MShared cfgB
  , KeepIrqList cfgA ~ 'False
  , KeepIrqNumber cfgA ~ 'False ) =>
  ( KeepType (KeepIrqList cfgB) (Unsigned 32)
  , KeepType (KeepIrqNumber cfgB) (Maybe (Unsigned 6)) ) ->
  AvalonManagerIn cfgA ->
  AvalonManagerIn cfgB
-}
  GoodMMManagerConfig cfg =>
  ( KeepType (KeepIrqList cfg) (Unsigned 32)
  , KeepType (KeepIrqNumber cfg) (Maybe (Unsigned 6)) ) ->
  AvalonManagerIn (RemoveNonDfManager cfg) ->
  AvalonManagerIn cfg
managerInAddNonDf (irqList, irqNumber) AvalonManagerIn{..}
  = AvalonManagerIn
  { mi_readData, mi_waitRequest, mi_readDataValid, mi_endOfPacket
  , mi_irqList = irqList
  , mi_irqNumber = irqNumber
  }

managerInRemoveNonDf ::
{-
  ( GoodMMManagerConfig cfgA
  , GoodMMManagerConfig cfgB
  , KeepFlush cfgA ~ KeepFlush cfgB
  , MShared cfgA ~ MShared cfgB
  , KeepIrqList cfgA ~ 'False
  , KeepIrqNumber cfgA ~ 'False ) =>
  AvalonManagerIn cfgB ->
  ( AvalonManagerIn cfgA
  , ( KeepType (KeepIrqList cfgB) (Unsigned 32)
    , KeepType (KeepIrqNumber cfgB) (Maybe (Unsigned 6)) ) )
-}
  GoodMMManagerConfig cfg =>
  AvalonManagerIn cfg ->
  ( AvalonManagerIn (RemoveNonDfManager cfg)
  , ( KeepType (KeepIrqList cfg) (Unsigned 32)
    , KeepType (KeepIrqNumber cfg) (Maybe (Unsigned 6)) ) )
managerInRemoveNonDf AvalonManagerIn{..}
  = (AvalonManagerIn
  { mi_readData, mi_waitRequest, mi_readDataValid, mi_endOfPacket
  , mi_irqList = Proxy
  , mi_irqNumber = Proxy
  }, (mi_irqList, mi_irqNumber))

subordinateOutAddNonDf ::
{-
  ( GoodMMSubordinateConfig cfgA
  , GoodMMSubordinateConfig cfgB
  , KeepAddr cfgA ~ KeepAddr cfgB
  , KeepWriteByteEnable cfgA ~ KeepWriteByteEnable cfgB
  , KeepChipSelect cfgA ~ KeepChipSelect cfgB
  , KeepBeginTransfer cfgA ~ KeepBeginTransfer cfgB
  , KeepWaitRequest cfgA ~ KeepWaitRequest cfgB
  , KeepBeginBurstTransfer cfgA ~ KeepBeginBurstTransfer cfgB
  , SShared cfgA ~ SShared cfgB
  , KeepReadyForData cfgA ~ 'False
  , KeepDataAvailable cfgA ~ 'False
  , KeepIrq cfgA ~ 'False ) =>
  ( KeepType (KeepReadyForData cfgB) Bool
  , KeepType (KeepDataAvailable cfgB) Bool
  , KeepType (KeepIrq cfgB) Bool ) ->
  AvalonSubordinateOut cfgA ->
  AvalonSubordinateOut cfgB
-}
  GoodMMSubordinateConfig cfg =>
  ( KeepType (KeepReadyForData cfg) Bool
  , KeepType (KeepDataAvailable cfg) Bool
  , KeepType (KeepIrq cfg) Bool ) ->
  AvalonSubordinateOut (RemoveNonDfSubordinate cfg) ->
  AvalonSubordinateOut cfg
subordinateOutAddNonDf (readyForData, dataAvailable, irq) AvalonSubordinateOut{..}
  = AvalonSubordinateOut
  { so_waitRequest, so_readDataValid, so_endOfPacket, so_readData
  , so_readyForData = readyForData
  , so_dataAvailable = dataAvailable
  , so_irq = irq
  }

subordinateOutRemoveNonDf ::
{-
  ( GoodMMSubordinateConfig cfgA
  , GoodMMSubordinateConfig cfgB
  , KeepAddr cfgA ~ KeepAddr cfgB
  , KeepWriteByteEnable cfgA ~ KeepWriteByteEnable cfgB
  , KeepChipSelect cfgA ~ KeepChipSelect cfgB
  , KeepBeginTransfer cfgA ~ KeepBeginTransfer cfgB
  , KeepWaitRequest cfgA ~ KeepWaitRequest cfgB
  , KeepBeginBurstTransfer cfgA ~ KeepBeginBurstTransfer cfgB
  , SShared cfgA ~ SShared cfgB
  , KeepReadyForData cfgA ~ 'False
  , KeepDataAvailable cfgA ~ 'False
  , KeepIrq cfgA ~ 'False ) =>
  AvalonSubordinateOut cfgB ->
  ( AvalonSubordinateOut cfgA
  , ( KeepType (KeepReadyForData cfgB) Bool
    , KeepType (KeepDataAvailable cfgB) Bool
    , KeepType (KeepIrq cfgB) Bool ) )
-}
  GoodMMSubordinateConfig cfg =>
  AvalonSubordinateOut cfg ->
  ( AvalonSubordinateOut (RemoveNonDfSubordinate cfg)
  , ( KeepType (KeepReadyForData cfg) Bool
    , KeepType (KeepDataAvailable cfg) Bool
    , KeepType (KeepIrq cfg) Bool ) )
subordinateOutRemoveNonDf AvalonSubordinateOut{..}
  = (AvalonSubordinateOut
  { so_waitRequest, so_readDataValid, so_endOfPacket, so_readData
  , so_readyForData = Proxy
  , so_dataAvailable = Proxy
  , so_irq = Proxy
  }, (so_readyForData, so_dataAvailable, so_irq))

subordinateInAddNonDf ::
{-
  ( GoodMMSubordinateConfig cfgA
  , GoodMMSubordinateConfig cfgB
  , KeepAddr cfgA ~ KeepAddr cfgB
  , KeepWriteByteEnable cfgA ~ KeepWriteByteEnable cfgB
  , KeepChipSelect cfgA ~ KeepChipSelect cfgB
  , KeepBeginTransfer cfgA ~ KeepBeginTransfer cfgB
  , KeepWaitRequest cfgA ~ KeepWaitRequest cfgB
  , KeepBeginBurstTransfer cfgA ~ KeepBeginBurstTransfer cfgB
  , SShared cfgA ~ SShared cfgB
  , KeepReadyForData cfgA ~ 'False
  , KeepDataAvailable cfgA ~ 'False
  , KeepIrq cfgA ~ 'False ) =>
  AvalonSubordinateIn cfgA ->
  AvalonSubordinateIn cfgB
-}
  GoodMMSubordinateConfig cfg =>
  AvalonSubordinateIn (RemoveNonDfSubordinate cfg) ->
  AvalonSubordinateIn cfg
subordinateInAddNonDf AvalonSubordinateIn{..} = AvalonSubordinateIn{..}

subordinateInRemoveNonDf ::
{-
  ( GoodMMSubordinateConfig cfgA
  , GoodMMSubordinateConfig cfgB
  , KeepAddr cfgA ~ KeepAddr cfgB
  , KeepWriteByteEnable cfgA ~ KeepWriteByteEnable cfgB
  , KeepChipSelect cfgA ~ KeepChipSelect cfgB
  , KeepBeginTransfer cfgA ~ KeepBeginTransfer cfgB
  , KeepWaitRequest cfgA ~ KeepWaitRequest cfgB
  , KeepBeginBurstTransfer cfgA ~ KeepBeginBurstTransfer cfgB
  , SShared cfgA ~ SShared cfgB
  , KeepReadyForData cfgA ~ 'False
  , KeepDataAvailable cfgA ~ 'False
  , KeepIrq cfgA ~ 'False ) =>
-}
  GoodMMSubordinateConfig cfg =>
  AvalonSubordinateIn cfg ->
  AvalonSubordinateIn (RemoveNonDfSubordinate cfg)
subordinateInRemoveNonDf AvalonSubordinateIn{..} = AvalonSubordinateIn{..}

-- Convert a boolean value to an @AvalonSubordinateOut@ structure.
-- The structure gives no read data, no IRQ, etc.
-- Fields relating to "acknowledging" a write are controlled by the bool input.
boolToMMSubordinateAck :: (GoodMMSubordinateConfig config) => Bool -> AvalonSubordinateOut config
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
mmSubordinateReadDat :: (GoodMMSubordinateConfig config) => AvalonSubordinateReadImpt config -> AvalonSubordinateOut config
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
mmSubordinateOutToReadData :: (GoodMMSubordinateConfig config) => AvalonSubordinateOut config -> Maybe (KeepType (KeepReadData (SShared config)) (Unsigned (DataWidth (SShared config))))
mmSubordinateOutToReadData so
  = if (fromKeepTypeDef True (so_readDataValid so) && not (fromKeepTypeDef False (so_waitRequest so))) then Just (so_readData so) else Nothing

mmSubordinateOutToReadImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateOut config -> Maybe (AvalonSubordinateReadImpt config)
mmSubordinateOutToReadImpt (AvalonSubordinateOut{..})
  = if cond then Just AvalonSubordinateReadImpt
  { sri_endOfPacket = so_endOfPacket
  , sri_readData    = so_readData
  } else Nothing
  where
  cond = fromKeepTypeDef True so_readDataValid && not (fromKeepTypeDef False so_waitRequest)

mmManagerInToReadImpt :: (GoodMMManagerConfig config) => AvalonManagerIn config -> Maybe (AvalonManagerReadImpt config)
mmManagerInToReadImpt (AvalonManagerIn{..})
  = if cond then Just AvalonManagerReadImpt
  { mri_endOfPacket = mi_endOfPacket
  , mri_readData    = mi_readData
  } else Nothing
  where
  cond = not mi_waitRequest -- TODO anything else?

mmSubordinateInToWriteImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config -> Maybe (AvalonSubordinateWriteImpt config)
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
       && 0 /= fromKeepTypeDef 1 si_byteEnable
       && 0 /= fromKeepTypeDef 1 si_writeByteEnable

mmSubordinateInToReadReqImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config -> Maybe (AvalonSubordinateReadReqImpt config)
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

mmManagerOutToWriteImpt :: (GoodMMManagerConfig config) => AvalonManagerOut config -> Maybe (AvalonManagerWriteImpt config)
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
       && 0 /= fromKeepTypeDef 1 mo_byteEnable

mmManagerOutToReadReqImpt :: (GoodMMManagerConfig config) => AvalonManagerOut config -> Maybe (AvalonManagerReadReqImpt config)
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
       && 0 /= fromKeepTypeDef 1 mo_byteEnable

-- TODO comment
mmWriteImptToSubordinateIn :: (GoodMMSubordinateConfig config) => AvalonSubordinateWriteImpt config -> AvalonSubordinateIn config
mmWriteImptToSubordinateIn (AvalonSubordinateWriteImpt{..})
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType True
  , si_addr               = swi_addr
  , si_read               = toKeepType False
  , si_write              = toKeepType True
  , si_byteEnable         = swi_byteEnable
  , si_writeByteEnable    = toKeepType $ bitCoerce $ repeat True
  , si_beginTransfer      = swi_beginTransfer
  , si_burstCount         = swi_burstCount
  , si_beginBurstTransfer = swi_beginBurstTransfer
  , si_writeData          = swi_writeData
  }

-- TODO comment
mmReadReqImptToSubordinateIn :: (GoodMMSubordinateConfig config) => AvalonSubordinateReadReqImpt config -> AvalonSubordinateIn config
mmReadReqImptToSubordinateIn (AvalonSubordinateReadReqImpt{..})
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType False
  , si_addr               = srri_addr
  , si_read               = toKeepType True
  , si_write              = toKeepType False
  , si_byteEnable         = srri_byteEnable
  , si_writeByteEnable    = toKeepType 0
  , si_beginTransfer      = srri_beginTransfer
  , si_burstCount         = srri_burstCount
  , si_beginBurstTransfer = srri_beginBurstTransfer
  , si_writeData          = errorX "No writeData for read req"
  }

-- TODO comment
mmWriteImptToManagerOut :: (GoodMMManagerConfig config) => AvalonManagerWriteImpt config -> AvalonManagerOut config
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
mmReadReqImptToManagerOut :: (GoodMMManagerConfig config) => AvalonManagerReadReqImpt config -> AvalonManagerOut config
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
boolToMMManagerAck :: (GoodMMManagerConfig config) => Bool -> AvalonManagerIn config
boolToMMManagerAck ack
  = AvalonManagerIn
  { mi_waitRequest   = not ack
  , mi_readDataValid = toKeepType False
  , mi_endOfPacket   = toKeepType False
  , mi_irqList       = toKeepType 0
  , mi_irqNumber     = toKeepType Nothing
  , mi_readData      = errorX "No readData for boolToAck"
  }

-- An @AvalonManagerIn@ containing no read data, but not giving a wait request or an IRQ.
mmManagerInNoData :: (GoodMMManagerConfig config) => AvalonManagerIn config
mmManagerInNoData
  = AvalonManagerIn
  { mi_waitRequest   = False
  , mi_readDataValid = toKeepType False
  , mi_endOfPacket   = toKeepType False
  , mi_irqList       = toKeepType 0
  , mi_irqNumber     = toKeepType Nothing
  , mi_readData      = errorX "No read data defined"
  }

-- An @AvalonManagerIn@ TODO
mmManagerReadDat :: (GoodMMManagerConfig config) => AvalonManagerReadImpt config -> AvalonManagerIn config
mmManagerReadDat dat
  = AvalonManagerIn
  { mi_waitRequest   = False
  , mi_readDataValid = toKeepType True
  , mi_endOfPacket   = mri_endOfPacket dat
  , mi_irqList       = toKeepType 0
  , mi_irqNumber     = toKeepType Nothing
  , mi_readData      = mri_readData dat
  }

-- TODO
mmManagerInToReadData :: (GoodMMManagerConfig config) => AvalonManagerIn config -> Maybe (KeepType (KeepReadData (MShared config)) (Unsigned (DataWidth (MShared config))))
mmManagerInToReadData mi
  = if (fromKeepTypeDef True (mi_readDataValid mi) && not (mi_waitRequest mi)) then Just (mi_readData mi) else Nothing

-- An @AvalonSubordinateIn@ containing no write data, and indicating that no transmission is currently occurring.
mmSubordinateInNoData :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config
mmSubordinateInNoData
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType False
  , si_addr               = toKeepType 0
  , si_read               = toKeepType False
  , si_write              = toKeepType False
  , si_byteEnable         = toKeepType 0
  , si_writeByteEnable    = toKeepType 0
  , si_beginTransfer      = toKeepType False
  , si_burstCount         = toKeepType 0
  , si_beginBurstTransfer = toKeepType False
  , si_writeData          = errorX "No writeData for noData"
  }

-- An @AvalonSubordinateIn@ TODO
mmSubordinateInReadingData :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config
mmSubordinateInReadingData
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType False
  , si_addr               = toKeepType 0
  , si_read               = toKeepType True
  , si_write              = toKeepType False
  , si_byteEnable         = toKeepType 0
  , si_writeByteEnable    = toKeepType 0
  , si_beginTransfer      = toKeepType False
  , si_burstCount         = toKeepType 0
  , si_beginBurstTransfer = toKeepType False
  , si_writeData          = errorX "No writeData for noData"
  }

-- An @AvalonManagerOut@ containing no write data, and indicating that no transmission is currently occurring.
mmManagerOutNoData :: (GoodMMManagerConfig config) => AvalonManagerOut config
mmManagerOutNoData
  = AvalonManagerOut
  { mo_addr        = 0
  , mo_read        = toKeepType False
  , mo_write       = toKeepType False
  , mo_byteEnable  = toKeepType 0
  , mo_burstCount  = toKeepType 0
  , mo_flush       = toKeepType False
  , mo_writeData   = errorX "No writeData for noData"
  }

-- An @AvalonManagerOut@ TODO
mmManagerOutReadingData :: (GoodMMManagerConfig config) => AvalonManagerOut config
mmManagerOutReadingData
  = AvalonManagerOut
  { mo_addr        = 0
  , mo_read        = toKeepType True
  , mo_write       = toKeepType False
  , mo_byteEnable  = toKeepType 0
  , mo_burstCount  = toKeepType 0
  , mo_flush       = toKeepType False
  , mo_writeData   = errorX "No writeData for noData"
  }

-- Grab the "acknowledgement" value from an @AvalonSubordinateOut@.
-- Reasonable defaults are provided for optional fields.
mmSubordinateOutToBool :: (GoodMMSubordinateConfig config) => AvalonSubordinateOut config -> Bool
mmSubordinateOutToBool so = fromKeepTypeDef True (so_readyForData so) && not (fromKeepTypeDef False (so_waitRequest so))

-- Grab the "acknowledgement" value from an @AvalonManagerIn@.
-- Reasonable defaults are provided for optional fields.
mmManagerInToBool :: (GoodMMManagerConfig config) => AvalonManagerIn config -> Bool
mmManagerInToBool = not . mi_waitRequest

-- Default @AvalonSubordinateIn@ whose fields indicate that a write transaction is occurring.
-- The @writeData@ field needs to be filled in with data.
mmSubordinateInSendingData :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config
mmSubordinateInSendingData
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType True
  , si_addr               = toKeepType 0
  , si_read               = toKeepType False
  , si_write              = toKeepType True
  , si_byteEnable         = toKeepType $ bitCoerce $ repeat True
  , si_writeByteEnable    = toKeepType $ bitCoerce $ repeat True
  , si_beginTransfer      = toKeepType False
  , si_burstCount         = toKeepType 0
  , si_beginBurstTransfer = toKeepType False
  , si_writeData          = errorX "No writeData for mmSubordinateInSendingData"
  }

-- Default @AvalonManagerOut@ whose fields indicate that a write transaction is occurring.
-- The @writeData@ field needs to be filled in with data.
mmManagerOutSendingData :: (GoodMMManagerConfig config) => AvalonManagerOut config
mmManagerOutSendingData
  = AvalonManagerOut
  { mo_addr        = 0
  , mo_read        = toKeepType False
  , mo_write       = toKeepType True
  , mo_byteEnable  = toKeepType $ bitCoerce $ repeat True
  , mo_burstCount  = toKeepType 1
  , mo_flush       = toKeepType False
  , mo_writeData   = errorX "No writeData for mmManagerOutSendingData"
  }


-- Grab the data from an @AvalonSubordinateIn@, if there is any.
mmSubordinateInToMaybe :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config -> Maybe (KeepType (KeepWriteData (SShared config)) (Unsigned (DataWidth (SShared config))))
mmSubordinateInToMaybe si = if cond then Just (si_writeData si) else Nothing where
  cond =  fromKeepTypeDef True (si_chipSelect si)
       && fromKeepTypeDef True (si_write si)
       && not (fromKeepTypeDef False (si_read si))
       && 0 /= fromKeepTypeDef 1 (si_byteEnable si)
       && 0 /= fromKeepTypeDef 1 (si_writeByteEnable si)

-- Grab the data from an @AvalonManagerOut@, if there is any.
mmManagerOutToMaybe :: (GoodMMManagerConfig config) => AvalonManagerOut config -> Maybe (KeepType (KeepWriteData (MShared config)) (Unsigned (DataWidth (MShared config))))
mmManagerOutToMaybe mo = if cond then Just (mo_writeData mo) else Nothing where
  cond =  fromKeepTypeDef True (mo_write mo)
       && not (fromKeepTypeDef False (mo_read mo))
       && 0 /= fromKeepTypeDef 1 (mo_byteEnable mo)

-- TODO support fixed wait time in instances below

-- Datatype for the manager end of the Avalon memory-mapped protocol.
data AvalonMMManager (dom :: Domain) (config :: AvalonMMManagerConfig) = AvalonMMManager

-- Datatype for the subordinate end of the Avalon memory-mapped protocol.
data AvalonMMSubordinate (dom :: Domain) (fixedWaitTime :: Nat) (config :: AvalonMMSubordinateConfig) = AvalonMMSubordinate

instance Protocol (AvalonMMManager dom config) where
  type Fwd (AvalonMMManager dom config) = Signal dom (AvalonManagerOut config)
  type Bwd (AvalonMMManager dom config) = Signal dom (AvalonManagerIn config)

instance Protocol (AvalonMMSubordinate dom fixedWaitTime config) where
  type Fwd (AvalonMMSubordinate dom fixedWaitTime config) = Signal dom (AvalonSubordinateIn config)
  type Bwd (AvalonMMSubordinate dom fixedWaitTime config) = Signal dom (AvalonSubordinateOut config)

instance (GoodMMSubordinateConfig config, KeepWaitRequest config ~ 'True) => Backpressure (AvalonMMSubordinate dom 0 config) where
  boolsToBwd _ = C.fromList_lazy . fmap boolToMMSubordinateAck

instance (GoodMMManagerConfig config) => Backpressure (AvalonMMManager dom config) where
  boolsToBwd _ = C.fromList_lazy . fmap boolToMMManagerAck

-- TODO keep waitrequest on when not receiving data?

instance (GoodMMSubordinateConfig config) =>
  DfConv.DfConv   (AvalonMMSubordinate dom 0 config) where
  type Dom        (AvalonMMSubordinate dom 0 config) = dom
  type BwdPayload (AvalonMMSubordinate dom 0 config) = AvalonSubordinateReadImpt config
  type FwdPayload (AvalonMMSubordinate dom 0 config) = Either (AvalonSubordinateReadReqImpt config) (AvalonSubordinateWriteImpt config)

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
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

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
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

instance (GoodMMManagerConfig config) =>
  DfConv.DfConv   (AvalonMMManager dom config) where
  type Dom        (AvalonMMManager dom config) = dom
  type BwdPayload (AvalonMMManager dom config) = AvalonManagerReadImpt config
  type FwdPayload (AvalonMMManager dom config) = Either (AvalonManagerReadReqImpt config) (AvalonManagerWriteImpt config)

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
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

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
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

instance (GoodMMManagerConfig config, KnownDomain dom) =>
  Simulate (AvalonMMManager dom config) where
  type SimulateFwdType (AvalonMMManager dom config) = [AvalonManagerOut config]
  type SimulateBwdType (AvalonMMManager dom config) = [AvalonManagerIn config]
  type SimulateChannels (AvalonMMManager dom config) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ = sample_lazy
  sigToSimBwd _ = sample_lazy

  stallC conf (head -> (stallAck, stalls))
    = withClockResetEnable clockGen resetGen enableGen
    $ DfConv.stall Proxy Proxy conf stallAck stalls

instance (GoodMMSubordinateConfig config, KnownDomain dom) =>
  Simulate (AvalonMMSubordinate dom 0 config) where
  type SimulateFwdType (AvalonMMSubordinate dom 0 config) = [AvalonSubordinateIn config]
  type SimulateBwdType (AvalonMMSubordinate dom 0 config) = [AvalonSubordinateOut config]
  type SimulateChannels (AvalonMMSubordinate dom 0 config) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ = sample_lazy
  sigToSimBwd _ = sample_lazy

  stallC conf (head -> (stallAck, stalls))
    = withClockResetEnable clockGen resetGen enableGen
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
