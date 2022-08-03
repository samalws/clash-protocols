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

module Protocols.Avalon.MemMap.AvalonMemMap
  ( AvalonMMSharedConfig(..)
  , AvalonMMSubordinateConfig(..)
  , AvalonMMManagerConfig(..)

  , DataWidth
  , KeepReadData
  , KeepWriteData
  , AddrWidth
  , KeepRead
  , KeepWrite
  , ByteEnableWidth
  , KeepByteEnable
  , BurstCountWidth
  , KeepBurstCount
  , KeepReadDataValid
  , KeepEndOfPacket

  , KeepAddr
  , KeepWriteByteEnable
  , KeepChipSelect
  , KeepBeginTransfer
  , KeepWaitRequest
  , KeepBeginBurstTransfer
  , KeepReadyForData
  , KeepDataAvailable
  , KeepIrq
  , SShared

  , KeepFlush
  , KeepIrqList
  , KeepIrqNumber
  , MShared

  , RemoveNonDfSubordinate 
  , RemoveNonDfManager 

  , GoodMMSharedConfig
  , GoodMMSubordinateConfig
  , GoodMMManagerConfig

  , AvalonManagerOut(..)
  , AvalonManagerIn(..)
  , AvalonSubordinateOut(..)
  , AvalonSubordinateIn(..)

  -- TODO i think impts are identical for M and S
  , AvalonManagerWriteImpt(..)
  , AvalonManagerReadReqImpt(..)
  , AvalonManagerReadImpt(..)
  , AvalonSubordinateWriteImpt(..)
  , AvalonSubordinateReadReqImpt(..)
  , AvalonSubordinateReadImpt(..)

  , interconnectFabric
  , interconnectFabricSingleMember

  , managerOutAddNonDf
  , managerOutRemoveNonDf
  , managerInAddNonDf
  , managerInRemoveNonDf
  , subordinateOutAddNonDf
  , subordinateOutRemoveNonDf
  , subordinateInAddNonDf
  , subordinateInRemoveNonDf

  , AvalonMMManager(..)
  , AvalonMMSubordinate(..)

  ) where

-- base
import Prelude ()

import           Control.Arrow ((***))
import           Control.Monad.State (put, gets, get)
import           Control.DeepSeq (NFData)
import qualified Data.Maybe as Maybe
import           Data.Proxy

-- clash-prelude
import           Clash.Prelude hiding (take, concat, length)
import qualified Clash.Prelude as C

-- me
import           Protocols.Internal
import qualified Protocols.DfConv as DfConv


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
      (KeepBeginTransfer cfg) -- 'False -- (KeepBeginTransfer cfg) -- TODO deal with this elsewhere
      (KeepWaitRequest cfg)
      (KeepBeginBurstTransfer cfg) -- 'False -- TODO deal with this elsewhere -- (KeepBeginBurstTransfer cfg) -- TODO this isnt df compatible
      'False
      'False
      'False
      (SShared cfg)

type family RemoveNonDfManager (cfg :: AvalonMMManagerConfig) where
  RemoveNonDfManager cfg
    = 'AvalonMMManagerConfig
      (KeepFlush cfg) -- TODO make this false also
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

-- TODO remove some constraints
-- TOOD remove the forall
interconnectFabric ::
  forall dom managerConfig subordinateConfig numManager numSubordinate decNumSub fixedWaitTime.
  ( GoodMMManagerConfig managerConfig
  , GoodMMSubordinateConfig subordinateConfig
  , MShared managerConfig ~ SShared subordinateConfig
  , HiddenClockResetEnable dom
  , KnownNat fixedWaitTime
  , KnownNat numManager
  , KnownNat numSubordinate
  , numSubordinate ~ (decNumSub + 1) ) =>
  Vec numSubordinate (Unsigned (AddrWidth (SShared subordinateConfig)) -> Bool) ->
  Vec numSubordinate (Unsigned 6) ->
  SNat fixedWaitTime ->
  Circuit
    (Vec numManager (AvalonMMManager dom managerConfig))
    (Vec numSubordinate (AvalonMMSubordinate dom fixedWaitTime subordinateConfig))
interconnectFabric subordinateAddrFns irqNums fixedWaitTime = Circuit cktFn where
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
    setIrq miMsg = miMsg { mi_irqList = toKeepType $ unpack $ resize $ pack irqList, mi_irqNumber = toKeepType mirq }
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
  moIsOn mo = (fromKeepTypeDef True (mo_read mo) || fromKeepTypeDef True (mo_write mo)) -- && (0 /= fromKeepTypeDef 1 (mo_byteEnable mo))
  -- mo wants to read
  moIsRead mo = moIsOn mo && fromKeepTypeDef True (mo_read mo) && not (fromKeepTypeDef False (mo_write mo))
  -- mo wants to write
  moIsWrite mo = moIsOn mo && fromKeepTypeDef True (mo_write mo) && not (fromKeepTypeDef False (mo_read mo))

  -- modify one xferSt value, given one manager-out message and one subordinate-out message
  -- if there is no manager connected, our state should be Nothing
  modifySt Nothing _ _ = Nothing
  -- if there is a manager connected, give a default value of xferSt if needed, and then call on modifySt' to modify it
  modifySt (Just mo) so st = modifySt' mo so (Maybe.fromMaybe (0 :: Unsigned 8,
                                                               fromKeepTypeDef 1 (mo_burstCount mo),
                                                               _0 fixedWaitTime,
                                                               False,
                                                               False) st)
  modifySt' mo so (ctr1, ctr2, ctr3, readyForTransfer, flushed) = modifySt'' so (optDecCtr so $ optIncCtr1 mo so ctr1,
                                                                                 optDecCtr2 mo so ctr2,
                                                                                 modifyCtr3 mo ctr3,
                                                                                 modifyReadyForTransfer mo so ctr3 readyForTransfer,
                                                                                 flushed || fromKeepTypeDef False (mo_flush mo))
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
  -- finally, kill the xferSt if all the counters are at 0, or if we're flushing and the subordinate is currently transferring data
  -- modifySt'' so (ctr1, _, ctr3, _, True) | not (fromKeepTypeDef False (so_waitRequest so)) = Nothing TODO how to check if currently xfering data?
  modifySt'' _ (0, 0, 0, _, _) = Nothing
  modifySt'' _ st = Just st
  -- hack to get a "0" value of the right type
  _0 :: (KnownNat n) => SNat n -> Index (n+1)
  _0 _ = 0

  -- given subordinate-out message and xferSt, generate manager-in message
  convSoMi _ (Just (_,_,_,_,True)) = mmManagerInNoData -- if manager flushes, we make it sit and wait for all the transfers to go through
  convSoMi so st
    = AvalonManagerIn
    { mi_waitRequest   = Maybe.maybe True (\(ctr1,_,ctr3,_,_) -> ctr1 < maxBound && ctr3 == 0) st && (fromKeepTypeDef False (so_waitRequest so))
    , mi_readDataValid = convKeepType False (so_readDataValid so) -- TODO uhhhhhhh I think there's more to it than that...
    , mi_endOfPacket   = convKeepType False (so_endOfPacket so)
    , mi_irqList       = errorX "interconnect fabric: this value gets overwritten later"
    , mi_irqNumber     = errorX "interconnect fabric: this value gets overwritten later"
    , mi_readData      = so_readData so
    }

  -- given manager-out message and xferSt, generate subordinate-in message
  convMoSi mo st
    = AvalonSubordinateIn
    { si_addr               = toKeepType $ mo_addr mo
    , si_read               = toKeepType $ fromKeepTypeDef True (mo_read  mo) && not (fromKeepTypeDef False (mo_write mo))
    , si_write              = toKeepType $ fromKeepTypeDef True (mo_write mo) && not (fromKeepTypeDef False (mo_read  mo))
    , si_writeByteEnable    = toKeepType $ resize $ if (fromKeepTypeDef True (mo_write mo)) then fromKeepTypeDef 0 (mo_byteEnable mo) else 0
    , si_burstCount         = mo_burstCount mo
    , si_chipSelect         = toKeepType True
    , si_byteEnable         = toKeepType $ resize $ fromKeepTypeDef 0 $ mo_byteEnable mo
    , si_beginTransfer      = toKeepType $ moIsOn mo && (Maybe.maybe True (\(_,_,_,readyForMsg,_) -> readyForMsg) st)
    , si_beginBurstTransfer = toKeepType $ Maybe.isNothing st
    , si_writeData          = mo_writeData mo
    }

interconnectFabricSingleMember subordinateAddrFn irqNum fixedWaitTime
  = Circuit ((head *** head) . toSignals (interconnectFabric (singleton subordinateAddrFn) (singleton irqNum) fixedWaitTime) . (singleton *** singleton))

managerOutAddNonDf ::
  GoodMMManagerConfig cfg =>
  AvalonManagerOut (RemoveNonDfManager cfg) ->
  AvalonManagerOut cfg
managerOutAddNonDf AvalonManagerOut{..} = AvalonManagerOut{..}

managerOutRemoveNonDf ::
  GoodMMManagerConfig cfg =>
  AvalonManagerOut cfg ->
  AvalonManagerOut (RemoveNonDfManager cfg)
managerOutRemoveNonDf AvalonManagerOut{..} = AvalonManagerOut{..}

managerInAddNonDf ::
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

-- TODO take out begintransfer and beginbursttransfer
subordinateInAddNonDf ::
  GoodMMSubordinateConfig cfg =>
  AvalonSubordinateIn (RemoveNonDfSubordinate cfg) ->
  AvalonSubordinateIn cfg
subordinateInAddNonDf AvalonSubordinateIn{..} = AvalonSubordinateIn{..}

subordinateInRemoveNonDf ::
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
  cond = not mi_waitRequest -- TODO anything else? YES I THINK SO........

mmSubordinateInToWriteImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config -> Maybe (AvalonSubordinateWriteImpt config)
mmSubordinateInToWriteImpt (AvalonSubordinateIn{..})
  = if cond then Just AvalonSubordinateWriteImpt
  { swi_addr               = si_addr
  , swi_byteEnable         = si_byteEnable
  , swi_burstCount         = si_burstCount
  , swi_writeData          = si_writeData
  } else Nothing
  where
  cond =  fromKeepTypeDef True si_chipSelect
       && fromKeepTypeDef True si_write
       && not (fromKeepTypeDef False si_read)
       -- && 0 /= fromKeepTypeDef 1 si_byteEnable
       -- && 0 /= fromKeepTypeDef 1 si_writeByteEnable

mmSubordinateInToReadReqImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config -> Maybe (AvalonSubordinateReadReqImpt config)
mmSubordinateInToReadReqImpt (AvalonSubordinateIn{..})
  = if cond then Just AvalonSubordinateReadReqImpt
  { srri_addr               = si_addr
  , srri_byteEnable         = si_byteEnable
  , srri_burstCount         = si_burstCount
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
  , mwi_writeData  = mo_writeData
  } else Nothing
  where
  cond =  fromKeepTypeDef True mo_write
       && not (fromKeepTypeDef False mo_read)
       -- && 0 /= fromKeepTypeDef 1 mo_byteEnable

mmManagerOutToReadReqImpt :: (GoodMMManagerConfig config) => AvalonManagerOut config -> Maybe (AvalonManagerReadReqImpt config)
mmManagerOutToReadReqImpt (AvalonManagerOut{..})
  = if cond then Just AvalonManagerReadReqImpt
  { mrri_addr       = mo_addr
  , mrri_byteEnable = mo_byteEnable
  , mrri_burstCount = mo_burstCount
  } else Nothing
  where
  cond =  fromKeepTypeDef True mo_read
       && not (fromKeepTypeDef False mo_write)
       -- && 0 /= fromKeepTypeDef 1 mo_byteEnable

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
  , si_beginTransfer      = toKeepType False -- TODO??
  , si_burstCount         = swi_burstCount
  , si_beginBurstTransfer = toKeepType False -- TODO??
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
  , si_beginTransfer      = toKeepType False -- TODO??
  , si_burstCount         = srri_burstCount
  , si_beginBurstTransfer = toKeepType False -- TODO??
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
  , mo_flush       = toKeepType False
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
  , mo_flush       = toKeepType False
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

-- Grab the "acknowledgement" value from an @AvalonSubordinateOut@.
-- Reasonable defaults are provided for optional fields.
mmSubordinateOutToBool :: (GoodMMSubordinateConfig config) => AvalonSubordinateOut config -> Bool
mmSubordinateOutToBool so = fromKeepTypeDef True (so_readyForData so) && not (fromKeepTypeDef False (so_waitRequest so))

-- Grab the "acknowledgement" value from an @AvalonManagerIn@.
-- Reasonable defaults are provided for optional fields.
mmManagerInToBool :: (GoodMMManagerConfig config) => AvalonManagerIn config -> Bool
mmManagerInToBool = not . mi_waitRequest

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

instance (GoodMMSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  DfConv.DfConv   (AvalonMMSubordinate dom 0 config) where
  type Dom        (AvalonMMSubordinate dom 0 config) = dom
  type BwdPayload (AvalonMMSubordinate dom 0 config) = AvalonSubordinateReadImpt config
  type FwdPayload (AvalonMMSubordinate dom 0 config) = Either (AvalonSubordinateReadReqImpt config) (AvalonSubordinateWriteImpt config)

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = Nothing
    blankOtp = mmSubordinateInNoData
    stateFn so dfAck dfDat = do
      readDatStored <- get
      let readDatIn = mmSubordinateOutToReadImpt so
      let (toPut, toRetSi)
            = case ( readDatStored
                   , dfDat
                   ) of
            (_, Just (Right wi)) -> (readDatStored, mmWriteImptToSubordinateIn (wi { swi_burstCount = toKeepType 1 }))
            (Just _, _) -> (readDatStored, mmSubordinateInNoData)
            (Nothing, Just (Left ri)) -> (readDatIn, mmReadReqImptToSubordinateIn (ri { srri_burstCount = toKeepType 1 }))
            (Nothing, Nothing) -> (Nothing, mmSubordinateInNoData)
      put $ if dfAck then Nothing else toPut
      pure (toRetSi, toPut, mmSubordinateOutToBool so)

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = False
    blankOtp = boolToMMSubordinateAck False
    stateFn si dfAck dfDat = do
      dfAckSt <- get -- s (|| dfAck)
      let (toPut, toRet)
            = case ( mmSubordinateInToWriteImpt si {- write data -}
                   , mmSubordinateInToReadReqImpt si {- read request coming in -}
                   , dfAckSt {- df acknowledged read request -}
                   , dfDat {- df sending read data -}
                   ) of
            (Just wi, _, _, _) -> (False, (boolToMMSubordinateAck dfAck, Just (Right wi), False))
            (Nothing, Just _, True, Just rdat) -> (False, (mmSubordinateReadDat rdat, Nothing, True))
            (Nothing, Just ri, _, _) -> ((dfAckSt || dfAck), (boolToMMSubordinateAck False, if dfAckSt then Nothing else Just (Left ri), False))
            (Nothing, Nothing, _, _) -> (False, (boolToMMSubordinateAck False, Nothing, False))
      put toPut
      pure toRet

-- TODO comment abt burstcount forced to be 1
instance (GoodMMManagerConfig config, config ~ RemoveNonDfManager config) =>
  DfConv.DfConv   (AvalonMMManager dom config) where
  type Dom        (AvalonMMManager dom config) = dom
  type BwdPayload (AvalonMMManager dom config) = AvalonManagerReadImpt config
  type FwdPayload (AvalonMMManager dom config) = Either (AvalonManagerReadReqImpt config) (AvalonManagerWriteImpt config)

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = Nothing -- reads only get sent for one clock cycle, so we have to store it until it's acked
    blankOtp = mmManagerOutNoData
    stateFn mi dfAck dfDat = do
      readDatStored <- get
      let readDatIn = mmManagerInToReadImpt mi
      let (toPut, toRetMo)
            = case ( readDatStored
                   , dfDat
                   ) of
            (_, Just (Right wi)) -> (readDatStored, mmWriteImptToManagerOut (wi { mwi_burstCount = toKeepType 1 }))
            (Just _, _) -> (readDatStored, mmManagerOutNoData)
            (Nothing, Just (Left ri)) -> (readDatIn, mmReadReqImptToManagerOut (ri { mrri_burstCount = toKeepType 1 }))
            (Nothing, Nothing) -> (Nothing, mmManagerOutNoData)
      put $ if dfAck then Nothing else toPut
      pure (toRetMo, toPut, mmManagerInToBool mi)

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = False -- read request might be acked before read is sent back
    blankOtp = boolToMMManagerAck False
    stateFn mo dfAck dfDat = do
      dfAckSt <- get -- s (|| dfAck)
      let (toPut, toRet)
            = case ( mmManagerOutToWriteImpt mo {- write data -}
                   , mmManagerOutToReadReqImpt mo {- read request coming in -}
                   , dfAckSt {- df acknowledged read request -}
                   , dfDat {- df sending read data -}
                   ) of
            (Just wi, _, _, _) -> (False, (boolToMMManagerAck dfAck, Just (Right wi), False))
            (Nothing, Just _, True, Just rdat) -> (False, (mmManagerReadDat rdat, Nothing, True))
            (Nothing, Just ri, _, _) -> ((dfAckSt || dfAck), (boolToMMManagerAck False, if dfAckSt then Nothing else Just (Left ri), False))
            (Nothing, Nothing, _, _) -> (False, (boolToMMManagerAck False, Nothing, False))
      put toPut
      pure toRet

instance (GoodMMManagerConfig config, KnownDomain dom, config ~ RemoveNonDfManager config) =>
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

instance (GoodMMSubordinateConfig config, KnownDomain dom, config ~ RemoveNonDfSubordinate config) =>
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
