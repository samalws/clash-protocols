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
  , AvalonWriteImpt(..)
  , AvalonReadReqImpt(..)
  , AvalonReadImpt(..)

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

import           Control.Monad.State (put, get)
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
      'False
      (KeepWaitRequest cfg)
      'False
      'False
      'False
      'False
      (SShared cfg)

type family RemoveNonDfManager (cfg :: AvalonMMManagerConfig) where
  RemoveNonDfManager cfg
    = 'AvalonMMManagerConfig
      'False
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
data AvalonWriteImpt keepAddr config
  = AvalonWriteImpt
  { wi_writeData  :: KeepType (KeepWriteData config) (Unsigned (DataWidth config))
  , wi_addr       :: KeepType keepAddr (Unsigned (AddrWidth config))
  , wi_byteEnable :: KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config))
  , wi_burstCount :: KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config))
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSharedConfig config,
                   NFDataX (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => NFDataX (AvalonWriteImpt keepAddr config)
deriving instance (GoodMMSharedConfig config,
                   NFData (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => NFData (AvalonWriteImpt keepAddr config)
deriving instance (GoodMMSharedConfig config,
                   ShowX (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => ShowX (AvalonWriteImpt keepAddr config)
deriving instance (GoodMMSharedConfig config,
                   Show (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => Show (AvalonWriteImpt keepAddr config)
deriving instance (GoodMMSharedConfig config,
                   Eq (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => Eq (AvalonWriteImpt keepAddr config)

-- TODO
-- data AvalonReadReqImpt (KeepAddr config) (SShared config)
data AvalonReadReqImpt keepAddr config
  = AvalonReadReqImpt
  { rri_addr       :: KeepType keepAddr (Unsigned (AddrWidth config))
  , rri_byteEnable :: KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config))
  , rri_burstCount :: KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config))
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSharedConfig config,
                   NFDataX (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => NFDataX (AvalonReadReqImpt keepAddr config)
deriving instance (GoodMMSharedConfig config,
                   NFData (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => NFData (AvalonReadReqImpt keepAddr config)
deriving instance (GoodMMSharedConfig config,
                   ShowX (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => ShowX (AvalonReadReqImpt keepAddr config)
deriving instance (GoodMMSharedConfig config,
                   Show (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => Show (AvalonReadReqImpt keepAddr config)
deriving instance (GoodMMSharedConfig config,
                   Eq (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => Eq (AvalonReadReqImpt keepAddr config)

data AvalonReadImpt config
  = AvalonReadImpt
  { ri_readData    :: KeepType (KeepReadData config) (Unsigned (DataWidth config))
  , ri_endOfPacket :: KeepType (KeepEndOfPacket config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (GoodMMSharedConfig config)
                   => NFDataX (AvalonReadImpt config)
deriving instance (GoodMMSharedConfig config)
                   => NFData (AvalonReadImpt config)
deriving instance (GoodMMSharedConfig config)
                   => ShowX (AvalonReadImpt config)
deriving instance (GoodMMSharedConfig config)
                   => Show (AvalonReadImpt config)
deriving instance (GoodMMSharedConfig config)
                   => Eq (AvalonReadImpt config)


managerOutAddNonDf ::
  GoodMMManagerConfig cfg =>
  KeepType (KeepFlush cfg) Bool ->
  AvalonManagerOut (RemoveNonDfManager cfg) ->
  AvalonManagerOut cfg
managerOutAddNonDf flush AvalonManagerOut{..}
  = AvalonManagerOut
  { mo_addr, mo_read, mo_write, mo_byteEnable, mo_burstCount, mo_writeData
  , mo_flush = flush
  }

managerOutRemoveNonDf ::
  GoodMMManagerConfig cfg =>
  AvalonManagerOut cfg ->
  ( AvalonManagerOut (RemoveNonDfManager cfg)
  , KeepType (KeepFlush cfg) Bool )
managerOutRemoveNonDf AvalonManagerOut{..}
  = (AvalonManagerOut
  { mo_addr, mo_read, mo_write, mo_byteEnable, mo_burstCount, mo_writeData
  , mo_flush = keepTypeFalse
  }, mo_flush)

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
  , mi_irqList = keepTypeFalse
  , mi_irqNumber = keepTypeFalse
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
  , so_readyForData = keepTypeFalse
  , so_dataAvailable = keepTypeFalse
  , so_irq = keepTypeFalse
  }, (so_readyForData, so_dataAvailable, so_irq))

-- TODO take out begintransfer and beginbursttransfer
subordinateInAddNonDf ::
  GoodMMSubordinateConfig cfg =>
  ( KeepType (KeepBeginBurstTransfer cfg) Bool
  , KeepType (KeepBeginTransfer cfg) Bool ) ->
  AvalonSubordinateIn (RemoveNonDfSubordinate cfg) ->
  AvalonSubordinateIn cfg
subordinateInAddNonDf (beginBurstTransfer, beginTransfer) AvalonSubordinateIn{..}
  = AvalonSubordinateIn
  { si_chipSelect, si_addr, si_read, si_write, si_byteEnable, si_writeByteEnable, si_burstCount, si_writeData
  , si_beginBurstTransfer = beginBurstTransfer
  , si_beginTransfer = beginTransfer
  }

subordinateInRemoveNonDf ::
  GoodMMSubordinateConfig cfg =>
  AvalonSubordinateIn cfg ->
  ( AvalonSubordinateIn (RemoveNonDfSubordinate cfg)
  , ( KeepType (KeepBeginBurstTransfer cfg) Bool
    , KeepType (KeepBeginTransfer cfg) Bool ) )
subordinateInRemoveNonDf AvalonSubordinateIn{..}
  = (AvalonSubordinateIn
  { si_chipSelect, si_addr, si_read, si_write, si_byteEnable, si_writeByteEnable, si_burstCount, si_writeData
  , si_beginBurstTransfer = keepTypeFalse
  , si_beginTransfer = keepTypeFalse
  }, (si_beginBurstTransfer, si_beginTransfer))

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
mmSubordinateReadDat :: (GoodMMSubordinateConfig config) => AvalonReadImpt (SShared config) -> AvalonSubordinateOut config
mmSubordinateReadDat dat
  = AvalonSubordinateOut
    { so_waitRequest   = toKeepType False
    , so_readDataValid = toKeepType True
    , so_readyForData  = toKeepType False
    , so_dataAvailable = toKeepType False
    , so_endOfPacket   = ri_endOfPacket dat
    , so_irq           = toKeepType False
    , so_readData      = ri_readData dat
    }

mmSubordinateOutToReadImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateOut config -> Maybe (AvalonReadImpt (SShared config))
mmSubordinateOutToReadImpt (AvalonSubordinateOut{..})
  = if cond then Just AvalonReadImpt
  { ri_endOfPacket = so_endOfPacket
  , ri_readData    = so_readData
  } else Nothing
  where
  cond = fromKeepTypeDef True so_readDataValid && not (fromKeepTypeDef False so_waitRequest)

mmManagerInToReadImpt :: (GoodMMManagerConfig config) => AvalonManagerIn config -> Maybe (AvalonReadImpt (MShared config))
mmManagerInToReadImpt (AvalonManagerIn{..})
  = if cond then Just AvalonReadImpt
  { ri_endOfPacket = mi_endOfPacket
  , ri_readData    = mi_readData
  } else Nothing
  where
  cond = not mi_waitRequest && fromKeepTypeDef True mi_readDataValid -- TODO anything else? YES I THINK SO........

mmSubordinateInToWriteImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config -> Maybe (AvalonWriteImpt (KeepAddr config) (SShared config))
mmSubordinateInToWriteImpt (AvalonSubordinateIn{..})
  = if cond then Just AvalonWriteImpt
  { wi_addr               = si_addr
  , wi_byteEnable         = si_byteEnable
  , wi_burstCount         = si_burstCount
  , wi_writeData          = si_writeData
  } else Nothing
  where
  cond =  fromKeepTypeDef True si_chipSelect
       && fromKeepTypeDef True si_write
       && not (fromKeepTypeDef False si_read)
       -- && 0 /= fromKeepTypeDef 1 si_byteEnable
       -- && 0 /= fromKeepTypeDef 1 si_writeByteEnable

mmSubordinateInToReadReqImpt :: (GoodMMSubordinateConfig config) => AvalonSubordinateIn config -> Maybe (AvalonReadReqImpt (KeepAddr config) (SShared config))
mmSubordinateInToReadReqImpt (AvalonSubordinateIn{..})
  = if cond then Just AvalonReadReqImpt
  { rri_addr               = si_addr
  , rri_byteEnable         = si_byteEnable
  , rri_burstCount         = si_burstCount
  } else Nothing
  where
  cond =  fromKeepTypeDef True si_chipSelect
       && fromKeepTypeDef True si_read
       && not (fromKeepTypeDef False si_write)

mmManagerOutToWriteImpt :: (GoodMMManagerConfig config) => AvalonManagerOut config -> Maybe (AvalonWriteImpt 'True (MShared config))
mmManagerOutToWriteImpt (AvalonManagerOut{..})
  = if cond then Just AvalonWriteImpt
  { wi_addr       = toKeepType mo_addr
  , wi_byteEnable = mo_byteEnable
  , wi_burstCount = mo_burstCount
  , wi_writeData  = mo_writeData
  } else Nothing
  where
  cond =  fromKeepTypeDef True mo_write
       && not (fromKeepTypeDef False mo_read)
       -- && 0 /= fromKeepTypeDef 1 mo_byteEnable

mmManagerOutToReadReqImpt :: (GoodMMManagerConfig config) => AvalonManagerOut config -> Maybe (AvalonReadReqImpt 'True (MShared config))
mmManagerOutToReadReqImpt (AvalonManagerOut{..})
  = if cond then Just AvalonReadReqImpt
  { rri_addr       = toKeepType mo_addr
  , rri_byteEnable = mo_byteEnable
  , rri_burstCount = mo_burstCount
  } else Nothing
  where
  cond =  fromKeepTypeDef True mo_read
       && not (fromKeepTypeDef False mo_write)
       -- && 0 /= fromKeepTypeDef 1 mo_byteEnable

-- TODO comment
mmWriteImptToSubordinateIn :: (GoodMMSubordinateConfig config) => AvalonWriteImpt (KeepAddr config) (SShared config) -> AvalonSubordinateIn config
mmWriteImptToSubordinateIn (AvalonWriteImpt{..})
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType True
  , si_addr               = wi_addr
  , si_read               = toKeepType False
  , si_write              = toKeepType True
  , si_byteEnable         = wi_byteEnable
  , si_writeByteEnable    = toKeepType $ bitCoerce $ repeat True
  , si_beginTransfer      = toKeepType False -- TODO??
  , si_burstCount         = wi_burstCount
  , si_beginBurstTransfer = toKeepType False -- TODO??
  , si_writeData          = wi_writeData
  }

-- TODO comment
mmReadReqImptToSubordinateIn :: (GoodMMSubordinateConfig config) => AvalonReadReqImpt (KeepAddr config) (SShared config) -> AvalonSubordinateIn config
mmReadReqImptToSubordinateIn (AvalonReadReqImpt{..})
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType False
  , si_addr               = rri_addr
  , si_read               = toKeepType True
  , si_write              = toKeepType False
  , si_byteEnable         = rri_byteEnable
  , si_writeByteEnable    = toKeepType 0
  , si_beginTransfer      = toKeepType False -- TODO??
  , si_burstCount         = rri_burstCount
  , si_beginBurstTransfer = toKeepType False -- TODO??
  , si_writeData          = errorX "No writeData for read req"
  }

-- TODO comment
mmWriteImptToManagerOut :: (GoodMMManagerConfig config) => AvalonWriteImpt 'True (MShared config) -> AvalonManagerOut config
mmWriteImptToManagerOut (AvalonWriteImpt{..})
  = AvalonManagerOut
  { mo_addr        = fromKeepTypeTrue wi_addr
  , mo_read        = toKeepType False
  , mo_write       = toKeepType True
  , mo_byteEnable  = wi_byteEnable
  , mo_burstCount  = wi_burstCount
  , mo_flush       = toKeepType False
  , mo_writeData   = wi_writeData
  }

-- TODO comment
mmReadReqImptToManagerOut :: (GoodMMManagerConfig config) => AvalonReadReqImpt 'True (MShared config) -> AvalonManagerOut config
mmReadReqImptToManagerOut (AvalonReadReqImpt{..})
  = AvalonManagerOut
  { mo_addr        = fromKeepTypeTrue rri_addr
  , mo_read        = toKeepType True
  , mo_write       = toKeepType False
  , mo_byteEnable  = rri_byteEnable
  , mo_burstCount  = rri_burstCount
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

-- An @AvalonManagerIn@ TODO
mmManagerReadDat :: (GoodMMManagerConfig config) => AvalonReadImpt (MShared config) -> AvalonManagerIn config
mmManagerReadDat dat
  = AvalonManagerIn
  { mi_waitRequest   = False
  , mi_readDataValid = toKeepType True
  , mi_endOfPacket   = ri_endOfPacket dat
  , mi_irqList       = toKeepType 0
  , mi_irqNumber     = toKeepType Nothing
  , mi_readData      = ri_readData dat
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

-- TODO comment that this is a copypaste of the master one
instance (GoodMMSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  DfConv.DfConv   (AvalonMMSubordinate dom 0 config) where
  type Dom        (AvalonMMSubordinate dom 0 config) = dom
  type BwdPayload (AvalonMMSubordinate dom 0 config) = AvalonReadImpt (SShared config)
  type FwdPayload (AvalonMMSubordinate dom 0 config) = Either (AvalonReadReqImpt (KeepAddr config) (SShared config)) (AvalonWriteImpt (KeepAddr config) (SShared config))

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = (Nothing, False)
    blankOtp = mmSubordinateInNoData
    stateFn so dfAck dfDat = do
      (readDatStored, readReqAcked) <- get
      let readDatIn = mmSubordinateOutToReadImpt so -- TODO this fn should not look at waitrequest, only at readdatavalid
      let miBool = mmSubordinateOutToBool so
      let (readDatStored', readReqAcked', si, dfAckOut)
            = case (dfDat, readDatStored) of
                (Just (Right wi), _) ->
                  ( readDatStored
                  , False
                  , mmWriteImptToSubordinateIn (wi { wi_burstCount = toKeepType 1 })
                  , miBool )
                (_, Just _) ->
                  ( readDatStored
                  , False
                  , mmSubordinateInNoData
                  , miBool )
                (Just (Left ri), Nothing) ->
                  ( readDatIn
                  , miBool
                  , if readReqAcked
                      then mmSubordinateInNoData
                      else mmReadReqImptToSubordinateIn (ri { rri_burstCount = toKeepType 1 })
                  , miBool )
                (Nothing, Nothing) ->
                  ( Nothing
                  , False
                  , mmSubordinateInNoData
                  , False )
      put (if dfAck then Nothing else readDatStored', readReqAcked')
      pure (si, readDatStored', dfAckOut)

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = False
    blankOtp = boolToMMSubordinateAck False
    stateFn si dfAck dfDat = do
      dfAckSt <- get
      let writeImpt = mmSubordinateInToWriteImpt si
      let readReqImpt = if Maybe.isNothing writeImpt then mmSubordinateInToReadReqImpt si else Nothing
      let sendingReadDat = if (Maybe.isJust readReqImpt) && dfAckSt then dfDat else Nothing
      let dfAckSt' = Maybe.isJust readReqImpt && (dfAckSt || dfAck)
      let so = case (writeImpt, sendingReadDat) of
                 (Just _, _) -> boolToMMSubordinateAck dfAck
                 (_, Just rdat) -> mmSubordinateReadDat rdat
                 _ -> boolToMMSubordinateAck False
      let dfDatOut = case (writeImpt, readReqImpt, dfAckSt) of
                       (Just wi, _, _) -> Just (Right wi)
                       (_, Just ri, False) -> Just (Left ri)
                       _ -> Nothing
      let dfAckOut = Maybe.isJust sendingReadDat
      put dfAckSt'
      pure (so, dfDatOut, dfAckOut)

-- TODO comment abt burstcount forced to be 1
instance (GoodMMManagerConfig config, config ~ RemoveNonDfManager config) =>
  DfConv.DfConv   (AvalonMMManager dom config) where
  type Dom        (AvalonMMManager dom config) = dom
  type BwdPayload (AvalonMMManager dom config) = AvalonReadImpt (MShared config)
  type FwdPayload (AvalonMMManager dom config) = Either (AvalonReadReqImpt 'True (MShared config)) (AvalonWriteImpt 'True (MShared config))

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = (Nothing, False) -- reads only get sent for one clock cycle, so we have to store it until it's acked
    -- TODO added "already waitrequest=false'd our read request" to state; don't ack df but stop sending forward mmReadReqImptToManagerOut, comment abt it
    blankOtp = mmManagerOutNoData
    stateFn mi dfAck dfDat = do
      (readDatStored, readReqAcked) <- get
      let readDatIn = mmManagerInToReadImpt mi -- TODO this fn should not look at waitrequest, only at readdatavalid
      let miBool = mmManagerInToBool mi
      let (readDatStored', readReqAcked', mo, dfAckOut)
            = case (dfDat, readDatStored) of
                (Just (Right wi), _) ->
                  ( readDatStored
                  , False
                  , mmWriteImptToManagerOut (wi { wi_burstCount = toKeepType 1 })
                  , miBool )
                (_, Just _) ->
                  ( readDatStored
                  , False
                  , mmManagerOutNoData
                  , miBool )
                (Just (Left ri), Nothing) ->
                  ( readDatIn
                  , miBool
                  , if readReqAcked
                      then mmManagerOutNoData
                      else mmReadReqImptToManagerOut (ri { rri_burstCount = toKeepType 1 })
                  , miBool )
                (Nothing, Nothing) ->
                  ( Nothing
                  , False
                  , mmManagerOutNoData
                  , False )
      put (if dfAck then Nothing else readDatStored', readReqAcked')
      pure (mo, readDatStored', dfAckOut)

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = False -- read request might be acked before read is sent back
    blankOtp = boolToMMManagerAck False
    stateFn mo dfAck dfDat = do
      dfAckSt <- get
      let writeImpt = mmManagerOutToWriteImpt mo
      let readReqImpt = if Maybe.isNothing writeImpt then mmManagerOutToReadReqImpt mo else Nothing
      let sendingReadDat = if (Maybe.isJust readReqImpt) && dfAckSt then dfDat else Nothing
      let dfAckSt' = Maybe.isJust readReqImpt && (dfAckSt || dfAck)
      let mi = case (writeImpt, sendingReadDat) of
                 (Just _, _) -> boolToMMManagerAck dfAck
                 (_, Just rdat) -> mmManagerReadDat rdat
                 _ -> boolToMMManagerAck False
      let dfDatOut = case (writeImpt, readReqImpt, dfAckSt) of
                       (Just wi, _, _) -> Just (Right wi)
                       (_, Just ri, False) -> Just (Left ri)
                       _ -> Nothing
      let dfAckOut = Maybe.isJust sendingReadDat
      put dfAckSt'
      pure (mi, dfDatOut, dfAckOut)

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
