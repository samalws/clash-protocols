{-|
Types and instance declarations for the Avalon memory mapped protocol.
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, RecordWildCards, TypeFamilyDependencies, UndecidableInstances #-}

module Protocols.Avalon.MemMap.AvalonMemMap where

-- base
import           Control.DeepSeq (NFData)
import           Prelude hiding (not, (&&), repeat)

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


data AvalonMMM2SConfig
  = AvalonMMM2SConfig
  { keepChipSelect         :: Bool
  , addrWidth              :: Nat
  , keepRead               :: Bool
  , keepWrite              :: Bool
  , byteEnableWidth        :: Nat
  , writeByteEnableWidth   :: Nat
  , keepBeginTransfer      :: Bool
  , burstCountWidth        :: Nat
  , keepBeginBurstTransfer :: Bool
  }

type family KeepChipSelect (c :: AvalonMMM2SConfig) where
  KeepChipSelect ('AvalonMMM2SConfig a _ _ _ _ _ _ _ _) = a

type family AddrWidth (c :: AvalonMMM2SConfig) where
  AddrWidth ('AvalonMMM2SConfig _ a _ _ _ _ _ _ _) = a

type family KeepRead (c :: AvalonMMM2SConfig) where
  KeepRead ('AvalonMMM2SConfig _ _ a _ _ _ _ _ _) = a

type family KeepWrite (c :: AvalonMMM2SConfig) where
  KeepWrite ('AvalonMMM2SConfig _ _ _ a _ _ _ _ _) = a

type family ByteEnableWidth (c :: AvalonMMM2SConfig) where
  ByteEnableWidth ('AvalonMMM2SConfig _ _ _ _ a _ _ _ _) = a

type family WriteByteEnableWidth (c :: AvalonMMM2SConfig) where
  WriteByteEnableWidth ('AvalonMMM2SConfig _ _ _ _ _ a _ _ _) = a

type family KeepBeginTransfer (c :: AvalonMMM2SConfig) where
  KeepBeginTransfer ('AvalonMMM2SConfig _ _ _ _ _ _ a _ _) = a

type family BurstCountWidth (c :: AvalonMMM2SConfig) where
  BurstCountWidth ('AvalonMMM2SConfig _ _ _ _ _ _ _ a _) = a

type family KeepBeginBurstTransfer (c :: AvalonMMM2SConfig) where
  KeepBeginBurstTransfer ('AvalonMMM2SConfig _ _ _ _ _ _ _ _ a) = a


class (KeepBoolClass (KeepChipSelect config), KnownNat (AddrWidth config), KeepBoolClass (KeepRead config), KeepBoolClass (KeepWrite config), KnownNat (ByteEnableWidth config), KnownNat (WriteByteEnableWidth config), KeepBoolClass (KeepBeginTransfer config), KnownNat (BurstCountWidth config), KeepBoolClass (KeepBeginBurstTransfer config)) => GoodMMM2SConfig config
instance (KeepBoolClass (KeepChipSelect config), KnownNat (AddrWidth config), KeepBoolClass (KeepRead config), KeepBoolClass (KeepWrite config), KnownNat (ByteEnableWidth config), KnownNat (WriteByteEnableWidth config), KeepBoolClass (KeepBeginTransfer config), KnownNat (BurstCountWidth config), KeepBoolClass (KeepBeginBurstTransfer config)) => GoodMMM2SConfig config


data AvalonMMS2MConfig
  = AvalonMMS2MConfig
  { keepWaitRequest   :: Bool
  , keepReadDataValid :: Bool
  , keepReadyForData  :: Bool
  , keepDataAvailable :: Bool
  , keepEndOfPacket   :: Bool
  , keepIrq           :: Bool
  }

type family KeepWaitRequest (c :: AvalonMMS2MConfig) where
  KeepWaitRequest ('AvalonMMS2MConfig a _ _ _ _ _) = a

type family KeepReadDataValid (c :: AvalonMMS2MConfig) where
  KeepReadDataValid ('AvalonMMS2MConfig _ a _ _ _ _) = a

type family KeepReadyForData (c :: AvalonMMS2MConfig) where
  KeepReadyForData ('AvalonMMS2MConfig _ _ a _ _ _) = a

type family KeepDataAvailable (c :: AvalonMMS2MConfig) where
  KeepDataAvailable ('AvalonMMS2MConfig _ _ _ a _ _) = a

type family KeepEndOfPacket (c :: AvalonMMS2MConfig) where
  KeepEndOfPacket ('AvalonMMS2MConfig _ _ _ _ a _) = a

type family KeepIrq (c :: AvalonMMS2MConfig) where
  KeepIrq ('AvalonMMS2MConfig _ _ _ _ _ a) = a


class (KeepBoolClass (KeepWaitRequest config), KeepBoolClass (KeepReadDataValid config), KeepBoolClass (KeepReadyForData config), KeepBoolClass (KeepDataAvailable config), KeepBoolClass (KeepEndOfPacket config), KeepBoolClass (KeepIrq config)) => GoodMMS2MConfig config
instance (KeepBoolClass (KeepWaitRequest config), KeepBoolClass (KeepReadDataValid config), KeepBoolClass (KeepReadyForData config), KeepBoolClass (KeepDataAvailable config), KeepBoolClass (KeepEndOfPacket config), KeepBoolClass (KeepIrq config)) => GoodMMS2MConfig config


-- | Data sent from master to slave.
data AvalonMMM2S config writeDataType
  = AvalonMMM2S
  { _chipSelect         :: KeepBool (KeepChipSelect  config)
  , _addr               :: Unsigned (AddrWidth       config)
  , _read               :: KeepBool (KeepRead        config)
  , _write              :: KeepBool (KeepWrite       config)
  , _byteEnable         :: Vec (ByteEnableWidth      config) Bool
  , _writeByteEnable    :: Vec (WriteByteEnableWidth config) Bool
  , _beginTransfer      :: KeepBool (KeepBeginTransfer      config)
  , _burstCount         :: Unsigned (BurstCountWidth        config)
  , _beginBurstTransfer :: KeepBool (KeepBeginBurstTransfer config)
  , _writeData          :: writeDataType
  }
  deriving Generic

deriving instance (GoodMMM2SConfig config, NFDataX writeDataType) => NFDataX (AvalonMMM2S config writeDataType)
deriving instance (GoodMMM2SConfig config, NFData  writeDataType) => NFData  (AvalonMMM2S config writeDataType)
deriving instance (GoodMMM2SConfig config, ShowX   writeDataType) => ShowX   (AvalonMMM2S config writeDataType)
deriving instance (GoodMMM2SConfig config, Show    writeDataType) => Show    (AvalonMMM2S config writeDataType)
deriving instance (GoodMMM2SConfig config, Eq      writeDataType) => Eq      (AvalonMMM2S config writeDataType)

-- | Data sent from slave to master.
data AvalonMMS2M config readDataType
  = AvalonMMS2M
  { _waitRequest   :: KeepBool (KeepWaitRequest   config)
  , _readDataValid :: KeepBool (KeepReadDataValid config)
  , _readyForData  :: KeepBool (KeepReadyForData  config)
  , _dataAvailable :: KeepBool (KeepDataAvailable config)
  , _endOfPacket   :: KeepBool (KeepEndOfPacket   config)
  , _irq           :: KeepBool (KeepIrq           config)
  , _readData      :: readDataType
  }
  deriving Generic

deriving instance (GoodMMS2MConfig config, NFDataX readDataType) => NFDataX (AvalonMMS2M config readDataType)
deriving instance (GoodMMS2MConfig config, NFData  readDataType) => NFData  (AvalonMMS2M config readDataType)
deriving instance (GoodMMS2MConfig config, ShowX   readDataType) => ShowX   (AvalonMMS2M config readDataType)
deriving instance (GoodMMS2MConfig config, Show    readDataType) => Show    (AvalonMMS2M config readDataType)
deriving instance (GoodMMS2MConfig config, Eq      readDataType) => Eq      (AvalonMMS2M config readDataType)


-- | Type for Avalon memory mapped protocol.
-- We do not support the bidirectional port @data@
data AvalonMM (dom :: Domain) (configS2M :: AvalonMMS2MConfig) (readDataType :: Type) (configM2S :: AvalonMMM2SConfig) (writeDataType :: Type)

instance Protocol (AvalonMM dom configS2M readDataType configM2S writeDataType) where
  type Fwd (AvalonMM dom configS2M readDataType configM2S writeDataType) = Signal dom (AvalonMMM2S configM2S writeDataType)
  type Bwd (AvalonMM dom configS2M readDataType configM2S writeDataType) = Signal dom (AvalonMMS2M configS2M readDataType)

instance (KnownDomain dom, GoodMMS2MConfig configS2M, GoodMMM2SConfig configM2S) => Backpressure (AvalonMM dom configS2M readDataType configM2S writeDataType) where
  boolsToBwd proxy = C.fromList_lazy . fmap (DfLike.boolToAck proxy)

instance (KnownDomain dom, GoodMMS2MConfig configS2M, GoodMMM2SConfig configM2S) => Simulate (AvalonMM dom configS2M readDataType configM2S writeDataType) where
  type SimulateFwdType (AvalonMM dom configS2M readDataType configM2S writeDataType) = [AvalonMMM2S configM2S writeDataType]
  type SimulateBwdType (AvalonMM dom configS2M readDataType configM2S writeDataType) = [AvalonMMS2M configS2M readDataType]
  type SimulateChannels (AvalonMM dom configS2M readDataType configM2S writeDataType) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = C.sample_lazy
  sigToSimBwd _ = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) = DfLike.stall Proxy conf stallAck stalls

-- | Grab the data from a master-to-slave message, if there is any
mmM2SToMaybe :: (GoodMMM2SConfig configM2S) => AvalonMMM2S configM2S writeDataType -> Maybe writeDataType
mmM2SToMaybe AvalonMMM2S{..} = if cond then Just _writeData else Nothing where
  cond =  fromKeepBool True _chipSelect
       && fromKeepBool True _write
       && not (fromKeepBool False _read)
  -- TODO look at byteenable

mmM2SNoData :: (GoodMMM2SConfig configM2S) => AvalonMMM2S configM2S writeDataType
mmM2SNoData
  = AvalonMMM2S
  { _chipSelect         = toKeepBool False
  , _addr               = 0
  , _read               = toKeepBool False
  , _write              = toKeepBool False
  , _byteEnable         = repeat False
  , _writeByteEnable    = repeat False
  , _beginTransfer      = toKeepBool False
  , _burstCount         = 0
  , _beginBurstTransfer = toKeepBool False
  , _writeData          = errorX "No writeData for noData"
  }

mmM2SSendingData :: (GoodMMM2SConfig configM2S) => AvalonMMM2S configM2S writeDataType
mmM2SSendingData
  = AvalonMMM2S
  { _chipSelect         = toKeepBool True
  , _addr               = 0
  , _read               = toKeepBool False
  , _write              = toKeepBool True
  , _byteEnable         = repeat True
  , _writeByteEnable    = repeat True
  , _beginTransfer      = toKeepBool False
  , _burstCount         = 0
  , _beginBurstTransfer = toKeepBool False
  , _writeData          = errorX "No writeData for mmM2SSendingData"
  }

boolToMMS2M :: (GoodMMS2MConfig configS2M) => Bool -> AvalonMMS2M configS2M readDataType
boolToMMS2M ack
  = AvalonMMS2M
    { _waitRequest   = toKeepBool (not ack)
    , _readDataValid = toKeepBool False
    , _readyForData  = toKeepBool ack
    , _dataAvailable = toKeepBool False
    , _endOfPacket   = toKeepBool False
    , _irq           = toKeepBool False
    , _readData      = errorX "No readData for boolToAck"
    }

mmS2MToBool :: (GoodMMS2MConfig configS2M) => AvalonMMS2M configS2M readDataType -> Bool
mmS2MToBool AvalonMMS2M{..} = fromKeepBool True _readyForData && not (fromKeepBool False _waitRequest)

instance (KnownDomain dom, GoodMMS2MConfig configS2M, GoodMMM2SConfig configM2S) => Drivable (AvalonMM dom configS2M readDataType configM2S writeDataType) where
  type ExpectType (AvalonMM dom configS2M readDataType configM2S writeDataType) = [writeDataType]

  -- | All the fields aside from @_data@ are left at zero/default values,
  -- except for packet fields, which assume that this is an individual packet (start=end=True).
  toSimulateType Proxy = fmap (\dat -> mmM2SSendingData { _writeData = dat })
  fromSimulateType Proxy = Maybe.mapMaybe mmM2SToMaybe

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy

instance (KnownDomain dom, GoodMMS2MConfig configS2M, GoodMMM2SConfig configM2S) => DfLike dom (AvalonMM dom configS2M readDataType configM2S) writeDataType where
  type Data (AvalonMM dom configS2M readDataType configM2S) writeDataType = AvalonMMM2S configM2S writeDataType
  type Payload writeDataType = writeDataType
  type Ack (AvalonMM dom configS2M readDataType configM2S) writeDataType = AvalonMMS2M configS2M readDataType

  getPayload = const $ mmM2SToMaybe

  setPayload _ _ m2s (Just b) = m2s { _writeData = b }
  setPayload _ pxy _ Nothing = DfLike.noData pxy

  noData _ = mmM2SNoData

  boolToAck _ = boolToMMS2M
  ackToBool _ = mmS2MToBool

instance (KnownDomain dom, GoodMMS2MConfig configS2M, GoodMMM2SConfig configM2S, Show writeDataType, ShowX writeDataType, NFData writeDataType, NFDataX writeDataType, Eq writeDataType, Show readDataType, ShowX readDataType, NFData readDataType, NFDataX readDataType, Eq readDataType) => Test (AvalonMM dom configS2M readDataType configM2S writeDataType) where
  expectToLengths Proxy = pure . length

  -- directly copied from Df instance, with minor changes made
  expectN Proxy (ExpectOptions{eoEmptyTail, eoTimeout}) (C.head -> nExpected) sampled = do
    go (Maybe.fromMaybe maxBound eoTimeout) nExpected sampled
   where
    catDatas [] = []
    catDatas (m2s:xs) = Maybe.maybe (catDatas xs) (:catDatas xs) (mmM2SToMaybe m2s)

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

    go timeout n (a:as) | Maybe.isNothing (mmM2SToMaybe a) = do
      -- Circuit did not output valid cycle, just continue
      go (pred timeout) n as
    go _ n (_:as) =
      -- Circuit produced a valid cycle, reset timeout
      go (Maybe.fromMaybe maxBound eoTimeout) (pred n) as
