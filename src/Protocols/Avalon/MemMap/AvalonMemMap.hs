{-|
Types and instance declarations for the Avalon memory mapped protocol.
-}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, TypeFamilyDependencies, UndecidableInstances #-}

module Protocols.Avalon.MemMap.AvalonMemMap where

-- base
import           Control.DeepSeq (NFData)
import           Prelude hiding (not)

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

class KeepBoolClass (keep :: Bool) where
  fromKeepBool :: Bool -> KeepBool keep -> Bool
  toKeepBool   :: Bool -> KeepBool keep

instance KeepBoolClass 'True where
  fromKeepBool _ = id
  toKeepBool = id

instance KeepBoolClass 'False where
  fromKeepBool b = const b
  toKeepBool = const ()

-- | Data sent from master to slave.
data AvalonMMM2S keepChipSelect addrWidth keepRead keepWrite byteEnableWidth writeByteEnableWidth keepBeginTransfer burstCountWidth keepBeginBurstTransfer writeDataType
  = AvalonMMM2S
  { _chipSelect         :: KeepBool keepChipSelect
  , _addr               :: Unsigned addrWidth
  , _read               :: KeepBool keepRead
  , _keepWrite          :: KeepBool keepWrite
  , _byteEnable         :: Vec byteEnableWidth Bool
  , _writeByteEnable    :: Vec writeByteEnableWidth Bool
  , _beginTransfer      :: KeepBool keepBeginTransfer
  , _burstCount         :: Unsigned burstCountWidth
  , _beginBurstTransfer :: KeepBool keepBeginBurstTransfer
  , _writeData          :: writeDataType
  }
  deriving Generic

deriving instance
  ( C.NFDataX (KeepBool keepChipSelect)
  , C.NFDataX (KeepBool keepRead)
  , C.NFDataX (KeepBool keepWrite)
  , C.NFDataX (KeepBool keepBeginTransfer)
  , C.NFDataX (KeepBool keepBeginBurstTransfer)
  , C.NFDataX writeDataType
  , KnownNat addrWidth
  , KnownNat byteEnableWidth
  , KnownNat writeByteEnableWidth
  , KnownNat burstCountWidth
  )
  => C.NFDataX (AvalonMMM2S keepChipSelect addrWidth keepRead keepWrite byteEnableWidth writeByteEnableWidth keepBeginTransfer burstCountWidth keepBeginBurstTransfer writeDataType)

deriving instance
  ( NFData (KeepBool keepChipSelect)
  , NFData (KeepBool keepRead)
  , NFData (KeepBool keepWrite)
  , NFData (KeepBool keepBeginTransfer)
  , NFData (KeepBool keepBeginBurstTransfer)
  , NFData writeDataType
  , KnownNat addrWidth
  , KnownNat byteEnableWidth
  , KnownNat writeByteEnableWidth
  , KnownNat burstCountWidth
  )
  => NFData (AvalonMMM2S keepChipSelect addrWidth keepRead keepWrite byteEnableWidth writeByteEnableWidth keepBeginTransfer burstCountWidth keepBeginBurstTransfer writeDataType)

deriving instance
  ( C.ShowX (KeepBool keepChipSelect)
  , C.ShowX (KeepBool keepRead)
  , C.ShowX (KeepBool keepWrite)
  , C.ShowX (KeepBool keepBeginTransfer)
  , C.ShowX (KeepBool keepBeginBurstTransfer)
  , C.ShowX writeDataType
  , KnownNat addrWidth
  , KnownNat byteEnableWidth
  , KnownNat writeByteEnableWidth
  , KnownNat burstCountWidth
  )
  => C.ShowX (AvalonMMM2S keepChipSelect addrWidth keepRead keepWrite byteEnableWidth writeByteEnableWidth keepBeginTransfer burstCountWidth keepBeginBurstTransfer writeDataType)

deriving instance
  ( Show (KeepBool keepChipSelect)
  , Show (KeepBool keepRead)
  , Show (KeepBool keepWrite)
  , Show (KeepBool keepBeginTransfer)
  , Show (KeepBool keepBeginBurstTransfer)
  , Show writeDataType
  , KnownNat addrWidth
  , KnownNat byteEnableWidth
  , KnownNat writeByteEnableWidth
  , KnownNat burstCountWidth
  )
  => Show (AvalonMMM2S keepChipSelect addrWidth keepRead keepWrite byteEnableWidth writeByteEnableWidth keepBeginTransfer burstCountWidth keepBeginBurstTransfer writeDataType)

-- | Data sent from slave to master.
data AvalonMMS2M keepWaitRequest keepReadDataValid keepReadyForData keepDataAvailable keepEndOfPacket keepIrq readDataType
  = AvalonMMS2M
  { _waitRequest   :: KeepBool keepWaitRequest
  , _readDataValid :: KeepBool keepReadDataValid
  , _readyForData  :: KeepBool keepReadyForData
  , _dataAvailable :: KeepBool keepDataAvailable
  , _endOfPacket   :: KeepBool keepEndOfPacket
  , _irq           :: KeepBool keepIrq
  , _readData      :: readDataType
  }
  deriving Generic

deriving instance
  ( C.NFDataX (KeepBool keepWaitRequest)
  , C.NFDataX (KeepBool keepReadDataValid)
  , C.NFDataX (KeepBool keepReadyForData)
  , C.NFDataX (KeepBool keepDataAvailable)
  , C.NFDataX (KeepBool keepEndOfPacket)
  , C.NFDataX (KeepBool keepIrq)
  , C.NFDataX readDataType
  )
  => C.NFDataX (AvalonMMS2M keepWaitRequest keepReadDataValid keepReadyForData keepDataAvailable keepEndOfPacket keepIrq readDataType)

deriving instance
  ( NFData (KeepBool keepWaitRequest)
  , NFData (KeepBool keepReadDataValid)
  , NFData (KeepBool keepReadyForData)
  , NFData (KeepBool keepDataAvailable)
  , NFData (KeepBool keepEndOfPacket)
  , NFData (KeepBool keepIrq)
  , NFData readDataType
  )
  => NFData (AvalonMMS2M keepWaitRequest keepReadDataValid keepReadyForData keepDataAvailable keepEndOfPacket keepIrq readDataType)

deriving instance
  ( C.ShowX (KeepBool keepWaitRequest)
  , C.ShowX (KeepBool keepReadDataValid)
  , C.ShowX (KeepBool keepReadyForData)
  , C.ShowX (KeepBool keepDataAvailable)
  , C.ShowX (KeepBool keepEndOfPacket)
  , C.ShowX (KeepBool keepIrq)
  , C.ShowX readDataType
  )
  => C.ShowX (AvalonMMS2M keepWaitRequest keepReadDataValid keepReadyForData keepDataAvailable keepEndOfPacket keepIrq readDataType)

deriving instance
  ( Show (KeepBool keepWaitRequest)
  , Show (KeepBool keepReadDataValid)
  , Show (KeepBool keepReadyForData)
  , Show (KeepBool keepDataAvailable)
  , Show (KeepBool keepEndOfPacket)
  , Show (KeepBool keepIrq)
  , Show readDataType
  )
  => Show (AvalonMMS2M keepWaitRequest keepReadDataValid keepReadyForData keepDataAvailable keepEndOfPacket keepIrq readDataType)

-- | Type for Avalon memory mapped protocol.
data AvalonStream (dom :: Domain) (keepChipSelect :: Bool) (addrWidth :: Nat) (keepRead :: Bool) (keepWrite :: Bool) (byteEnableWidth :: Nat) (writeByteEnableWidth :: Nat) (keepBeginTransfer :: Bool) (burstCountWidth :: Nat) (keepBeginBurstTransfer :: Bool) (writeDataType :: Type) (keepWaitRequest :: Bool) (keepReadDataValid :: Bool) (keepReadyForData :: Bool) (keepDataAvailable :: Bool) (keepEndOfPacket :: Bool) (keepIrq :: Bool) (readDataType :: Type)

instance Protocol (AvalonStream dom keepChipSelect addrWidth keepRead keepWrite byteEnableWidth writeByteEnableWidth keepBeginTransfer burstCountWidth keepBeginBurstTransfer writeDataType keepWaitRequest keepReadDataValid keepReadyForData keepDataAvailable keepEndOfPacket keepIrq readDataType) where
  type Fwd (AvalonStream dom keepChipSelect addrWidth keepRead keepWrite byteEnableWidth writeByteEnableWidth keepBeginTransfer burstCountWidth keepBeginBurstTransfer writeDataType keepWaitRequest keepReadDataValid keepReadyForData keepDataAvailable keepEndOfPacket keepIrq readDataType) = Signal dom (AvalonMMM2S keepChipSelect addrWidth keepRead keepWrite byteEnableWidth writeByteEnableWidth keepBeginTransfer burstCountWidth keepBeginBurstTransfer writeDataType)
  type Bwd (AvalonStream dom keepChipSelect addrWidth keepRead keepWrite byteEnableWidth writeByteEnableWidth keepBeginTransfer burstCountWidth keepBeginBurstTransfer writeDataType keepWaitRequest keepReadDataValid keepReadyForData keepDataAvailable keepEndOfPacket keepIrq readDataType) = Signal dom (AvalonMMS2M keepWaitRequest keepReadDataValid keepReadyForData keepDataAvailable keepEndOfPacket keepIrq readDataType)

instance (KeepBoolClass keepWaitRequest, KeepBoolClass keepReadDataValid, KeepBoolClass keepReadyForData, KeepBoolClass keepDataAvailable, KeepBoolClass keepEndOfPacket, KeepBoolClass keepIrq) => Backpressure (AvalonStream dom keepChipSelect addrWidth keepRead keepWrite byteEnableWidth writeByteEnableWidth keepBeginTransfer burstCountWidth keepBeginBurstTransfer writeDataType keepWaitRequest keepReadDataValid keepReadyForData keepDataAvailable keepEndOfPacket keepIrq readDataType) where
  boolsToBwd _ = C.fromList_lazy . fmap f where
    f ack = AvalonMMS2M
      { _waitRequest   = toKeepBool (not ack)
      , _readDataValid = toKeepBool False
      , _readyForData  = toKeepBool ack
      , _dataAvailable = toKeepBool False
      , _endOfPacket   = toKeepBool False
      , _irq           = toKeepBool False
      , _readData      = errorX "No readData for backpressure"
      }

{-
instance (C.KnownDomain dom, KnownNat channelWidth, KnownNat errorWidth, KnownNat emptyWidth) => Simulate (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) where
  type SimulateFwdType (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = [AvalonStreamM2S channelWidth errorWidth emptyWidth dataType]
  type SimulateBwdType (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = [AvalonStreamS2M readyLatency]
  type SimulateChannels (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth dataType) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = C.sample_lazy
  sigToSimBwd _ = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) = DfLike.stall Proxy conf stallAck stalls

-- | Grab the data from a master-to-slave message, if there is any
streamM2SToMaybe :: AvalonStreamM2S channelWidth errorWidth emptyWidth dataType -> Maybe dataType
streamM2SToMaybe NoAvalonStreamM2S = Nothing
streamM2SToMaybe m2s = Just (_data m2s)

instance (C.KnownDomain dom, KnownNat channelWidth, KnownNat errorWidth, KnownNat emptyWidth) => Drivable (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType) where
  type ExpectType (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType) = [dataType]

  -- | All the fields aside from @_data@ are left at zero/default values,
  -- except for packet fields, which assume that this is an individual packet (start=end=True).
  toSimulateType Proxy = fmap (\dat -> (AvalonStreamM2S { _data = dat, _channel = 0, _error = 0, _startofpacket = True, _endofpacket = True, _empty = 0 }))
  fromSimulateType Proxy = Maybe.mapMaybe streamM2SToMaybe

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy

instance (KnownNat channelWidth, KnownNat errorWidth, KnownNat emptyWidth) => DfLike dom (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth) dataType where
  type Data (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth) dataType = AvalonStreamM2S channelWidth errorWidth emptyWidth dataType
  type Payload dataType = dataType
  type Ack (AvalonStream dom readyLatency channelWidth errorWidth emptyWidth) dataType = AvalonStreamS2M readyLatency

  getPayload = const $ streamM2SToMaybe

  -- | If we try to give data to a @NoAvalonStreamM2S@, all the fields aside from @_data@ and packet fields are left at zero/default values.
  -- Packet fields assume this is an individual packet (start=end=True).
  setPayload _ _ NoAvalonStreamM2S (Just b) = AvalonStreamM2S { _data = b, _channel = 0, _error = 0, _startofpacket = True, _endofpacket = True, _empty = 0 }
  setPayload _ _ m2s (Just b) = m2s { _data = b }
  setPayload _ _ _ Nothing = NoAvalonStreamM2S

  noData _ = NoAvalonStreamM2S

  boolToAck _ = AvalonStreamS2M
  ackToBool _ = _ready

instance (C.KnownDomain dom, KnownNat channelWidth, KnownNat errorWidth, KnownNat emptyWidth, Show dataType, ShowX dataType, NFData dataType, NFDataX dataType, Eq dataType) => Test (AvalonStream dom 0 channelWidth errorWidth emptyWidth dataType) where
  expectToLengths Proxy = pure . length

  -- directly copied from Df instance, with minor changes made
  expectN Proxy (ExpectOptions{eoEmptyTail, eoTimeout}) (C.head -> nExpected) sampled = do
    go (Maybe.fromMaybe maxBound eoTimeout) nExpected sampled
   where
    catDatas [] = []
    catDatas (NoAvalonStreamM2S:xs) = catDatas xs
    catDatas (AvalonStreamM2S{_data=x}:xs) = x:catDatas xs

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

    go timeout n (NoAvalonStreamM2S:as) = do
      -- Circuit did not output valid cycle, just continue
      go (pred timeout) n as
    go _ n (AvalonStreamM2S{}:as) =
      -- Circuit produced a valid cycle, reset timeout
      go (Maybe.fromMaybe maxBound eoTimeout) (pred n) as
-}
