{-|
Types and instance declarations for the Avalon-stream protocol.
-}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, UndecidableInstances #-}

module Protocols.Avalon.Stream.AvalonStream where

-- base
import           Control.DeepSeq (NFData)
import           Prelude

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


-- | Data sent from master to slave.
-- The tvalid field is left out: messages with
-- @tvalid = False@ should be sent as a @NoAvalonStreamM2S@.
data AvalonStreamM2S channelWidth errorWidth dataType
  = NoAvalonStreamM2S
  | AvalonStreamM2S
  {
    _data    :: dataType,
    _channel :: Unsigned channelWidth,
    _error   :: Unsigned errorWidth
  }
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)
-- TODO options to leave stuff out
-- TODO packet transfer signals

-- | Data sent from slave to master.
-- A simple acknowledge message.
data AvalonStreamS2M (readyLatency :: Nat) = AvalonStreamS2M { _ready :: Bool } deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

-- | Type for Avalon Stream protocol.
data AvalonStream (dom :: Domain) (readyLatency :: Nat) (channelWidth :: Nat) (errorWidth :: Nat) (dataType :: Type)

instance Protocol (AvalonStream dom readyLatency channelWidth errorWidth dataType) where
  type Fwd (AvalonStream dom readyLatency channelWidth errorWidth dataType) = Signal dom (AvalonStreamM2S channelWidth errorWidth dataType)
  type Bwd (AvalonStream dom readyLatency channelWidth errorWidth dataType) = Signal dom (AvalonStreamS2M readyLatency)

instance Backpressure (AvalonStream dom 0 channelWidth errorWidth dataType) where
  boolsToBwd _ = C.fromList_lazy . fmap AvalonStreamS2M

instance (C.KnownDomain dom, KnownNat channelWidth, KnownNat errorWidth) => Simulate (AvalonStream dom readyLatency channelWidth errorWidth dataType) where
  type SimulateFwdType (AvalonStream dom readyLatency channelWidth errorWidth dataType) = [AvalonStreamM2S channelWidth errorWidth dataType]
  type SimulateBwdType (AvalonStream dom readyLatency channelWidth errorWidth dataType) = [AvalonStreamS2M readyLatency]
  type SimulateChannels (AvalonStream dom readyLatency channelWidth errorWidth dataType) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = C.sample_lazy
  sigToSimBwd _ = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) = DfLike.stall Proxy conf stallAck stalls

-- | Grab the data from a master-to-slave message, if there is any
streamM2SToMaybe :: AvalonStreamM2S channelWidth errorWidth dataType -> Maybe dataType
streamM2SToMaybe NoAvalonStreamM2S = Nothing
streamM2SToMaybe m2s = Just (_data m2s)

instance (C.KnownDomain dom, KnownNat channelWidth, KnownNat errorWidth) => Drivable (AvalonStream dom 0 channelWidth errorWidth dataType) where
  type ExpectType (AvalonStream dom 0 channelWidth errorWidth dataType) = [dataType]

  -- | All the fields aside from @_data@ are left at zero/default values.
  toSimulateType Proxy = fmap (\dat -> (AvalonStreamM2S { _data = dat, _channel = 0, _error = 0 }))
  fromSimulateType Proxy = Maybe.mapMaybe streamM2SToMaybe

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy

instance (KnownNat channelWidth, KnownNat errorWidth) => DfLike dom (AvalonStream dom readyLatency channelWidth errorWidth) dataType where
  type Data (AvalonStream dom readyLatency channelWidth errorWidth) dataType = AvalonStreamM2S channelWidth errorWidth dataType
  type Payload dataType = dataType
  type Ack (AvalonStream dom readyLatency channelWidth errorWidth) dataType = AvalonStreamS2M readyLatency

  getPayload = const $ streamM2SToMaybe

  -- | If we try to give data to a @NoAvalonStreamM2S@, all the fields aside from @_data@ are left at zero/default values.
  setPayload _ _ NoAvalonStreamM2S (Just b) = AvalonStreamM2S { _data = b, _channel = 0, _error = 0 }
  setPayload _ _ m2s (Just b) = m2s { _data = b }
  setPayload _ _ _ Nothing = NoAvalonStreamM2S

  noData _ = NoAvalonStreamM2S

  boolToAck _ = AvalonStreamS2M
  ackToBool _ = _ready

instance (C.KnownDomain dom, KnownNat channelWidth, KnownNat errorWidth, Show dataType, ShowX dataType, NFData dataType, NFDataX dataType, Eq dataType) => Test (AvalonStream dom 0 channelWidth errorWidth dataType) where
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
