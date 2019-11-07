module Test.Cores.Base
  ( SPISamples(..)
  , sampleSPI
  -- TODO Export a function from here to allow sampling from one master and
  -- multiple slaves. Then this wouldn't need to be exported for Test.Cores.SPI.
  , masterInBP
  ) where

import qualified Data.List as L (unzip4)

import           Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import           Clash.Cores.SPI


masterInBP
  :: (KnownDomain dom)
  => Clock dom
  -> Reset dom
  -> BitVector n
  -> Signal dom Bool
  -> Signal dom (Maybe (BitVector n))
masterInBP clk rst val =
  E.moore clk rst enableGen
    (flip const)
    (\busy -> if busy then Nothing else Just val)
    True

-- Generally speaking, test units will be able to determine passing or failing
-- by looking at the values of the master and slave, and the acknolwedgement
-- signals sent over time.
--
data SPISamples master slave = SPISamples
  { ssMasterOut :: [Maybe (BitVector master)]
  , ssMasterAck :: [Bool]
  , ssSlaveOut  :: [Maybe (BitVector slave)]
  , ssSlaveAck  :: [Bool]
  } deriving (Eq, Show)

-- Sample from an SPI device with a single master and a single slave. The
-- master and slave can send different width data to eachother.
--
sampleSPI
  :: ( KnownNat master, KnownNat slave
     , 1 <= halfPeriod, 1 <= waitTime, 1 <= master, 1 <= slave
     )
  => SPIMode
  -- ^ The mode of the SPI core
  -> Bool
  -- ^ Whether the SPI slave latches signals
  -> SNat halfPeriod
  -- ^ Half-period of the clock divider for the SPI master
  -> SNat waitTime
  -- ^ Core clock cycles between de-assertion of slave-select
  -- and the start of the SPI clock
  -> BitVector master
  -- ^ Value master sends to slave
  -> BitVector slave
  -- ^ Value slave sends to master
  -> Int
  -- ^ Sample duration
  -> SPISamples master slave
  -- ^ Samples of outputs and acknowledgements for the master and slave
sampleSPI mode latch divHalf wait mVal sVal duration =
  (\(mO, mA, sO, sA) -> SPISamples mO mA sO sA) $ L.unzip4 samples
 where
  samples = sampleN duration
    $ bundle (masterOut, masterAck, slaveOut, slaveAck)

  (misoZ, slaveAck, slaveOut) =
    exposeSpecificClockResetEnable spiSlaveLatticeSBIO clk rst enableGen
      mode latch sclk mosi miso ss (pure sVal)

  miso = veryUnsafeToBiSignalIn misoZ
  masterIn = masterInBP clk rst mVal bp

  (sclk, mosi, ss, bp, masterAck, masterOut) =
    exposeSpecificClockResetEnable spiMaster clk rst enableGen
      mode divHalf wait masterIn (readFromBiSignal miso)

  clk = systemClockGen
  rst = systemResetGen

