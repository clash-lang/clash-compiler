{-|
Copyright  :  (C) 2022, Google Inc,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Cores.Xilinx.DcFifo.Internal.Types where

import Clash.Prelude

type Full = Bool
type Empty = Bool
type DataCount n = Unsigned n

-- | Parameters for the FIFO generator; toggle various signals.
--
-- The @depth@ parameter configures the number of elements the FIFO will hold,
-- however the actual number of elements is equal to @2^depth - 1 @. So
-- @dcDepth=d4@ will create a FIFO with an actual depth of 15 elements. The
-- range of @depth@ is 4 through 17 inclusive: the deepest FIFO can hold 131,071
-- elements.
--
-- Example:
--
-- @
-- fifo = dcFifo DcConfig{ dcDepth=d5
--                       , dcReadDataCount=False
--                       , dcWriteDataCount=False
--                       , dcOverflow=True
--                       , dcUnderflow=True
--                       }
-- @
data DcConfig depth = DcConfig
  { -- | The FIFO will be able to hold @2^depth - 1@ elements.
    dcDepth :: !(SNat depth)
  -- | Enable @rd_data_count@ signal
  , dcReadDataCount :: !Bool
  -- | Enable @wr_data_count@ signal
  , dcWriteDataCount :: !Bool
  -- | Enable @overflow@ signal
  , dcOverflow :: !Bool
  -- | Enable @underflow@ signal
  , dcUnderflow :: !Bool
  }
  deriving (Show, Generic)

-- | Output of 'Clash.Cores.Xilinx.DcFifo.dcFifo'
data FifoOut read write depth a =
  FifoOut
    {
    -- | @full@. Full Flag: When asserted, this signal indicates that the FIFO
    -- is full. Write requests are ignored when the FIFO is full, initiating a
    -- write when the FIFO is full is not destructive to the contents of the
    -- FIFO.
      isFull :: Signal write Full
    -- | @overflow@. Overflow: This signal indicates that a write request
    -- (@wr_en@) during the prior clock cycle was rejected, because the FIFO is
    -- full. Overflowing the FIFO is not destructive to the FIFO.
    --
    -- This signal will return 'XException' when 'dcOverflow' is disabled.
    , isOverflow :: Signal write Bool
    -- | @wr_data_count@. Write Data Count: This bus indicates the number of
    -- words written into the FIFO. The count is guaranteed to never
    -- under-report the number of words in the FIFO, to ensure you never
    -- overflow the FIFO. The exception to this behavior is when a write
    -- operation occurs at the rising edge of @wr_clk@, that write operation
    -- will only be reflected on @wr_data_count@ at the next rising clock edge.
    --
    -- This signal will return 'XException' when 'dcWriteDataCount' is disabled.
    , writeCount :: Signal write (DataCount depth)
    -- | @empty@. Empty Flag: When asserted, this signal indicates that the FIFO
    -- is empty. Read requests are ignored when the FIFO is empty, initiating a
    -- read while empty is not destructive to the FIFO.
    , isEmpty :: Signal read Empty
    -- | @underflow@. Underflow: Indicates that read request (@rd_en@) during
    -- the previous clock cycle was rejected because the FIFO is empty.
    -- Underflowing the FIFO is not destructive to the FIFO.
    --
    -- This signal will return 'XException' when 'dcUnderflow' is disabled.
    , isUnderflow :: Signal read Bool
    -- | @rd_data_count@. Read Data Count: This bus indicates the number of
    -- words available for reading in the FIFO. The count is guaranteed to never
    -- over-report the number of words available for reading, to ensure that you
    -- do not underflow the FIFO. The exception to this behavior is when the
    -- read operation occurs at the rising edge of @rd_clk@, that read operation
    -- is only reflected on @rd_data_count@ at the next rising clock edge.
    --
    -- This signal will return 'XException' when 'dcReadDataCount' is disabled.
    , readCount :: Signal read (DataCount depth)
    -- | @dout@. Data Output: The output data bus is driven in the cycle after
    -- asserting the read enable @rd_en@.
    , fifoData :: Signal read a
    }
