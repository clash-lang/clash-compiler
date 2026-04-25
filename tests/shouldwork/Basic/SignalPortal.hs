{-# LANGUAGE CPP #-}

module SignalPortal where

import Clash.Prelude
import Clash.Explicit.Testbench

#ifdef OUTPUTTEST
import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))
#endif

producerLeaf :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
producerLeaf x = portalSource "probe" (x + 1)
{-# OPAQUE producerLeaf #-}

producerMiddle :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
producerMiddle x = producerLeaf x + 3
{-# OPAQUE producerMiddle #-}

producerBranch :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
producerBranch x = producerMiddle x + 5
{-# OPAQUE producerBranch #-}

consumerLeaf :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
consumerLeaf x = portalSink "probe" + x
{-# OPAQUE consumerLeaf #-}

consumerMiddle :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
consumerMiddle x = consumerLeaf (x + 7) + 11
{-# OPAQUE consumerMiddle #-}

consumerBranch :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
consumerBranch x = consumerMiddle x
{-# OPAQUE consumerBranch #-}

islandProducer :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
islandProducer x = portalSource "local" (x + 2)
{-# OPAQUE islandProducer #-}

islandConsumer :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
islandConsumer x = portalSink "local" + x
{-# OPAQUE islandConsumer #-}

portalIsland :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
portalIsland x = islandProducer x + islandConsumer x
{-# OPAQUE portalIsland #-}

topEntity
  :: SystemClockResetEnable
  => Signal System (Unsigned 8)
  -> Signal System (Unsigned 8, Unsigned 8, Unsigned 8)
topEntity x = bundle (producerBranch x, consumerBranch x, portalIsland x)
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput =
    stimuliGenerator clk rst (10 :> 11 :> 12 :> 13 :> Nil)
  expectedOutput =
    outputVerifier' clk rst
      ( (19, 39, 34) :> (20, 41, 37) :> (21, 43, 40) :> (22, 45, 43) :> Nil )
  done =
    expectedOutput (exposeClockResetEnable topEntity clk rst enableGen testInput)
  clk =
    tbSystemClockGen (not <$> done)
  rst =
    systemResetGen
{-# OPAQUE testBench #-}

#ifdef OUTPUTTEST
assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack
  | P.not (needle `isInfixOf` haystack) = return ()
  | otherwise =
      P.error $
        P.concat
          [ "Did not expect:\n\n  "
          , needle
          , "\n\nIn:\n\n"
          , haystack
          ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")
  assertNotIn "portal_local" content
#endif
