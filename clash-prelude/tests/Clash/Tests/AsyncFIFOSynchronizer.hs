{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Clash.Tests.AsyncFIFOSynchronizer (tests) where

import Data.Maybe (isJust, catMaybes)
import qualified Prelude as P

import Hedgehog as H
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog.Extra
import Test.Tasty.HUnit

import Clash.Explicit.Prelude

createDomain vSystem{vName = "Slow", vPeriod = 2 * vPeriod vSystem}

sync12RW
  :: ( KnownDomain dom1
     , KnownDomain dom2
     )
  => Int
  -> Int
  -> Int
  -> ( Signal dom1 (Bool, (Int, Bool))
     , Signal dom2 (Maybe Int, Bool)
     )
sync12RW w1 w2 w3 =
  ( bundle (inputRInc, bundle (rdata, rempty))
  , bundle (inputWDataM, wfull)
  )
 where
  (rdata, rempty, wfull) =
    asyncFIFOSynchronizer d4 clockGen clockGen resetGen resetGen enableGen
      enableGen inputRInc inputWDataM
  inputRInc = fromList $ P.replicate w1 False <> P.replicate 4 True <>
                P.replicate w2 False <> P.replicate 4 True <> P.repeat False
  inputWDataM = fromList $ Nothing : P.map Just [1 .. 4] <>
                  P.replicate w3 Nothing <> P.map Just [5 .. 8] <>
                  P.repeat Nothing

-- Test n.1:
--  - Write 4
--  - Empty as soon as we can stream all in a row
--  - Stay empty for a moment
--  - Write another 4
--  - Empty as soon as we can stream all in a row

sync1R1 :: Signal Slow (Bool, (Int, Bool))
sync1W1 :: Signal System (Maybe Int, Bool)
(sync1R1, sync1W1) = sync12RW 4 2 7

sync2R1 :: Signal System (Bool, (Int, Bool))
sync2W1 :: Signal Slow (Maybe Int, Bool)
(sync2R1, sync2W1) = sync12RW 9 6 1

-- Test n.2:
--  - Write 4
--  - Don't start immediately, but then empty all in a row
--  - Stay empty for a moment
--  - Write another 4
--  - Empty as soon as we can stream all in a row

sync1R2 :: Signal Slow (Bool, (Int, Bool))
sync1W2 :: Signal System (Maybe Int, Bool)
(sync1R2, sync1W2) = sync12RW 5 2 8

sync2R2 :: Signal System (Bool, (Int, Bool))
sync2W2 :: Signal Slow (Maybe Int, Bool)
(sync2R2, sync2W2) = sync12RW 10 10 1

-- Test n.3:
--  - Write 4
--  - Empty as soon as we can stream all in a row
--  - Stay empty for a moment
--  - Write another 4
--  - Don't start immediately, but then empty all in a row

sync1R3 :: Signal Slow (Bool, (Int, Bool))
sync1W3 :: Signal System (Maybe Int, Bool)
(sync1R3, sync1W3) = sync12RW 4 4 7

sync2R3 :: Signal System (Bool, (Int, Bool))
sync2W3 :: Signal Slow (Maybe Int, Bool)
(sync2R3, sync2W3) = sync12RW 9 8 1

-- Test n.4:
--  - Write 4
--  - Don't start immediately, but then empty all in a row
--  - Stay empty for a moment
--  - Write another 4
--  - Don't start immediately, but then empty all in a row

sync1R4 :: Signal Slow (Bool, (Int, Bool))
sync1W4 :: Signal System (Maybe Int, Bool)
(sync1R4, sync1W4) = sync12RW 5 3 8

sync2R4 :: Signal System (Bool, (Int, Bool))
sync2W4 :: Signal Slow (Maybe Int, Bool)
(sync2R4, sync2W4) = sync12RW 10 12 1

sync34RW
  :: ( KnownDomain dom1
     , KnownDomain dom2
     )
  => Int
  -> Int
  -> Int
  -> Int
  -> ( Signal dom1 (Bool, (Int, Bool))
     , Signal dom2 (Maybe Int, Bool)
     )
sync34RW w1 n1 w2 w3 =
  ( bundle (inputRInc, bundle (rdata, rempty))
  , bundle (inputWDataM, wfull)
  )
 where
  (rdata, rempty, wfull) =
    asyncFIFOSynchronizer d4 clockGen clockGen resetGen resetGen enableGen
      enableGen inputRInc inputWDataM
  inputRInc = fromList $ P.replicate w1 False <> P.replicate n1 True <>
                P.replicate w2 False <> P.replicate 16 True <> P.repeat False
  inputWDataM = fromList $ Nothing : P.map Just [1 .. n1] <>
                  P.replicate w3 Nothing <> P.map Just [n1 + 1 .. n1 + 16] <>
                  P.repeat Nothing

-- Test n.5: Fully fill the FIFO, then empty it again

sync3R5 :: Signal Slow (Bool, (Int, Bool))
sync3W5 :: Signal System (Maybe Int, Bool)
(sync3R5, sync3W5) = sync34RW 0 0 7 0

sync4R5 :: Signal System (Bool, (Int, Bool))
sync4W5 :: Signal Slow (Maybe Int, Bool)
(sync4R5, sync4W5) = sync34RW 0 0 28 0

-- Test n.6: Transfer 3 elements (so pointers are not zero), then fully fill
-- the FIFO, then empty it again

sync3R6 :: Signal Slow (Bool, (Int, Bool))
sync3W6 :: Signal System (Maybe Int, Bool)
(sync3R6, sync3W6) = sync34RW 4 3 4 4

sync4R6 :: Signal System (Bool, (Int, Bool))
sync4W6 :: Signal Slow (Maybe Int, Bool)
(sync4R6, sync4W6) = sync34RW 8 3 25 1

sync56RW
  :: ( KnownDomain dom1
     , KnownDomain dom2
     )
  => ( Signal dom1 (Bool, (Int, Bool))
     , Signal dom2 (Maybe Int, Bool)
     )
sync56RW =
  ( bundle (inputRInc, bundle (rdata, rempty))
  , bundle (inputWDataM, wfull)
  )
 where
  (rdata, rempty, wfull) =
    asyncFIFOSynchronizer d4 clockGen clockGen resetGen resetGen enableGen
      enableGen inputRInc inputWDataM
  inputRInc = not <$> rempty
  inputWDataM = fromList $ Nothing : P.map Just [1 .. 17] <> P.repeat Nothing

-- Test n.7: Read data as quickly as it comes, all through the memory and one
--           beyond the wrap of the circular buffer

sync5R7 :: Signal Slow (Bool, (Int, Bool))
sync5W7 :: Signal System (Maybe Int, Bool)
(sync5R7, sync5W7) = sync56RW

sync6R7 :: Signal System (Bool, (Int, Bool))
sync6W7 :: Signal Slow (Maybe Int, Bool)
(sync6R7, sync6W7) = sync56RW

-- For use with syncXRY syncXWY.
-- It prints (sample number, (input, output))
printTest
  :: (Foldable f, NFDataX a, ShowX a)
  => f a
  -> IO ()
printTest t = P.mapM_ (putStrLn . showX) $ P.zip [1 :: Int ..] $ sampleN 60 t

sampleR
  :: Foldable f
  => Int
  -> f (Bool, (Int, Bool))
  -> [(Bool, (Maybe Int, Bool))]
sampleR n r =
  P.map (\(inputRInc, (rdata, rempty)) -> (inputRInc, (maybeIsX rdata, rempty)))
    (sampleN n r)

-- For creating the expected constants below
printR
  :: Foldable f
  => f (Bool, (Int, Bool))
  -> IO ()
printR r = case P.map show $ sampleR 60 r of
  (first:rest) ->
    putStrLn $ (P.foldl (\b a -> b <> "  , " <> a <> "\n")
                  ("  [ " <> first <> "\n") rest) <> "  ]"
  _ -> error "impossible"

printW
  :: (Foldable f, NFDataX a, Show a)
  => f a
  -> IO ()
printW w = case P.map show $ sampleN 60 w of
  (first:rest) ->
    putStrLn $ (P.foldl (\b a -> b <> "  , " <> a <> "\n")
                  ("  [ " <> first <> "\n") rest) <> "  ]"
  _ -> error "impossible"

testR
  :: Foldable f
  => f (Bool, (Int, Bool))
  -> [(Bool, (Maybe Int, Bool))]
  -> Assertion
testR r expected = sampleR (P.length expected) r @?= expected

testW
  :: (Foldable f, NFDataX a, Eq a, Show a)
  => f a
  -> [a]
  -> Assertion
testW w expected = sampleN (P.length expected) w @?= expected


test1R1 :: Assertion
test1R1 = testR sync1R1
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,True))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (False,(Just 5,True))
  , (False,(Just 5,True))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (False,(Nothing,True))
  ]

test1W1 :: Assertion
test1W1 = testW sync1W1
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Nothing,False)
  ]

test2R1 :: Assertion
test2R1 = testR sync2R1
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 5,True))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (False,(Nothing,True))
  ]

test2W1 :: Assertion
test2W1 = testW sync2W1
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Nothing,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Nothing,False)
  ]

test1R2 :: Assertion
test1R2 = testR sync1R2
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,True))
  , (False,(Just 1,False))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (False,(Just 5,True))
  , (False,(Just 5,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (False,(Nothing,True))
  ]

test1W2 :: Assertion
test1W2 = testW sync1W2
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Nothing,False)
  ]

test2R2 :: Assertion
test2R2 = testR sync2R2
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (False,(Nothing,True))
  , (False,(Just 5,True))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (False,(Nothing,True))
  ]

test2W2 :: Assertion
test2W2 = testW sync2W2
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Nothing,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Nothing,False)
  ]

test1R3 :: Assertion
test1R3 = testR sync1R3
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,True))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (False,(Just 5,True))
  , (False,(Just 5,True))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (False,(Nothing,True))
  ]

test1W3 :: Assertion
test1W3 = testW sync1W3
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Nothing,False)
  ]

test2R3 :: Assertion
test2R3 = testR sync2R3
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 5,True))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (False,(Nothing,True))
  ]

test2W3 :: Assertion
test2W3 = testW sync2W3
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Nothing,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Nothing,False)
  ]

test1R4 :: Assertion
test1R4 = testR sync1R4
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,True))
  , (False,(Just 1,False))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (False,(Just 5,True))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (False,(Nothing,True))
  ]

test1W4 :: Assertion
test1W4 = testW sync1W4
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Nothing,False)
  ]

test2R4 :: Assertion
test2R4 = testR sync2R4
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (False,(Nothing,True))
  , (False,(Just 5,True))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (False,(Just 5,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (False,(Nothing,True))
  ]

test2W4 :: Assertion
test2W4 = testW sync2W4
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Nothing,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Nothing,False)
  ]

test3R5 :: Assertion
test3R5 = testR sync3R5
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,True))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (True,(Just 9,False))
  , (True,(Just 10,False))
  , (True,(Just 11,False))
  , (True,(Just 12,False))
  , (True,(Just 13,False))
  , (True,(Just 14,False))
  , (True,(Just 15,False))
  , (True,(Just 16,False))
  , (False,(Just 1,True))
  ]

test3W5 :: Assertion
test3W5 = testW sync3W5
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Just 9,False)
  , (Just 10,False)
  , (Just 11,False)
  , (Just 12,False)
  , (Just 13,False)
  , (Just 14,False)
  , (Just 15,False)
  , (Just 16,False)
  , (Nothing,True)
  , (Nothing,False)
  ]

test4R5 :: Assertion
test4R5 = testR sync4R5
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (True,(Just 9,False))
  , (True,(Just 10,False))
  , (True,(Just 11,False))
  , (True,(Just 12,False))
  , (True,(Just 13,False))
  , (True,(Just 14,False))
  , (True,(Just 15,False))
  , (True,(Just 16,False))
  , (False,(Just 1,True))
  ]

test4W5 :: Assertion
test4W5 = testW sync4W5
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Just 9,False)
  , (Just 10,False)
  , (Just 11,False)
  , (Just 12,False)
  , (Just 13,False)
  , (Just 14,False)
  , (Just 15,False)
  , (Just 16,False)
  , (Nothing,True)
  , (Nothing,False)
  ]

test3R6 :: Assertion
test3R6 = testR sync3R6
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,True))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (False,(Just 4,True))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (True,(Just 4,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (True,(Just 9,False))
  , (True,(Just 10,False))
  , (True,(Just 11,False))
  , (True,(Just 12,False))
  , (True,(Just 13,False))
  , (True,(Just 14,False))
  , (True,(Just 15,False))
  , (True,(Just 16,False))
  , (True,(Just 17,False))
  , (True,(Just 18,False))
  , (True,(Just 19,False))
  , (False,(Just 4,True))
  ]

test3W6 :: Assertion
test3W6 = testW sync3W6
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Nothing,False)
  , (Just 4,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Just 9,False)
  , (Just 10,False)
  , (Just 11,False)
  , (Just 12,False)
  , (Just 13,False)
  , (Just 14,False)
  , (Just 15,False)
  , (Just 16,False)
  , (Just 17,False)
  , (Just 18,False)
  , (Just 19,False)
  , (Nothing,True)
  , (Nothing,True)
  , (Nothing,False)
  ]

test4R6 :: Assertion
test4R6 = testR sync4R6
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,False))
  , (False,(Just 1,False))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 4,True))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (False,(Just 4,False))
  , (True,(Just 4,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (True,(Just 9,False))
  , (True,(Just 10,False))
  , (True,(Just 11,False))
  , (True,(Just 12,False))
  , (True,(Just 13,False))
  , (True,(Just 14,False))
  , (True,(Just 15,False))
  , (True,(Just 16,False))
  , (True,(Just 17,False))
  , (True,(Just 18,False))
  , (True,(Just 19,False))
  , (False,(Just 4,True))
  ]

test4W6 :: Assertion
test4W6 = testW sync4W6
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Nothing,False)
  , (Just 4,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Just 9,False)
  , (Just 10,False)
  , (Just 11,False)
  , (Just 12,False)
  , (Just 13,False)
  , (Just 14,False)
  , (Just 15,False)
  , (Just 16,False)
  , (Just 17,False)
  , (Just 18,False)
  , (Just 19,False)
  , (Nothing,True)
  , (Nothing,False)
  ]

test5R7 :: Assertion
test5R7 = testR sync5R7
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (False,(Just 1,True))
  , (True,(Just 1,False))
  , (True,(Just 2,False))
  , (True,(Just 3,False))
  , (True,(Just 4,False))
  , (True,(Just 5,False))
  , (True,(Just 6,False))
  , (True,(Just 7,False))
  , (True,(Just 8,False))
  , (True,(Just 9,False))
  , (True,(Just 10,False))
  , (True,(Just 11,False))
  , (True,(Just 12,False))
  , (True,(Just 13,False))
  , (True,(Just 14,False))
  , (True,(Just 15,False))
  , (True,(Just 16,False))
  , (True,(Just 17,False))
  , (False,(Just 2,True))
  ]

test5W7 :: Assertion
test5W7 = testW sync5W7
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Just 9,False)
  , (Just 10,False)
  , (Just 11,False)
  , (Just 12,False)
  , (Just 13,False)
  , (Just 14,False)
  , (Just 15,False)
  , (Just 16,False)
  , (Just 17,False)
  , (Nothing,False)
  ]

test6R7 :: Assertion
test6R7 = testR sync6R7
  [ (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Nothing,True))
  , (False,(Just 1,True))
  , (True,(Just 1,False))
  , (False,(Just 2,True))
  , (True,(Just 2,False))
  , (False,(Just 3,True))
  , (True,(Just 3,False))
  , (False,(Just 4,True))
  , (True,(Just 4,False))
  , (False,(Just 5,True))
  , (True,(Just 5,False))
  , (False,(Just 6,True))
  , (True,(Just 6,False))
  , (False,(Just 7,True))
  , (True,(Just 7,False))
  , (False,(Just 8,True))
  , (True,(Just 8,False))
  , (False,(Just 9,True))
  , (True,(Just 9,False))
  , (False,(Just 10,True))
  , (True,(Just 10,False))
  , (False,(Just 11,True))
  , (True,(Just 11,False))
  , (False,(Just 12,True))
  , (True,(Just 12,False))
  , (False,(Just 13,True))
  , (True,(Just 13,False))
  , (False,(Just 14,True))
  , (True,(Just 14,False))
  , (False,(Just 15,True))
  , (True,(Just 15,False))
  , (False,(Just 16,True))
  , (True,(Just 16,False))
  , (False,(Just 17,True))
  , (True,(Just 17,False))
  , (False,(Just 2,True))
  ]

test6W7 :: Assertion
test6W7 = testW sync6W7
  [ (Nothing,False)
  , (Just 1,False)
  , (Just 2,False)
  , (Just 3,False)
  , (Just 4,False)
  , (Just 5,False)
  , (Just 6,False)
  , (Just 7,False)
  , (Just 8,False)
  , (Just 9,False)
  , (Just 10,False)
  , (Just 11,False)
  , (Just 12,False)
  , (Just 13,False)
  , (Just 14,False)
  , (Just 15,False)
  , (Just 16,False)
  , (Just 17,False)
  , (Nothing,False)
  ]

createDomain vSystem{vName="A", vPeriod=hzToPeriod 20e6} -- fast
createDomain vSystem{vName="B", vPeriod=hzToPeriod 10e6} -- slow
createDomain vSystem{vName="C", vPeriod=hzToPeriod 7e6} -- slower

data DomProxy (dom :: Domain) where
  DomProxy
    :: KnownDomain dom
    => DomProxy dom

-- A more useful Show instance than the one for 'Proxy'
instance Show (DomProxy dom) where
  showsPrec d dom@DomProxy =
    showParen (d > app_prec) $ ("DomProxy @" <>) . (symbolVal dom <>)
   where app_prec = 10

data NamedTest a = NamedTest
  { namedTest :: a
  , testName :: String
  }

instance Show (NamedTest a) where
  show nt = testName nt

testNameShowsPrec
  :: Int
  -> ShowS
  -> DomProxy dom1
  -> DomProxy dom2
  -> ShowS
testNameShowsPrec d baseName dom1 dom2 =
  showParen (d > app_prec) $ baseName . (' ':) . showsPrec 11 dom1 .
    (' ':) . showsPrec 11 dom2
 where app_prec = 10

forAllNamedTestProperties
  :: [NamedTest (PropertyT IO ())]
  -> Property

forAllNamedTestProperties nts = property $ do
  t <- fmap namedTest . forAllWith show . Gen.resize 99 $ Gen.element nts
  t

data WriteAction =
    Write
  | WNoOp
  | WGated
  | GatedWrite
  deriving (Eq, Enum, Bounded, Show, Generic, NFDataX)

data ReadAction =
    Read
  | RNoOp
  | RGated
  | GatedRead
  deriving (Eq, Enum, Bounded, Show, Generic, NFDataX)

fifoOperations
  :: forall wdom rdom
   . (KnownDomain wdom, KnownDomain rdom)
  => [ReadAction]
  -> [WriteAction]
  -> ( Signal rdom ( Bool
                     -- Test fully done
                   , Maybe Int
                     -- Element read from FIFO
                   )
     , Signal wdom ( Bool
                     -- Test fully done
                   , Maybe Int
                    -- Element written to FIFO
                   )
     )
fifoOperations racts wacts = (bundle (rAllDone, rel), bundle (wAllDone, wel))
 where
  (rdata, rempty, wfull) =
    asyncFIFOSynchronizer d3 wclk rclk noWRst noRRst (toEnable wen)
      (toEnable ren) rinc wDataM
  rclk = clockGen @rdom
  wclk = clockGen @wdom
  -- Not resetting makes the test easier to interpret and actual proper testing
  -- of reset behaviour is a lot more involved.
  noRRst = unsafeFromHighPolarity @rdom (pure False)
  noWRst = unsafeFromHighPolarity @wdom (pure False)
  (wdone, wact) =
    unbundle $ fromList $ P.zip (P.repeat False) wacts <> P.repeat (True, WNoOp)
  (rdone, ract) =
    unbundle $ fromList $ P.zip (P.repeat False) racts <> P.repeat (True, RNoOp)
  (wen, wDataM) = unbundle $
    liftA2 (\wact0 d -> case wact0 of
                          Write      -> (True , Just d )
                          WNoOp      -> (True , Nothing)
                          WGated     -> (False, Nothing)
                          GatedWrite -> (False, Just d )
           ) wact wdata
  wdata = regEn wclk noWRst enableGen 1 (isJust <$> wDataM) (wdata + 1)
  (ren, rinc) = unbundle $
    fmap (\ract0 -> case ract0 of
                      Read       -> (True , True )
                      RNoOp      -> (True , False)
                      RGated     -> (False, False)
                      GatedRead  -> (False, True)
         ) ract1
  mainDone = rdone .&&. unsafeSynchronizer wclk rclk wdone
  -- Empty FIFO after main test
  ract1 = mux mainDone (pure Read) ract
  flushCnt = regEn rclk noRRst enableGen (1 :: Int) mainDone (flushCnt + 1)
  -- Surely in 20 cycles we have seen all of the FIFO even if something has gone
  -- wrong with the pointers
  rAllDone = fmap (> 20) flushCnt
  wAllDone = unsafeSynchronizer rclk wclk rAllDone
  rel = mux (liftA3 (\en inc em -> en && inc && not em) ren rinc rempty)
            (Just <$> rdata) (pure Nothing)
  wel = mux (liftA3 (\en wdm fu -> en && isJust wdm && not fu) wen wDataM wfull)
            (Just <$> wdata) (pure Nothing)

fifoFunctionalTest
  :: forall wdom rdom m
   . Monad m
  => DomProxy wdom
  -> DomProxy rdom
  -> NamedTest (PropertyT m ())
fifoFunctionalTest wdom@DomProxy rdom@DomProxy =
  NamedTest { namedTest=test0, testName = name }
 where
  name = testNameShowsPrec 0 (shows 'fifoFunctionalTest) wdom rdom ""
  genInput :: (Bounded e, Enum e, Show e) => PropertyT m [e]
  genInput = fmap P.concat . forAll . Gen.list (Range.linear 1 10) .
               Gen.list (Range.linear 0 20) $ Gen.enumBounded
  test0 = do
    racts <- genInput
    wacts <- genInput
    let
      results = fifoOperations @wdom @rdom racts wacts
      catValids = catMaybes . P.map snd . P.takeWhile (not . fst)
      rels = catValids . sample $ fst results
      wels = catValids . sample $ snd results
    rels === wels

fifoFunctionalTestCombinations :: Monad m => [NamedTest (PropertyT m ())]
fifoFunctionalTestCombinations =
  [ fifoFunctionalTest (DomProxy @A) (DomProxy @A)
  , fifoFunctionalTest (DomProxy @A) (DomProxy @B)
  , fifoFunctionalTest (DomProxy @A) (DomProxy @C)
  , fifoFunctionalTest (DomProxy @B) (DomProxy @A)
  , fifoFunctionalTest (DomProxy @B) (DomProxy @C)
  , fifoFunctionalTest (DomProxy @C) (DomProxy @A)
  , fifoFunctionalTest (DomProxy @C) (DomProxy @B)
  ]

tests :: TestTree
tests = testGroup "asyncFIFOSynchronizer"
  [ testCase "Test 1.1 Read" test1R1
  , testCase "Test 1.1 Write" test1W1
  , testCase "Test 2.1 Read" test2R1
  , testCase "Test 2.1 Write" test2W1
  , testCase "Test 1.2 Read" test1R2
  , testCase "Test 1.2 Write" test1W2
  , testCase "Test 2.2 Read" test2R2
  , testCase "Test 2.2 Write" test2W2
  , testCase "Test 1.3 Read" test1R3
  , testCase "Test 1.3 Write" test1W3
  , testCase "Test 2.3 Read" test2R3
  , testCase "Test 2.3 Write" test2W3
  , testCase "Test 1.4 Read" test1R4
  , testCase "Test 1.4 Write" test1W4
  , testCase "Test 2.4 Read" test2R4
  , testCase "Test 2.4 Write" test2W4
  , testCase "Test 3.5 Read" test3R5
  , testCase "Test 3.5 Write" test3W5
  , testCase "Test 4.5 Read" test4R5
  , testCase "Test 4.5 Write" test4W5
  , testCase "Test 3.6 Read" test3R6
  , testCase "Test 3.6 Write" test3W6
  , testCase "Test 4.6 Read" test4R6
  , testCase "Test 4.6 Write" test4W6
  , testCase "Test 5.7 Read" test5R7
  , testCase "Test 5.7 Write" test5W7
  , testCase "Test 6.7 Read" test6R7
  , testCase "Test 6.7 Write" test6W7
  , testPropertyXXX "Functional test" $ forAllNamedTestProperties fifoFunctionalTestCombinations
  ]
