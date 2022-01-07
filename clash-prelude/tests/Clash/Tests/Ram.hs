-- Assert correct behavior:
--
-- Undefined enable:
--    The written-to address should read 'undefined', but other addresses
--    should still have their data.
--
-- Undefined write address:
--    All addresses should read 'undefined'.
--
-- Undefined write data:
--    The written-to address should read 'undefined', but other addresses
--    should still have their data.
--
-- Deasserted enable
--    It shouldn't matter that other write inputs are 'undefined'.
--
-- Deasserted enable, OOB address
--    It shouldn't matter that it is out of bounds.
--
-- Read address strictness
--    If the read result is not used, out-of-bounds read address shouldn't
--    matter (equivalent to issue #1458).

{-# LANGUAGE NoImplicitPrelude #-}

module Clash.Tests.Ram (tests) where

import qualified Data.List as L
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Explicit.Prelude
import Clash.Explicit.RAM

type Ram = (   Signal System Int
            -> Signal System Bool
            -> Signal System Int
            -> Signal System Int
            -> Signal System (Maybe Int)
           )

ram :: Ram
ram rd we wr din =
  maybeIsX <$> asyncRam# clockGen clockGen enableGen d2 rd we wr din

maskOobRead :: Ram
maskOobRead rd we wr din =
  maybeIsX <$> mux (rd .<. 2) ram0 (pure 4)
 where
  ram0 = asyncRam# clockGen clockGen enableGen d2 rd we wr din

type Samples = [(Int, Bool, Int, Int, Maybe Int)]

initMem, undefEn, undefWAddr, undefWData, enFalse, enFalseOobWAddr,
  oobRAddrStrict
  :: Samples

--                               rd  enable     waddr      wdata      dout
initMem =                     [ ( 0, True     , 0        , 0        , Nothing)
                              , ( 0, True     , 1        , 1        , Just 0 )
                              ]

undefEn = initMem <>          [ ( 0, undefined, 0        , 2        , Just 0 )
                              , ( 0, False    , 0        , 3        , Nothing)
                              , ( 1, False    , 0        , 3        , Just 1 )
                              ]

undefWAddr = initMem <>       [ ( 0, True     , undefined, 2        , Just 0 )
                              , ( 0, False    , 0        , 3        , Nothing)
                              , ( 1, False    , 0        , 3        , Nothing)
                              ]

undefWData = initMem <>       [ ( 0, True     , 0        , undefined, Just 0 )
                              , ( 0, False    , 0        , 3        , Nothing)
                              , ( 1, False    , 0        , 3        , Just 1 )
                              ]

enFalse = initMem <>          [ ( 0, False    , undefined, undefined, Just 0)
                              , ( 0, False    , undefined, undefined, Just 0)
                              , ( 1, False    , undefined, undefined, Just 1)
                              ]

enFalseOobWAddr = initMem <>  [ ( 0, False    , 255      , 2        , Just 0 )
                              , ( 0, False    , 0        , 3        , Just 0 )
                              , ( 1, False    , 0        , 3        , Just 1 )
                              ]

oobRAddrStrict = initMem <>   [ ( 1, False    , 0        , 3        , Just 1 )
                              , ( 2, False    , 0        , 3        , Just 4 )
                              , ( 0, False    , 0        , 3        , Just 0 )
                              ]

ramAssertion
  :: Ram
  -> Samples
  -> Assertion
ramAssertion ram0 samples = actual @?= expectedOutput
 where
  (rd, we, wr, din, expectedOutput) = L.unzip5 samples
  actual = sampleN (L.length samples) $ ram0 (fromList rd)
                                             (fromList we)
                                             (fromList wr)
                                             (fromList din)
tests :: TestTree
tests = testGroup "Ram"
  [ testCase "Undefined enable" $ ramAssertion ram undefEn
  , testCase "Undefined write address" $ ramAssertion ram undefWAddr
  , testCase "Undefined write data" $ ramAssertion ram undefWData
  , testCase "Deasserted enable" $ ramAssertion ram enFalse
  , testCase "Deasserted enable, OOB address" $ ramAssertion ram enFalseOobWAddr
  , testCase "Read address strictness" $ ramAssertion maskOobRead oobRAddrStrict
  ]
