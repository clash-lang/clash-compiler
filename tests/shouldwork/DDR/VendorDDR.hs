{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module VendorDDR where

import Clash.Explicit.DDR
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import qualified Prelude as P

createDomain vSystem{vName="DDRA", vPeriod=5000}
createDomain vXilinxSystem{vName="DDRS", vPeriod=5000}
createDomain vXilinxSystem{vName="Dom4", vPeriod=2500}

type C = Unsigned 11
type D = Unsigned 9

type TestLen = 44
type TestLen2 = 2 * TestLen - 1

type DomCxt slow fast fPeriod reset =
  ( KnownConfiguration fast ('DomainConfiguration fast fPeriod 'Rising reset 'Defined 'ActiveHigh)
  , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) 'Rising reset 'Defined 'ActiveHigh)
  )

type VendorIn slow fast =
  Clock slow ->
  Reset slow ->
  Enable slow ->
  Signal fast D ->
  Signal slow (D, D)

type VendorOut slow fast =
  Clock slow ->
  Reset slow ->
  Enable slow ->
  Signal slow (D, D) ->
  Signal fast D

type TopEntityEna slow fast =
  Clock slow ->
  Reset slow ->
  Enable slow ->
  Signal fast D ->
  Signal slow (D, D) ->
  ( Signal slow ((D, D, D, D))
  , Signal fast (D, D)
  )

type TopEntityNoEna slow fast =
  Clock slow ->
  Reset slow ->
  Signal fast D ->
  Signal slow (D, D) ->
  ( Signal slow ((D, D, D, D))
  , Signal fast (D, D)
  )

topEntityAnn ::
  Bool ->
  String ->
  TopEntity
topEntityAnn withEna nm =
  Synthesize
    { t_name = nm
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        ] P.++
        (if withEna then [PortName "en"] else []) P.++
        [ PortName "in_in"
        , PortProduct "in_out"
            [ PortName "1"
            , PortName "2"
            ]
        ]
    , t_output =
        PortProduct "out"
          [ PortProduct "in"
              [ PortName "gen1"
              , PortName "gen2"
              , PortName "vendor1"
              , PortName "vendor2"
              ]
          , PortProduct "out"
              [ PortName "gen"
              , PortName "vendor"
              ]
          ]
    }

topEntityEnaAnn, topEntityNoEnaAnn :: String -> TopEntity
topEntityEnaAnn = topEntityAnn True
topEntityNoEnaAnn = topEntityAnn False

type TestBenchT slow fast =
  ( "clkSlow" ::: Clock slow
  , "clkFast" ::: Clock fast
  , "en" ::: Enable slow
  , "in_in" ::: Signal fast D
  , "out_in" ::: Signal slow
      ( "1" ::: D
      , "2" ::: D
      )
  , "in_out" ::: Signal slow
      ( "gen1" ::: D
      , "gen2" ::: D
      , "vendor1" ::: D
      , "vendor2" ::: D
      )
  , "out_out" ::: Signal fast
      ( "gen" ::: D
      , "vendor" ::: D
      )
  , "done" ::: Signal fast Bool
  )

topEntityGeneric ::
  DomCxt slow fast fPeriod reset =>
  VendorIn slow fast ->
  VendorOut slow fast ->
  TopEntityEna slow fast
topEntityGeneric vendorIn vendorOut clk rst en inIn outIn =
  ( bundle (genInOut1, genInOut2, vendorInOut1, vendorInOut2)
  , bundle (genOutOut, vendorOutOut)
  )
 where
  (genInOut1, genInOut2) = unbundle $ ddrIn clk rst en (0, 0, 0) inIn
  (vendorInOut1, vendorInOut2) = unbundle $ vendorIn clk rst en inIn
  genOutOut = ddrOut clk rst en 0 outIn
  vendorOutOut = vendorOut clk rst en outIn

noEnable ::
  TopEntityNoEna slow fast ->
  TopEntityEna slow fast
noEnable top = top0
 where
  top0 clk rst _en = top clk rst

expOut :: [D]
expOut = 0 : P.concatMap (\e -> [e + 100, e + 200]) [0 .. natToNum @TestLen - 2]

genOutSyncReset :: [D] -> [D]
genOutSyncReset es =
  let
    es0 = P.take 51 es P.++ P.replicate 6 0 P.++ P.drop 57 es
  in P.take 13 es0 P.++ P.replicate 6 0 P.++ P.drop 19 es0


xilinxOutSyncReset :: [D] -> [D]
xilinxOutSyncReset es =
  let
    es0 = P.take 51 es P.++ P.replicate 6 0 P.++ P.drop 57 es
  in P.take 12 es0 P.++ P.replicate 7 0 P.++ P.drop 19 es0

outAsyncReset :: [D] -> [D]
outAsyncReset es =
  let
    es0 = P.take 50 es P.++ P.replicate 7 0 P.++ P.drop 57 es
  in P.take 11 es0 P.++ P.replicate 8 0 P.++ P.drop 19 es0

genOutEnable :: [D] -> [D]
genOutEnable es =
  let
    es0 = P.take 69 es P.++ P.take 8 (cycle [133, 233]) P.++ P.drop 77 es
  in P.take 31 es0 P.++ P.take 8 (cycle [114, 214]) P.++ P.drop 39 es0

xilinxOutEnable :: [D] -> [D]
xilinxOutEnable es =
  let es0 = P.take 68 es P.++ P.replicate 9 233 P.++ P.drop 77 es
  in P.take 30 es0 P.++ P.replicate 9 114 P.++ P.drop 39 es0

expIn :: [(D, D)]
expIn = (0,0) : (0,0) : P.zip [1, 3 .. hi] [2, 4 .. hi]
 where
  hi = 2 * (natToNum @TestLen - 2)

inSyncReset :: [(D, D)] -> [(D, D)]
inSyncReset es =
  let
    es0 = P.take 26 es P.++ P.replicate 3 (0,0) P.++ [(0, 56)] P.++ P.drop 30 es
  in P.take 7 es0 P.++ P.replicate 3 (0, 0) P.++ P.drop 10 es0

inAsyncReset :: [(D, D)] -> [(D, D)]
inAsyncReset es =
  let
    es0 = P.take 25 es P.++ P.replicate 4 (0,0) P.++ [(0,56)] P.++ P.drop 30 es
  in P.take 6 es0 P.++ P.replicate 4 (0, 0) P.++ P.drop 10 es0

inEnable ::[(D, D)] ->  [(D, D)]
inEnable es =
  let
    es0 =
      P.take 35 es P.++ P.replicate 4 (65,66) P.++ [(67,76)] P.++ P.drop 40 es
  in P.take 16 es0 P.++ P.replicate 4 (27, 28) P.++ P.drop 20 es0

-- I would have liked to output @rstSlow@, but I hit a Clash bug.
testBenchGeneric ::
  forall slow fast .
  ( KnownDomain slow
  , KnownDomain fast
  ) =>
  TopEntityEna slow fast ->
  Vec TestLen (D, D) ->
  Vec TestLen2 D ->
  Vec TestLen2 D ->
  TestBenchT slow fast
testBenchGeneric top expIn0 expGenOut expVendorOut =
  ( clkSlow
  , clkFast
  , enSlow
  , inIn
  , outIn
  , inOut
  , outOut
  , done
  )
 where
  (inOut, outOut) = top clkSlow rstSlow enSlow inIn outIn
  (clkSlow, clkFast, rstSlow, enSlow, inIn, outIn, done) =
    testBenchGeneric0 expIn0 expGenOut expVendorOut inOut outOut
{-# INLINE testBenchGeneric #-}

testBenchGeneric0 ::
  forall slow fast .
  ( KnownDomain slow
  , KnownDomain fast
  ) =>
  Vec TestLen (D, D) ->
  Vec TestLen2 D ->
  Vec TestLen2 D ->
  "in_out" ::: Signal slow
    ( "gen1" ::: D
    , "gen2" ::: D
    , "vendor1" ::: D
    , "vendor2" ::: D
    ) ->
  "out_out" ::: Signal fast
    ( "gen" ::: D
    , "vendor" ::: D
    ) ->
  ( "clkSlow" ::: Clock slow
  , "clkFast" ::: Clock fast
  , "rstSlow" ::: Reset slow
  , "en" ::: Enable slow
  , "in_in" ::: Signal fast D
  , "out_in" ::: Signal slow
      ( "1" ::: D
      , "2" ::: D
      )
  , "done" ::: Signal fast Bool
  )
testBenchGeneric0 expIn0 expGenOut expVendorOut inOut outOut =
  (clkSlow, clkFast, rstSlow, enSlow, inIn, outIn, done)
 where
  (genInOut1, genInOut2, vendorInOut1, vendorInOut2) = unbundle inOut
  (genOutOut, vendorOutOut) = unbundle outOut
  inIn = cToDFast cntr4
  outIn = bundle (posIn, negIn)
  posIn = cToDSlow $ cntr4 + 402
  negIn = cToDSlow $ cntr4 + 802
  cToDFast = unsafeSynchronizer clk4 clkFast . fmap (truncateB . (`shiftR` 1))
  cToDSlow = unsafeSynchronizer clk4 clkSlow . fmap (truncateB . (`shiftR` 2))
  cntr4 :: Signal Dom4 C
  cntr4 = register clk4 noReset enableGen 0 $ cntr4 + 1
#ifndef INTEL_VERILOG
  done1 = outputVerifierWith (\clk rst -> assert clk rst "genOutOut")
    clkFast clkFast noReset expGenOut $
      ignoreFor clkFast noReset enableGen d1 0 genOutOut
#else
  -- The test contains a number of (implicit) parallel, coinciding processes.
  -- Execution for these processes is left undefined in the Verilog spec, nor is
  -- there a mechanism to get consistent behavior (like in VHDL). Therefore,
  -- simulators are free to pick any execution order they'd like. With Verilog
  -- HDL, ModelSim executes the processes in an order that makes this particular
  -- `outputVerifier` fail. SystemVerilog however is unaffected, as of course is
  -- VHDL.
  --
  -- CI never runs @IntelDDR.hs@ (which imports this module), but to enable
  -- manual verification with @IntelDDR.hs@, you can define the macro
  -- INTEL_VERILOG when compiling. That way, you can disable this
  -- `outputVerifier` so you can still verify the vendor primitives, which is
  -- the main purpose of the test.
  --
  -- Note that @IntelDDR.hs@ still suffers from the issue described in
  -- https://github.com/clash-lang/clash-compiler/issues/2854
  --
  -- The `const` is just there to prevent a @-Wunused-matches@ on @expGenOut@
  -- and @genOutOut@.
  done1 = pure $ const True (expGenOut, genOutOut)
#endif
  done2 = outputVerifierWith (\clk rst -> assert clk rst "vendorOutOut")
    clkFast clkFast noReset expVendorOut vendorOutOut
  done3 = outputVerifierWith (\clk rst -> assert clk rst "genInOut1")
    clkSlow clkSlow noReset (map fst expIn0) $
      ignoreFor clkSlow noReset enableGen d2 0 genInOut1
  done4 = outputVerifierWith (\clk rst -> assert clk rst "genInOut2")
    clkSlow clkSlow noReset (map snd expIn0) $
      ignoreFor clkSlow noReset enableGen d1 0 genInOut2
  done5 = outputVerifierWith (\clk rst -> assert clk rst "vendorInOut1")
    clkSlow clkSlow noReset (map fst expIn0) $
      ignoreFor clkSlow noReset enableGen d2 0 vendorInOut1
  done6 = outputVerifierWith (\clk rst -> assert clk rst "vendorInOut2")
    clkSlow clkSlow noReset (map snd expIn0) vendorInOut2
  doneFast = done1 `strictAnd` done2
  doneSlow = done3 `strictAnd` done4 `strictAnd` done5 `strictAnd` done6
  done = doneFast `strictAnd` unsafeSynchronizer clkSlow clkFast doneSlow
  notDone = not <$> done
  clk4 = tbClockGen $ unsafeSynchronizer clkFast clk4 notDone
  -- {-# NOINLINE clk4 #-}
  clkFast = tbClockGen notDone
  -- {-# NOINLINE clkFast #-}
  clkSlow = tbClockGen $ unsafeSynchronizer clkFast clkSlow notDone
  -- {-# NOINLINE clkSlow #-}
  rstSlow =
    unsafeFromActiveHigh $ unsafeSynchronizer clk4 clkSlow $
      (\n -> (n >= 22 && n <= 33) || (n >= 100 && n <= 111)) <$> cntr4
  enSlow =
    toEnable $ unsafeSynchronizer clk4 clkSlow $
      (\n -> (n < 58 || n > 73) && ( n < 136 || n > 151)) <$> cntr4
{-# NOINLINE testBenchGeneric0 #-}

getDone ::
  forall slow fast .
  TestBenchT slow fast ->
  Signal fast Bool
getDone tb = done
 where
  (_, _, _, _, _, _, _, done) = tb

strictAnd :: Applicative f => f Bool -> f Bool -> f Bool
strictAnd = liftA2 (let f !a !b = a && b in f)
