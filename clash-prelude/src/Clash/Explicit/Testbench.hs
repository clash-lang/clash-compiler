{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017-2022, Google Inc.
                  2019     , Myrtle Software Ltd,
                  2021-2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Explicit.Testbench
  ( -- * Testbench functions for circuits
    assert
  , assertBitVector
  , ignoreFor
  , stimuliGenerator

  , tbClockGen
  , tbEnableGen
  , tbSystemClockGen
  , seClockToDiffClock

  , outputVerifier
  , outputVerifier'
  , outputVerifierBitVector
  , outputVerifierBitVector'
  , biTbClockGen
  , unsafeSimSynchronizer
  , outputVerifierWith
  )
where

import Control.Exception     (catch, evaluate)
import Debug.Trace           (trace)
import GHC.TypeLits          (KnownNat, type (+), type (<=))
import Prelude               hiding ((!!), length)
import System.IO.Unsafe      (unsafeDupablePerformIO)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Class.Num       (satSucc, SaturationMode(SatBound))
import Clash.Promoted.Nat    (SNat(..))
import Clash.Promoted.Symbol (SSymbol(..))
import Clash.Explicit.Signal
  (System, Signal, toEnable, fromList, register,
  unbundle, unsafeSynchronizer)
import Clash.Signal.Internal
  (Clock (..), ClockN (..), DiffClock (..), Reset (..), tbClockGen)
import Clash.Signal          (mux, KnownDomain, Enable)
import Clash.Sized.Index     (Index)
import Clash.Sized.Internal.BitVector
  (BitVector, isLike#)
import Clash.Sized.Vector    (Vec, (!!), length)
import Clash.XException      (ShowX (..), XException)

import Clash.Annotations.Primitive(Primitive (InlineYamlPrimitive), HDL(..))
import Data.List.Infinite (Infinite((:<)), (...))
import Data.String.Interpolate (__i)


-- Note that outputVerifier' is used in $setup, while the examples mention
-- outputVerifier. This is fine, as the examples have explicit type
-- signatures, turning 'outputVerifier' into 'outputVerifier''.

{- $setup
>>> :set -XTemplateHaskell -XDataKinds -XTypeFamilies
>>> import Clash.Explicit.Prelude
>>> let testInput clk rst = stimuliGenerator clk rst $(listToVecTH [(1::Int),3..21])
>>> let expectedOutput clk rst = outputVerifier' clk rst $(listToVecTH ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-}

-- | Compares the first two 'Signal's for equality and logs a warning when they
-- are not equal. The second 'Signal' is considered the expected value. This
-- function simply returns the third 'Signal' unaltered as its result. This
-- function is used by 'outputVerifier'.
--
-- === Usage in @clashi@ #assert-clashi#
--
-- __NB__: When simulating a component that uses 'assert' in @clashi@, usually,
-- the warnings are only logged the first time the component is simulated.
-- Issuing @:reload@ in @clashi@ will discard the cached result of the
-- computation, and warnings will once again be emitted.
--
-- __NB__: This function /can/ be used in synthesizable designs.
assert
  :: (Eq a, ShowX a)
  => Clock dom
  -> Reset dom
  -> String
  -- ^ Additional message
  -> Signal dom a
  -- ^ Checked value
  -> Signal dom a
  -- ^ Expected value
  -> Signal dom b
  -- ^ Return value
  -> Signal dom b
assert clk (Reset _) msg checked expected returned =
  (\c e cnt r ->
      if eqX c e
         then r
         else trace (concat [ "\ncycle(" ++ show clk ++ "): "
                            , show cnt
                            , ", "
                            , msg
                            , "\nexpected value: "
                            , showX e
                            , ", not equal to actual value: "
                            , showX c
                            ]) r)
  <$> checked <*> expected <*> fromList [(0::Integer)..] <*> returned
  where
    eqX a b = unsafeDupablePerformIO (catch (evaluate (a == b))
                                            (\(_ :: XException) -> return False))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE assert #-}
{-# ANN assert hasBlackBox #-}
{-# ANN assert (
  let
    bbName = show 'assert
    _arg1 :< _arg2 :< arg3 :< _arg4 :< arg5 :< arg6 :< arg7 :< arg8 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [SystemVerilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          assert
            :: (Eq a, ShowX a)      -- (ARG[1], ARG[2])
            => Clock dom                             -- ARG[3]
            -> Reset dom                             -- ARG[4]
            -> String                                -- ARG[5]
            -> Signal dom a                          -- Checked value  (ARG[6])
            -> Signal dom a                          -- Expected value (ARG[7])
            -> Signal dom b                          -- Return valued  (ARG[8])
            -> Signal dom b
        template: |-
          // assert begin
          // pragma translate_off
          always @(~IF ~ACTIVEEDGE[Rising][#{arg3}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg3}]) begin
            if (~ARG[#{arg6}] !== ~ARG[#{arg7}]) begin
              $display("@%0tns: %s, expected: %b, actual: %b", $time, ~LIT[#{arg5}], ~TOBV[~ARG[#{arg7}]][~TYP[#{arg7}]], ~TOBV[~ARG[#{arg6}]][~TYP[#{arg6}]]);
              $stop;
            end
          end
          // pragma translate_on
          assign ~RESULT = ~ARG[#{arg8}];
          // assert end
    |]) #-}
{-# ANN assert (
  let
    bbName = show 'assert
    _arg1 :< _arg2 :< arg3 :< _arg4 :< arg5 :< arg6 :< arg7 :< arg8 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [Verilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          assert
            :: (Eq a, ShowX a)      -- (ARG[1], ARG[2])
            => Clock dom                             -- ARG[3]
            -> Reset dom                             -- ARG[4]
            -> String                                -- ARG[5]
            -> Signal dom a                          -- Checked value  (ARG[6])
            -> Signal dom a                          -- Expected value (ARG[7])
            -> Signal dom b                          -- Return valued  (ARG[8])
            -> Signal dom b
        template: |-
          // assert begin
          // pragma translate_off
          always @(~IF ~ACTIVEEDGE[Rising][#{arg3}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg3}]) begin
            if (~ARG[#{arg6}] !== ~ARG[#{arg7}]) begin
              $display("@%0tns: %s, expected: %b, actual: %b", $time, ~LIT[#{arg5}], ~ARG[#{arg7}], ~ARG[#{arg6}]);
              $finish;
            end
          end
          // pragma translate_on
          assign ~RESULT = ~ARG[#{arg8}];
          // assert end
    |]) #-}
{-# ANN assert (
  let
    bbName = show 'assert
    _arg1 :< _arg2 :< arg3 :< _arg4 :< arg5 :< arg6 :< arg7 :< arg8 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        name: '#{bbName}'
        imports:
        - ~INCLUDENAME[0].all
        includes:
        - name: slv2string
          extension: vhdl
          template: |-
            -- helper function of Clash.Explicit.Testbench.assert
            library IEEE;
            use IEEE.STD_LOGIC_1164.ALL;

            package ~INCLUDENAME[0] is
              function slv2string (slv : std_logic_vector) return STRING;
            end;

            package body ~INCLUDENAME[0] is
              function slv2string (slv : std_logic_vector) return STRING is
                 variable result : string (1 to slv'length);
                 variable res_l : string (1 to 3);
                 variable r : integer;
               begin
                 r := 1;
                 for i in slv'range loop
                    res_l := std_logic'image(slv(i));
                    result(r) := res_l(2);
                    r := r + 1;
                 end loop;
                 return result;
              end slv2string;
            end;
        kind: Declaration
        type: |-
          assert
            :: (Eq a, ShowX a)      -- (ARG[1],ARG[2])
            => Clock dom                             -- ARG[3]
            -> Reset dom                             -- ARG[4]
            -> String                                -- ARG[5]
            -> Signal dom a                          -- Checked value  (ARG[6])
            -> Signal dom a                          -- Expected value (ARG[7])
            -> Signal dom b                          -- Return valued  (ARG[8])
            -> Signal dom b
        template: |-
          -- assert begin
          ~GENSYM[assert][0] : block
            -- pragma translate_off
            signal ~GENSYM[actual][2] : ~TYP[#{arg6}];
            signal ~GENSYM[expected][3] : ~TYP[#{arg7}];
            -- pragma translate_on
          begin
            -- pragma translate_off
            ~SYM[2] <= ~ARG[#{arg6}];
            ~SYM[3] <= ~ARG[#{arg7}];
            process(~ARG[#{arg3}]) is
            begin
              if (~IF ~ACTIVEEDGE[Rising][#{arg3}] ~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{arg3}])) then
                assert (toSLV(~SYM[2]) = toSLV(~SYM[3])) report (~LIT[#{arg5}] & ", expected: " & ~INCLUDENAME[0].slv2string(toSLV(~SYM[3])) & ", actual: " & ~INCLUDENAME[0].slv2string(toSLV(~SYM[2]))) severity error;
              end if;
            end process;
            -- pragma translate_on
            ~RESULT <= ~ARG[#{arg8}];
          end block;
          -- assert end
    |]) #-}

-- | The same as 'assert', but can handle don't care bits in its expected value.
assertBitVector
  :: KnownNat n
  => Clock dom
  -> Reset dom
  -> String
  -- ^ Additional message
  -> Signal dom (BitVector n)
  -- ^ Checked value
  -> Signal dom (BitVector n)
  -- ^ Expected value
  -> Signal dom b
  -- ^ Return value
  -> Signal dom b
assertBitVector clk (Reset _) msg checked expected returned =
  (\c e cnt r ->
      if eqX c e
         then r
         else trace (concat [ "\ncycle(" ++ show clk ++ "): "
                            , show cnt
                            , ", "
                            , msg
                            , "\nexpected value: "
                            , showX e
                            , ", not equal to actual value: "
                            , showX c
                            ]) r)
  <$> checked <*> expected <*> fromList [(0::Integer)..] <*> returned
  where
    eqX a b = unsafeDupablePerformIO (catch (evaluate (a `isLike#` b))
                                            (\(_ :: XException) -> return False))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE assertBitVector #-}
{-# ANN assertBitVector hasBlackBox #-}
{-# ANN assertBitVector (
  let
    bbName = show 'assertBitVector
    _arg1 :< arg2 :< _arg3 :< arg4 :< arg5 :< arg6 :< arg7 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [SystemVerilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          assertBitVector
            :: ( KnownNat n )           --                 ARG[1]
            => Clock dom                --                 ARG[2]
            -> Reset dom                --                 ARG[3]
            -> String                   --                 ARG[4]
            -> Signal dom (BitVector n) -- Checked value  (ARG[5])
            -> Signal dom (BitVector n) -- Expected value (ARG[6])
            -> Signal dom b             -- Return valued  (ARG[7])
            -> Signal dom b
        template: |-
          // assertBitVector begin
          // pragma translate_off
          wire ~TYP[#{arg6}] ~GENSYM[maskXor][0]  = ~ARG[#{arg6}] ^ ~ARG[#{arg6}];
          wire ~TYP[#{arg6}] ~GENSYM[checked][1]  = ~ARG[#{arg5}] ^ ~SYM[0];
          wire ~TYP[#{arg6}] ~GENSYM[expected][2] = ~ARG[#{arg6}] ^ ~SYM[0];

          always @(~IF ~ACTIVEEDGE[Rising][#{arg2}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg2}]) begin
            if (~SYM[1] !== ~SYM[2]) begin
              $display("@%0tns: %s, expected: %b, actual: %b", $time, ~LIT[#{arg4}], ~TOBV[~ARG[#{arg6}]][~TYP[#{arg6}]], ~TOBV[~ARG[#{arg5}]][~TYP[#{arg5}]]);
              $stop;
            end
          end
          // pragma translate_on
          assign ~RESULT = ~ARG[#{arg7}];
          // assertBitVector end
    |]) #-}
{-# ANN assertBitVector (
  let
    bbName = show 'assertBitVector
    _arg1 :< arg2 :< _arg3 :< arg4 :< arg5 :< arg6 :< arg7 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [Verilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          assertBitVector
            :: ( KnownNat n             --                 ARG[1]
            => Clock dom                --                 ARG[2]
            -> Reset dom                --                 ARG[3]
            -> String                   --                 ARG[4]
            -> Signal dom (BitVector n) -- Checked value  (ARG[5])
            -> Signal dom (BitVector n) -- Expected value (ARG[6])
            -> Signal dom b             -- Return valued  (ARG[7])
            -> Signal dom b
        template: |-
          // assertBitVector begin
          // pragma translate_off
          wire ~TYP[#{arg5}] ~GENSYM[maskXor][0]  = ~ARG[#{arg6}] ^ ~ARG[#{arg6}];
          wire ~TYP[#{arg5}] ~GENSYM[checked][1]  = ~ARG[#{arg5}] ^ ~SYM[0];
          wire ~TYP[#{arg5}] ~GENSYM[expected][2] = ~ARG[#{arg6}] ^ ~SYM[0];

          always @(~IF ~ACTIVEEDGE[Rising][#{arg2}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg2}]) begin
            if (~SYM[1] !== ~SYM[2]) begin
              $display("@%0tns: %s, expected: %b, actual: %b", $time, ~LIT[#{arg4}], ~ARG[#{arg6}], ~ARG[#{arg5}]);
              $finish;
            end
          end
          // pragma translate_on
          assign ~RESULT = ~ARG[#{arg7}];
          // assertBitVector end
    |]) #-}
{-# ANN assertBitVector (
  let
    bbName = show 'assertBitVector
    _arg1 :< arg2 :< _arg3 :< arg4 :< arg5 :< arg6 :< arg7 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        name: '#{bbName}'
        imports:
        - ~INCLUDENAME[0].all
        includes:
        - name: assertBitVector
          extension: vhdl
          template: |
            -- helper functions of Clash.Explicit.Testbench.assertBitVector
            library IEEE;
            use IEEE.STD_LOGIC_1164.ALL;

            package ~INCLUDENAME[0] is
              function non_std_match (l, r : std_logic_vector) return boolean;
              function slv2string (slv : std_logic_vector) return STRING;
            end;

            package body ~INCLUDENAME[0] is
              type match_table_type is array (std_ulogic, std_ulogic) of boolean;
              constant match_table: match_table_type :=
                ('0' | 'L' => ('0' | 'L' | '-' => true, others => false),
                 '1' | 'H' => ('1' | 'H' | '-' => true, others => false),
                 '-' => ('-' => true, others => false),
                 others    =>             ('-' => true, others => false)
                );
              -- non_std_match is like std_match
              -- But only accepts '-' as don't care in its the second argument r.
              function non_std_match (l, r : std_logic_vector) return boolean is
                alias la : std_logic_vector (l'length downto 1) is l;
                alias ra : std_logic_vector (r'length downto 1) is r;
              begin
                for i in l'range loop
                   if not match_table (l (i), r (i)) then
                     return false;
                   end if;
                end loop;
                return true;
              end non_std_match;

              function slv2string (slv : std_logic_vector) return STRING is
                 variable result : string (1 to slv'length);
                 variable res_l : string (1 to 3);
                 variable r : integer;
               begin
                 r := 1;
                 for i in slv'range loop
                    res_l := std_logic'image(slv(i));
                    result(r) := res_l(2);
                    r := r + 1;
                 end loop;
                 return result;
              end slv2string;

            end;
        kind: Declaration
        type: |-
          assertBitVector
            :: ( KnownNat n )           --                 ARG[1]
            => Clock dom                --                 ARG[2]
            -> Reset dom                --                 ARG[3]
            -> String                   --                 ARG[4]
            -> Signal dom (BitVector n) -- Checked value  (ARG[5])
            -> Signal dom (BitVector n) -- Expected value (ARG[6])
            -> Signal dom b             -- Return valued  (ARG[7])
            -> Signal dom b
        template: |-
          -- assertBitVector begin
          ~GENSYM[assert][0] : block
            -- pragma translate_off
            signal ~GENSYM[actual][2] : ~TYP[#{arg5}];
            signal ~GENSYM[expected][3] : ~TYP[#{arg6}];
            -- pragma translate_on
          begin
            -- pragma translate_off
            ~SYM[2] <= ~ARG[#{arg5}];
            ~SYM[3] <= ~ARG[#{arg6}];
            process(~ARG[#{arg2}]) is
            begin
              if (~IF ~ACTIVEEDGE[Rising][#{arg2}] ~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{arg2}])) then
                assert (~INCLUDENAME[0].non_std_match(toSLV(~SYM[2]),toSLV(~SYM[3]))) report (~LIT[#{arg4}] & ", expected: " & ~INCLUDENAME[0].slv2string(toSLV(~SYM[3])) & ", actual: " & ~INCLUDENAME[0].slv2string(toSLV(~SYM[2]))) severity error;
              end if;
            end process;
            -- pragma translate_on
            ~RESULT <= ~ARG[#{arg7}];
          end block;
          -- assertBitVector end
    |]) #-}



-- |
--
-- Example:
--
-- @
-- testInput
--   :: Clock dom
--   -> Reset dom
--   -> 'Signal' dom Int
-- testInput clk rst = 'stimuliGenerator' clk rst $('Clash.Sized.Vector.listToVecTH' [(1::Int),3..21])
-- @
--
-- >>> sampleN 14 (testInput systemClockGen resetGen)
-- [1,1,3,5,7,9,11,13,15,17,19,21,21,21]
stimuliGenerator
  :: forall l dom   a
   . ( KnownNat l
     )
  => Clock dom
  -- ^ Clock to which to synchronize the output signal
  -> Reset dom
  -> Vec l a
  -- ^ Samples to generate
  -> Signal dom a
  -- ^ Signal of given samples
stimuliGenerator clk rst samples =
    let (r,o) = unbundle (genT <$> register clk rst (toEnable (pure True)) 0 r)
    in  o
  where
    genT :: Index l -> (Index l,a)
    genT s = (s',samples !! s)
      where
        maxI = toEnum (length samples - 1)

        s' = if s < maxI
                then s + 1
                else s
{-# INLINABLE stimuliGenerator #-}

-- | Same as 'outputVerifier' but used in cases where the test bench domain and
-- the domain of the circuit under test are the same.
outputVerifier'
  :: forall l a dom
   . ( KnownNat l
     , Eq a
     , ShowX a
     , 1 <= l
     )
  => Clock dom
  -- ^ Clock to which the test bench is synchronized
  -> Reset dom
  -- ^ Reset line of test bench
  -> Vec l a
  -- ^ Samples to compare with
  -> Signal dom a
  -- ^ Signal to verify
  -> Signal dom Bool
  -- ^ Indicator that all samples are verified
outputVerifier' clk =
  outputVerifier @l @a clk clk
{-# INLINE outputVerifier' #-}

-- | Compare a signal (coming from a circuit) to a vector of samples. If a
-- sample from the signal is not equal to the corresponding sample in the
-- vector, print to stderr and continue testing. This function is
-- synthesizable in the sense that HDL simulators will run it. If @testDom@ and
-- @circuitDom@ refer to the same domain, it can also be synthesized into
-- hardware.
--
-- __NB__: This function uses 'assert'. When simulating this function in
-- @clashi@, read the [note](#assert-clashi).
--
-- Example:
--
-- @
-- expectedOutput
--   :: Clock dom -> Reset dom
--   -> 'Signal' dom Int -> 'Signal' dom Bool
-- expectedOutput clk rst = 'outputVerifier' clk rst $('Clash.Sized.Vector.listToVecTH' ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-- @
--
-- >>> import qualified Data.List as List
-- >>> sampleN 12 (expectedOutput systemClockGen resetGen (fromList (0:[0..10] List.++ [10,10,10])))
-- <BLANKLINE>
-- cycle(<Clock: System>): 0, outputVerifier
-- expected value: 70, not equal to actual value: 0
-- [False
-- cycle(<Clock: System>): 1, outputVerifier
-- expected value: 70, not equal to actual value: 0
-- ,False
-- cycle(<Clock: System>): 2, outputVerifier
-- expected value: 99, not equal to actual value: 1
-- ,False,False,False,False,False
-- cycle(<Clock: System>): 7, outputVerifier
-- expected value: 7, not equal to actual value: 6
-- ,False
-- cycle(<Clock: System>): 8, outputVerifier
-- expected value: 8, not equal to actual value: 7
-- ,False
-- cycle(<Clock: System>): 9, outputVerifier
-- expected value: 9, not equal to actual value: 8
-- ,False
-- cycle(<Clock: System>): 10, outputVerifier
-- expected value: 10, not equal to actual value: 9
-- ,False,True]
--
-- If you're working with 'BitVector's containing don't care bits you should
-- use 'outputVerifierBitVector'.
outputVerifier
  :: forall l a testDom circuitDom
   . ( KnownNat l
     , Eq a
     , ShowX a
     , 1 <= l
     )
  => Clock testDom
  -- ^ Clock to which the test bench is synchronized (but not necessarily
  -- the circuit under test)
  -> Clock circuitDom
  -- ^ Clock to which the circuit under test is synchronized
  -> Reset testDom
  -- ^ Reset line of test bench
  -> Vec l a
  -- ^ Samples to compare with
  -> Signal circuitDom a
  -- ^ Signal to verify
  -> Signal testDom Bool
  -- ^ True if all samples are verified
outputVerifier =
  outputVerifierWith (\clk rst -> assert clk rst "outputVerifier")
{-# INLINE outputVerifier #-}

-- | Same as 'outputVerifier'', but can handle don't care bits in its expected
-- values.
outputVerifierBitVector'
  :: forall l n dom
   . ( KnownNat l
     , KnownNat n
     , 1 <= l
     )
  => Clock dom
  -- ^ Clock to which the input signal is synchronized
  -> Reset dom
  -> Vec l (BitVector n)
  -- ^ Samples to compare with
  -> Signal dom (BitVector n)
  -- ^ Signal to verify
  -> Signal dom Bool
  -- ^ Indicator that all samples are verified
outputVerifierBitVector' clk =
  outputVerifierBitVector @l @n clk clk
{-# INLINE outputVerifierBitVector' #-}

-- | Same as 'outputVerifier', but can handle don't care bits in its
-- expected values.
outputVerifierBitVector
  :: forall l n testDom circuitDom
   . ( KnownNat l
     , KnownNat n
     , 1 <= l
     )
  => Clock testDom
  -- ^ Clock to which the test bench is synchronized (but not necessarily
  -- the circuit under test)
  -> Clock circuitDom
  -- ^ Clock to which the circuit under test is synchronized
  -> Reset testDom
  -- ^ Reset line of test bench
  -> Vec l (BitVector n)
  -- ^ Samples to compare with
  -> Signal circuitDom (BitVector n)
  -- ^ Signal to verify
  -> Signal testDom Bool
  -- ^ Indicator that all samples are verified
outputVerifierBitVector =
  outputVerifierWith
    (\clk rst -> assertBitVector clk rst "outputVerifierBitVector")
{-# INLINE outputVerifierBitVector #-}

outputVerifierWith
  :: forall l a testDom circuitDom
   . ( KnownNat l
     , Eq a
     , ShowX a
     , 1 <= l
     )
  => (    Clock testDom
       -> Reset testDom
       -> Signal testDom a
       -> Signal testDom a
       -> Signal testDom Bool
       -> Signal testDom Bool
      )
  -- ^ The @assert@ function to use
  -> Clock testDom
  -- ^ Clock to which the test bench is synchronized (but not necessarily
  -- the circuit under test)
  -> Clock circuitDom
  -- ^ Clock to which the circuit under test is synchronized
  -> Reset testDom
  -- ^ Reset line of test bench
  -> Vec l a
  -- ^ Samples to compare with
  -> Signal circuitDom a
  -- ^ Signal to verify
  -> Signal testDom Bool
  -- ^ True if all samples are verified
outputVerifierWith assertF clkTest clkCircuit rst samples i0 =
    let i1    = unsafeSimSynchronizer clkCircuit clkTest i0
        en    = toEnable (pure True)
        (s,o) = unbundle (genT <$> register clkTest rst en 0 s)
        (e,f) = unbundle o
        f'    = register clkTest rst en False f
        -- Only assert while not finished
    in  mux f' f' $ assertF clkTest rst i1 e f'
  where
    genT :: Index l -> (Index l,(a,Bool))
    genT s = (s',(samples !! s,finished))
      where
        s' = satSucc SatBound s
        finished = s == maxBound
{-# INLINABLE outputVerifierWith #-}

-- | Ignore signal for a number of cycles, while outputting a static value.
ignoreFor
  :: forall dom  n a
   . Clock dom
  -> Reset dom
  -> Enable dom
  -> SNat n
  -- ^ Number of cycles to ignore incoming signal
  -> a
  -- ^ Value function produces when ignoring signal
  -> Signal dom a
  -- ^ Incoming signal
  -> Signal dom a
  -- ^ Either a passthrough of the incoming signal, or the static value
  -- provided as the second argument.
ignoreFor clk rst en SNat a i =
  mux ((==) <$> counter <*> (pure maxBound)) i (pure a)
 where
  counter :: Signal dom (Index (n+1))
  counter = register clk rst en 0 (satSucc SatBound <$> counter)

-- | Same as 'tbClockGen', but returns two clocks on potentially different
-- domains. To be used in situations where the test circuit potentially operates
-- on a different clock than the device under test.
biTbClockGen
  :: forall testDom circuitDom
   . ( KnownDomain testDom
     , KnownDomain circuitDom
     )
  => Signal testDom Bool
  -> (Clock testDom, Clock circuitDom)
biTbClockGen done = (testClk, circuitClk)
 where
  testClk = tbClockGen done
  circuitClk = tbClockGen (unsafeSynchronizer testClk circuitClk done)

-- | Enable signal that's always enabled. Because it has a blackbox definition
-- this enable signal is opaque to other blackboxes. It will therefore never
-- be optimized away.
tbEnableGen :: Enable tag
tbEnableGen = toEnable (pure True)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE tbEnableGen #-}
{-# ANN tbEnableGen hasBlackBox #-}

-- | Clock generator for the 'System' clock domain.
--
-- __NB__: can be used in the /testBench/ function
--
-- === __Example__
--
-- @
-- topEntity :: Vec 2 (Vec 3 (Unsigned 8)) -> Vec 6 (Unsigned 8)
-- topEntity = concat
--
-- testBench :: Signal System Bool
-- testBench = done
--   where
--     testInput      = pure ((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil)
--     expectedOutput = outputVerifier' ((1:>2:>3:>4:>5:>6:>Nil):>Nil)
--     done           = exposeClockResetEnable (expectedOutput (topEntity <$> testInput)) clk rst
--     clk            = 'tbSystemClockGen' (not <\$\> done)
--     rst            = systemResetGen
-- @
tbSystemClockGen
  :: Signal System Bool
  -> Clock System
tbSystemClockGen = tbClockGen

-- | Convert a single-ended clock to a differential clock
--
-- The 'tbClockGen' function generates a single-ended clock. This function will
-- output the two phases of a differential clock corresponding to that
-- single-ended clock.
--
-- This function is only meant to be used in the /testBench/ function, not to
-- create a differential output in hardware.
--
-- Example:
--
-- @
-- clk = seClockToDiffClock $ tbClockGen (not \<\$\> done)
-- @
seClockToDiffClock ::
  -- | Single-ended input
  Clock dom ->
  -- | Differential output
  DiffClock dom
seClockToDiffClock clk@ExtractClockDom = DiffClock clk (ClockN SSymbol)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE seClockToDiffClock #-}
{-# ANN seClockToDiffClock hasBlackBox #-}

-- | Cross clock domains in a way that is unsuitable for hardware but good
-- enough for simulation.
--
-- It's equal to 'unsafeSynchronizer' but will warn when used outside of a test
-- bench. 'outputVerifier' uses this function when it needs to cross between
-- clock domains, which will render it unsuitable for synthesis, but good enough
-- for simulating the generated HDL.
unsafeSimSynchronizer
  :: forall dom1 dom2 a
   . Clock dom1
  -- ^ 'Clock' of the incoming signal
  -> Clock dom2
  -- ^ 'Clock' of the outgoing signal
  -> Signal dom1 a
  -> Signal dom2 a
unsafeSimSynchronizer = unsafeSynchronizer
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeSimSynchronizer #-}
{-# ANN unsafeSimSynchronizer hasBlackBox #-}
