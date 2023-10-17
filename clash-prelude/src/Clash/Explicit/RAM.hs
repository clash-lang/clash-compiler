{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

RAM primitives with a combinational read port.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE Trustworthy #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.RAM
  ( -- * RAM synchronized to an arbitrary clock
    asyncRam
  , asyncRamPow2
    -- * Internal
  , asyncRam#
  )
where

import Data.Maybe            (isJust)
import GHC.Stack             (HasCallStack, withFrozenCallStack)
import GHC.TypeLits          (KnownNat)
import qualified Data.Sequence as Seq

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Explicit.Signal (unbundle, andEnable)
import Clash.Promoted.Nat    (SNat (..), snatToNum, pow2SNat)
import Clash.Signal.Internal
  (Clock (..), ClockAB (..), Signal (..), Enable, fromEnable, clockTicks)
import Clash.Signal.Internal.Ambiguous (clockPeriod)
import Clash.Sized.Unsigned  (Unsigned)
import Clash.XException
  (defaultSeqX, deepErrorX, fromJustX, maybeIsX, NFDataX)

import Clash.Annotations.Primitive(Primitive (InlineYamlPrimitive), HDL(..))
import Data.List.Infinite (Infinite((:<)), (...))
import Data.String.Interpolate (__i)

-- | Create a RAM with space for 2^@n@ elements
--
-- * __NB__: Initial content of the RAM is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRamPow2
  :: forall wdom rdom n a
   . ( KnownNat n
     , HasCallStack
     , NFDataX a
     )
  => Clock wdom
   -- ^ 'Clock' to which the write port of the RAM is synchronized
  -> Clock rdom
   -- ^ 'Clock' to which the read address signal, @r@, is synchronized
  -> Enable wdom
  -- ^ 'Enable' line for the write port
  -> Signal rdom (Unsigned n)
  -- ^ Read address @r@
  -> Signal wdom (Maybe (Unsigned n, a))
  -- ^ (write address @w@, value to write)
  -> Signal rdom a
  -- ^ Value of the RAM at address @r@
asyncRamPow2 = \wclk rclk en rd wrM -> withFrozenCallStack
  (asyncRam wclk rclk en (pow2SNat (SNat @n)) rd wrM)
{-# INLINE asyncRamPow2 #-}


-- | Create a RAM with space for @n@ elements
--
-- * __NB__: Initial content of the RAM is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Explicit.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRam
  :: ( Enum addr
     , NFDataX addr
     , HasCallStack
     , NFDataX a
     )
  => Clock wdom
   -- ^ 'Clock' to which the write port of the RAM is synchronized
  -> Clock rdom
   -- ^ 'Clock' to which the read address signal, @r@, is synchronized
  -> Enable wdom
  -- ^ 'Enable' line for the write port
  -> SNat n
  -- ^ Size @n@ of the RAM
  -> Signal rdom addr
  -- ^ Read address @r@
  -> Signal wdom (Maybe (addr, a))
  -- ^ (write address @w@, value to write)
  -> Signal rdom a
   -- ^ Value of the RAM at address @r@
asyncRam = \wclk rclk gen sz rd wrM ->
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJustX <$> wrM)
  in  withFrozenCallStack
      (asyncRam# wclk rclk gen sz (fromEnum <$> rd) en (fromEnum <$> wr) din)
{-# INLINE asyncRam #-}

-- | RAM primitive
asyncRam#
  :: forall wdom rdom n a
   . ( HasCallStack
     , NFDataX a
     )
  => Clock wdom
   -- ^ 'Clock' to which the write port of the RAM is synchronized
  -> Clock rdom
   -- ^ 'Clock' to which the read address signal, @r@, is synchronized
  -> Enable wdom
  -- ^ 'Enable' line for the write port
  -> SNat n
  -- ^ Size @n@ of the RAM
  -> Signal rdom Int
  -- ^ Read address @r@
  -> Signal wdom Bool
  -- ^ Write enable
  -> Signal wdom Int
  -- ^ Write address @w@
  -> Signal wdom a
  -- ^ Value to write (at address @w@)
  -> Signal rdom a
  -- ^ Value of the RAM at address @r@
asyncRam# wClk@ExtractClockDom rClk@ExtractClockDom en sz rd we wr din = dout
  where
    ramI = Seq.replicate
              szI
              (withFrozenCallStack (deepErrorX "asyncRam: initial value undefined"))
    en0 = fromEnable (andEnable en we)
    dout = if rPeriod == wPeriod
           then goSingle ramI rd en0 wr din
           else go (clockTicks wClk rClk) ramI rd en0 wr din
    rPeriod = snatToNum (clockPeriod @rdom) :: Int
    wPeriod = snatToNum (clockPeriod @wdom) :: Int
    szI = snatToNum sz :: Int

    goSingle :: Seq.Seq a -> Signal rdom Int -> Signal wdom Bool
       -> Signal wdom Int -> Signal wdom a -> Signal rdom a
    goSingle !ram (r :- rs) ~(e :- es) wt@(~(w :- ws)) dt@(~(d :- ds)) =
      let ram0 = upd ram e w d
          o    = ram `safeAt` r
      in  o :- (o `defaultSeqX` wt `seq` dt `seq` goSingle ram0 rs es ws ds)

    go :: [ClockAB] -> Seq.Seq a -> Signal rdom Int -> Signal wdom Bool
       -> Signal wdom Int -> Signal wdom a -> Signal rdom a
    go [] _ _ _ _ _ = error "asyncRam#.go: `ticks` should have been an infinite list"
    go (tick:ticks) !ram rt@(~(r :- rs)) et@(~(e :- es)) wt@(~(w :- ws)) dt@(~(d :- ds)) =
      case tick of
        ClockA  ->
          let ram0 = upd ram e w d
          in  wt `seq` dt `seq` go ticks ram0 rt es ws ds
        ClockB  ->
          let o = ram `safeAt` r
          in  o :- (o `defaultSeqX` go ticks ram rs et wt dt)
        ClockAB ->
          go (ClockB:ClockA:ticks) ram rt et wt dt

    upd ram we0 waddr d = case maybeIsX we0 of
      Nothing -> case maybeIsX waddr of
        Nothing -> -- Put the XException from `waddr` as the value in all
                   -- locations of `ram`.
                   seq waddr d <$ ram
        Just wa -> -- Put the XException from `we` as the value at address
                   -- `waddr`.
                   safeUpdate wa (seq we0 d) ram
      Just True -> case maybeIsX waddr of
        Nothing -> -- Put the XException from `waddr` as the value in all
                   -- locations of `ram`.
                   seq waddr d <$ ram
        Just wa -> d `defaultSeqX` safeUpdate wa d ram
      _ -> ram

    safeAt :: HasCallStack => Seq.Seq a -> Int -> a
    safeAt s i =
      if (0 <= i) && (i < szI) then
        Seq.index s i
      else
        withFrozenCallStack
          (deepErrorX ("asyncRam: read address " ++ show i ++
                   " not in range [0.." ++ show szI ++ ")"))
    {-# INLINE safeAt #-}

    safeUpdate :: HasCallStack => Int -> a -> Seq.Seq a ->  Seq.Seq a
    safeUpdate i a s =
      if (0 <= i) && (i < szI) then
        Seq.update i a s
      else
        let d = withFrozenCallStack
                  (deepErrorX ("asyncRam: write address " ++ show i ++
                           " not in range [0.." ++ show szI ++ ")"))
        in d <$ s
    {-# INLINE safeUpdate #-}
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE asyncRam# #-}
{-# ANN asyncRam# hasBlackBox #-}
{-# ANN asyncRam# (
  let
    bbName = show 'asyncRam#
    _arg0 :<   _arg3 :< arg4 :< _arg5 :< arg6 :< arg7 :< arg8 :< arg9 :< arg10 :< arg11 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [SystemVerilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          asyncRam\#
            :: ( HasCallStack      --         ARG[0]

               , NFDataX a )       --         ARG[3]
            => Clock wdom          -- ^ wclk, ARG[4]
            -> Clock rdom          -- ^ rclk, ARG[5]
            -> Enable wdom         -- ^ wen,  ARG[6]
            -> SNat n              -- ^ sz,   ARG[7]
            -> Signal rdom Int     -- ^ rd,   ARG[8]
            -> Signal wdom Bool    -- ^ en,   ARG[9]
            -> Signal wdom Int     -- ^ wr,   ARG[10]
            -> Signal wdom a       -- ^ din,  ARG[11]
            -> Signal rdom a
        template: |-
          // asyncRam begin
          logic [~SIZE[~TYP[#{arg11}]]-1:0] ~GENSYM[RAM][0] [0:~LIT[#{arg7}]-1];
          always @(~IF ~ACTIVEEDGE[Rising][#{arg4}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg4}]) begin : ~GENSYM[~COMPNAME_Ram][1]
            if (~IF ~ISACTIVEENABLE[#{arg6}] ~THEN ~ARG[#{arg6}] & ~ELSE ~FI ~ARG[#{arg9}]) begin
              ~SYM[0][~ARG[#{arg10}]] <= ~TOBV[~ARG[#{arg11}]][~TYP[#{arg11}]];
            end
          end

          assign ~RESULT = ~FROMBV[~SYM[0][\\~ARG[#{arg8}]\\]][~TYPO];
          // asyncRam end
    |]) #-}
{-# ANN asyncRam# (
  let
    bbName = show 'asyncRam#
    _arg0 :<   _arg3 :< arg4 :< _arg5 :< arg6 :< arg7 :< arg8 :< arg9 :< arg10 :< arg11 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [Verilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          asyncRam\#
            :: ( HasCallStack      --         ARG[0]

               , NFDataX a )       --         ARG[3]
            => Clock wdom          -- ^ wclk, ARG[4]
            -> Clock rdom          -- ^ rclk, ARG[5]
            -> Enable wdom         -- ^ wen,  ARG[6]
            -> SNat n              -- ^ sz,   ARG[7]
            -> Signal rdom Int     -- ^ rd,   ARG[8]
            -> Signal wdom Bool    -- ^ en,   ARG[9]
            -> Signal wdom Int     -- ^ wr,   ARG[10]
            -> Signal wdom a       -- ^ din,  ARG[11]
            -> Signal rdom a
        template: |-
          // asyncRam begin
          reg ~TYPO ~GENSYM[RAM][0] [0:~LIT[#{arg7}]-1];
          always @(~IF ~ACTIVEEDGE[Rising][#{arg4}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg4}]) begin : ~GENSYM[~COMPNAME_Ram][1]
            if (~ARG[#{arg9}] ~IF ~ISACTIVEENABLE[#{arg6}] ~THEN & ~ARG[#{arg6}] ~ELSE ~FI) begin
              ~SYM[0][~ARG[#{arg10}]] <= ~ARG[#{arg11}];
            end
          end

          assign ~RESULT = ~SYM[0][~ARG[#{arg8}]];
          // asyncRam end
    |]) #-}
{-# ANN asyncRam# (
  let
    bbName = show 'asyncRam#
    _arg0 :<   _arg3 :< arg4 :< _arg5 :< arg6 :< arg7 :< arg8 :< arg9 :< arg10 :< arg11 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          asyncRam\#
            :: ( HasCallStack      --         ARG[0]

               , NFDataX a )       --         ARG[3]
            => Clock wdom          -- ^ wclk, ARG[4]
            -> Clock rdom          -- ^ rclk, ARG[5]
            -> Enable wdom         -- ^ wen,  ARG[6]
            -> SNat n              -- ^ sz,   ARG[7]
            -> Signal rdom Int     -- ^ rd,   ARG[8]
            -> Signal wdom Bool    -- ^ en,   ARG[9]
            -> Signal wdom Int     -- ^ wr,   ARG[10]
            -> Signal wdom a       -- ^ din,  ARG[11]
            -> Signal rdom a
        template: |-
          -- asyncRam begin
          ~GENSYM[~COMPNAME_asyncRam][0] : block~IF ~VIVADO ~THEN
            type ~GENSYM[RamType][4] is array(natural range <>) of std_logic_vector(~SIZE[~TYP[#{arg11}]]-1 downto 0);~ELSE
            type ~SYM[4] is array(natural range <>) of ~TYP[#{arg11}];~FI
            signal ~GENSYM[RAM][1] : ~SYM[4](0 to ~LIT[#{arg7}]-1);
            signal ~GENSYM[rd][2] : integer range 0 to ~LIT[#{arg7}] - 1;
            signal ~GENSYM[wr][3] : integer range 0 to ~LIT[#{arg7}] - 1;
          begin
            ~SYM[2] <= to_integer(~VAR[rdI][#{arg8}](31 downto 0))
            -- pragma translate_off
                          mod ~LIT[#{arg7}]
            -- pragma translate_on
                          ;

            ~SYM[3] <= to_integer(~VAR[wrI][#{arg10}](31 downto 0))
            -- pragma translate_off
                          mod ~LIT[#{arg7}]
            -- pragma translate_on
                          ;
            ~GENSYM[asyncRam_sync][7] : process(~ARG[#{arg4}])
            begin
              if ~IF ~ACTIVEEDGE[Rising][#{arg4}] ~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{arg4}]) then
                if (~ARG[#{arg9}] ~IF ~ISACTIVEENABLE[#{arg6}] ~THEN and ~ARG[#{arg6}] ~ELSE ~FI) then~IF ~VIVADO ~THEN
                  ~SYM[1](~SYM[3]) <= ~TOBV[~ARG[#{arg11}]][~TYP[#{arg11}]];~ELSE
                  ~SYM[1](~SYM[3]) <= ~ARG[#{arg11}];~FI
                end if;
              end if;
            end process;
            ~IF ~VIVADO ~THEN
            ~RESULT <= ~FROMBV[~SYM[1](~SYM[2])][~TYP[#{arg11}]];~ELSE
            ~RESULT <= ~SYM[1](~SYM[2]);~FI
          end block;
          -- asyncRam end
    |]) #-}
