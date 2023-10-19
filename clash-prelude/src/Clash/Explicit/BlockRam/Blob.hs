{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

= Efficient bundling of initial RAM content with the compiled code

Leveraging Template Haskell, the initial content for the block RAM components in
this module is stored alongside the compiled Haskell code. It covers use cases
where passing the initial content as a 'Clash.Sized.Vector.Vec' turns out to be
problematically slow.

The data is stored efficiently, with very little overhead (worst-case 7%, often
no overhead at all).

Unlike "Clash.Explicit.BlockRam.File", "Clash.Explicit.BlockRam.Blob"
generates practically the same HDL as "Clash.Explicit.BlockRam" and is
compatible with all tools consuming the generated HDL.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.BlockRam.Blob
  ( -- * Block RAMs initialized with a 'MemBlob'
    blockRamBlob
  , blockRamBlobPow2
    -- * Creating and inspecting 'MemBlob'
  , MemBlob
  , createMemBlob
  , memBlobTH
  , unpackMemBlob
    -- * Internal
  , blockRamBlob#
  ) where

import Control.Exception (catch, throw)
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeInterleaveST, unsafeIOToST, unsafeSTToIO)
import Data.Array.MArray (newListArray)
import qualified Data.ByteString.Lazy as L
import Data.Maybe (isJust)
import GHC.Arr (STArray, unsafeReadSTArray, unsafeWriteSTArray)
import GHC.Stack (withFrozenCallStack)
import GHC.TypeLits (KnownNat, type (^))
import Language.Haskell.TH
  (DecsQ, ExpQ, integerL, litE, litT, mkName, normalB, numTyLit, sigD,
   stringPrimL, valD, varP)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Class.BitPack.Internal (BitPack, BitSize)
import Clash.Explicit.BlockRam.Internal
  (MemBlob(..), packBVs, unpackMemBlob, unpackMemBlob0)
import Clash.Explicit.Signal (Enable, fromEnable)
import Clash.Promoted.Nat (natToInteger, natToNum)
import Clash.Signal.Bundle (unbundle)
import Clash.Signal.Internal (Clock, Signal(..), (.&&.))
import Clash.Sized.Internal.BitVector (Bit(..), BitVector(..))
import Clash.Sized.Internal.Unsigned (Unsigned)
import Clash.XException
  (maybeIsX, deepErrorX, defaultSeqX, fromJustX, NFDataX, XException (..), seqX)

import Clash.Annotations.Primitive(Primitive (InlineYamlPrimitive), HDL(..))
import Data.List.Infinite (Infinite((:<)), (...))
import Data.String.Interpolate (__i)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -fplugin GHC.TypeLits.Normalise
-- >>> :set -fplugin GHC.TypeLits.KnownNat.Solver
-- >>> :m -Prelude
-- >>> import Clash.Explicit.Prelude

-- | Create a block RAM with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
--
-- === See also:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- block RAM.
-- * Use the adapter 'Clash.Explicit.BlockRam.readNew' for obtaining
-- write-before-read semantics like this: @'Clash.Explicit.BlockRam.readNew'
-- clk rst en ('blockRamBlob' clk en content) rd wrM@.
blockRamBlob
  :: forall dom addr m n
   . ( Enum addr
     , NFDataX addr
     )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> MemBlob n m
  -- ^ Initial content of the BRAM, also determines the size, @n@, of the BRAM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal dom (BitVector m)
  -- ^ Value of the BRAM at address @r@ from the previous clock cycle
blockRamBlob = \clk gen content@MemBlob{} rd wrM ->
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJustX <$> wrM)
  in blockRamBlob# clk gen content (fromEnum <$> rd) en (fromEnum <$> wr) din
{-# INLINE blockRamBlob #-}

-- | Create a block RAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'XException'
--
-- === See also:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- block RAM.
-- * Use the adapter 'Clash.Explicit.BlockRam.readNew' for obtaining
-- write-before-read semantics like this: @'Clash.Explicit.BlockRam.readNew'
-- clk rst en ('blockRamBlobPow2' clk en content) rd wrM@.
blockRamBlobPow2
  :: forall dom m n
   . ( KnownNat n
     )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> MemBlob (2^n) m
  -- ^ Initial content of the BRAM, also determines the size, 2^@n@, of the BRAM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom (Maybe (Unsigned n, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal dom (BitVector m)
  -- ^ Value of the BRAM at address @r@ from the previous clock cycle
blockRamBlobPow2 = blockRamBlob
{-# INLINE blockRamBlobPow2 #-}

-- | blockRAMBlob primitive
blockRamBlob#
  :: forall dom m n
   . Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> MemBlob n m
  -- ^ Initial content of the BRAM, also determines the size, @n@, of the BRAM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom Int
  -- ^ Read address @r@
  -> Signal dom Bool
  -- ^ Write enable
  -> Signal dom Int
  -- ^ Write address @w@
  -> Signal dom (BitVector m)
  -- ^ Value to write (at address @w@)
  -> Signal dom (BitVector m)
  -- ^ Value of the BRAM at address @r@ from the previous clock cycle
blockRamBlob# !_ gen content@MemBlob{} = \rd wen waS wd -> runST $ do
  bvList <- unsafeIOToST (unpackMemBlob0 content)
  ramStart <- newListArray (0,szI-1) bvList
  go
    ramStart
    (withFrozenCallStack (deepErrorX "blockRamBlob: intial value undefined"))
    (fromEnable gen)
    rd
    (fromEnable gen .&&. wen)
    waS
    wd
 where
  szI = natToNum @n @Int

  go :: STArray s Int (BitVector m) -> BitVector m -> Signal dom Bool
     -> Signal dom Int -> Signal dom Bool -> Signal dom Int
     -> Signal dom (BitVector m) -> ST s (Signal dom (BitVector m))
  go !ram o ret@(~(re :- res)) rt@(~(r :- rs)) et@(~(e :- en)) wt@(~(w :- wr))
     dt@(~(d :- din)) = do
    o `seqX` (o :-) <$> (ret `seq` rt `seq` et `seq` wt `seq` dt `seq`
      unsafeInterleaveST
        (do o' <- unsafeIOToST
                    (catch (if re then unsafeSTToIO (ram `safeAt` r) else pure o)
                    (\err@XException {} -> pure (throw err)))
            d `defaultSeqX` upd ram e w d
            go ram o' res rs en wr din))

  upd :: STArray s Int (BitVector m) -> Bool -> Int -> BitVector m -> ST s ()
  upd ram we waddr d = case maybeIsX we of
    Nothing -> case maybeIsX waddr of
      Nothing -> -- Put the XException from `waddr` as the value in all
                 -- locations of `ram`.
                 forM_ [0..(szI-1)] (\i -> unsafeWriteSTArray ram i (seq waddr d))
      Just wa -> -- Put the XException from `we` as the value at address
                 -- `waddr`.
                 safeUpdate wa (seq we d) ram
    Just True -> case maybeIsX waddr of
      Nothing -> -- Put the XException from `waddr` as the value in all
                 -- locations of `ram`.
                 forM_ [0..(szI-1)] (\i -> unsafeWriteSTArray ram i (seq waddr d))
      Just wa -> safeUpdate wa d ram
    _ -> return ()

  safeAt :: STArray s Int (BitVector m) -> Int -> ST s (BitVector m)
  safeAt s i =
    if (0 <= i) && (i < szI) then
      unsafeReadSTArray s i
    else pure $
      withFrozenCallStack
        (deepErrorX ("blockRamBlob: read address " <> show i <>
                     " not in range [0.." <> show szI <> ")"))
  {-# INLINE safeAt #-}

  safeUpdate :: Int -> BitVector m -> STArray s Int (BitVector m) -> ST s ()
  safeUpdate i a s =
    if (0 <= i) && (i < szI) then
      unsafeWriteSTArray s i a
    else
      let d = withFrozenCallStack
                (deepErrorX ("blockRam: write address " <> show i <>
                             " not in range [0.." <> show szI <> ")"))
       in forM_ [0..(szI-1)] (\j -> unsafeWriteSTArray s j d)
  {-# INLINE safeUpdate #-}
{-# ANN blockRamBlob# hasBlackBox #-}
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE blockRamBlob# #-}
{-# ANN blockRamBlob# (
  let
    bbName = show 'blockRamBlob#
    arg1 :< arg2 :< arg3 :< arg4 :< arg5 :< arg6 :< arg7 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [SystemVerilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        type: |-
          blockRamBlob\#
            :: Clock dom                 -- clk,  ARG[1]
            -> Enable dom                -- en,   ARG[2]
            -> MemBlob n m               -- init, ARG[3]
            -> Signal dom Int            -- rd,   ARG[4]
            -> Signal dom Bool           -- wren, ARG[5]
            -> Signal dom Int            -- wr,   ARG[6]
            -> Signal dom (BitVector m)  -- din,  ARG[7]
            -> Signal dom (BitVector m)
        template: |-
          // blockRamBlob begin
          ~SIGD[~GENSYM[RAM][1]][#{arg3}];
          logic [~SIZE[~TYP[#{arg7}]]-1:0] ~GENSYM[~RESULT_q][2];
          initial begin
            ~SYM[1] = ~CONST[#{arg3}];
          end~IF ~ISACTIVEENABLE[#{arg2}] ~THEN
          always @(~IF ~ACTIVEEDGE[Rising][#{arg1}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg1}]) begin : ~GENSYM[~COMPNAME_blockRam][3]~IF ~VIVADO ~THEN
            if (~ARG[#{arg2}]) begin
              if (~ARG[#{arg5}]) begin
                ~SYM[1][~ARG[#{arg6}]] <= ~ARG[#{arg7}];
              end
              ~SYM[2] <= ~SYM[1][~ARG[#{arg4}]];
            end~ELSE
            if (~ARG[#{arg5}] & ~ARG[#{arg2}]) begin
              ~SYM[1][~ARG[#{arg6}]] <= ~ARG[#{arg7}];
            end
            if (~ARG[#{arg2}]) begin
              ~SYM[2] <= ~SYM[1][~ARG[#{arg4}]];
            end~FI
          end~ELSE
          always @(~IF ~ACTIVEEDGE[Rising][#{arg1}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg1}]) begin : ~SYM[3]
            if (~ARG[#{arg5}]) begin
              ~SYM[1][~ARG[#{arg6}]] <= ~ARG[#{arg7}];
            end
            ~SYM[2] <= ~SYM[1][~ARG[#{arg4}]];
          end~FI
          assign ~RESULT = ~SYM[2];
          // blockRamBlob end
    |]) #-}
{-# ANN blockRamBlob# (
  let
    bbName = show 'blockRamBlob#
    arg1 :< arg2 :< arg3 :< arg4 :< arg5 :< arg6 :< arg7 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [Verilog] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        outputUsage: NonBlocking
        type: |-
          blockRamBlob\#
            :: Clock dom                 -- clk,  ARG[1]
            -> Enable dom                -- en,   ARG[2]
            -> MemBlob n m               -- init, ARG[3]
            -> Signal dom Int            -- rd,   ARG[4]
            -> Signal dom Bool           -- wren, ARG[5]
            -> Signal dom Int            -- wr,   ARG[6]
            -> Signal dom (BitVector m)  -- din,  ARG[7]
            -> Signal dom (BitVector m)
        template: |-
          // blockRamBlob begin
          reg ~TYPO ~GENSYM[~RESULT_RAM][1] [0:~LENGTH[~TYP[#{arg3}]]-1];

          reg ~TYP[#{arg3}] ~GENSYM[ram_init][3];
          integer ~GENSYM[i][4];
          initial begin
            ~SYM[3] = ~CONST[#{arg3}];
            for (~SYM[4]=0; ~SYM[4] < ~LENGTH[~TYP[#{arg3}]]; ~SYM[4] = ~SYM[4] + 1) begin
              ~SYM[1][~LENGTH[~TYP[#{arg3}]]-1-~SYM[4]] = ~SYM[3][~SYM[4]*~SIZE[~TYPO]+:~SIZE[~TYPO]];
            end
          end
          ~IF ~ISACTIVEENABLE[#{arg2}] ~THEN
          always @(~IF ~ACTIVEEDGE[Rising][#{arg1}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg1}]) begin : ~GENSYM[~RESULT_blockRam][5]~IF ~VIVADO ~THEN
            if (~ARG[#{arg2}]) begin
              if (~ARG[#{arg5}]) begin
                ~SYM[1][~ARG[#{arg6}]] <= ~ARG[#{arg7}];
              end
              ~RESULT <= ~SYM[1][~ARG[#{arg4}]];
            end~ELSE
            if (~ARG[#{arg5}] & ~ARG[#{arg2}]) begin
              ~SYM[1][~ARG[#{arg6}]] <= ~ARG[#{arg7}];
            end
            if (~ARG[#{arg2}]) begin
              ~RESULT <= ~SYM[1][~ARG[#{arg4}]];
            end~FI
          end~ELSE
          always @(~IF ~ACTIVEEDGE[Rising][#{arg1}] ~THENposedge~ELSEnegedge~FI ~ARG[#{arg1}]) begin : ~SYM[5]
            if (~ARG[#{arg5}]) begin
              ~SYM[1][~ARG[#{arg6}]] <= ~ARG[#{arg7}];
            end
            ~RESULT <= ~SYM[1][~ARG[#{arg4}]];
          end~FI
          // blockRamBlob end
    |]) #-}
{-# ANN blockRamBlob# (
  let
    bbName = show 'blockRamBlob#
    arg1 :< arg2 :< arg3 :< arg4 :< arg5 :< arg6 :< arg7 :< _ = ((0 :: Int)...)
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        name: '#{bbName}'
        kind: Declaration
        outputUsage: NonBlocking
        type: |-
          blockRamBlob\#
            :: Clock dom                 -- clk,  ARG[1]
            -> Enable dom                -- en,   ARG[2]
            -> MemBlob n m               -- init, ARG[3]
            -> Signal dom Int            -- rd,   ARG[4]
            -> Signal dom Bool           -- wren, ARG[5]
            -> Signal dom Int            -- wr,   ARG[6]
            -> Signal dom (BitVector m)  -- din,  ARG[7]
            -> Signal dom (BitVector m)
        template: |-
          -- blockRamBlob begin
          ~GENSYM[~RESULT_blockRam][1] : block
            signal ~GENSYM[~RESULT_RAM][2] : ~TYP[#{arg3}] := ~CONST[#{arg3}];
            signal ~GENSYM[rd][4]  : integer range 0 to ~LENGTH[~TYP[#{arg3}]] - 1;
            signal ~GENSYM[wr][5]  : integer range 0 to ~LENGTH[~TYP[#{arg3}]] - 1;
          begin
            ~SYM[4] <= to_integer(~VAR[rdI][#{arg4}](31 downto 0))
            -- pragma translate_off
                          mod ~LENGTH[~TYP[#{arg3}]]
            -- pragma translate_on
                          ;

            ~SYM[5] <= to_integer(~VAR[wrI][#{arg6}](31 downto 0))
            -- pragma translate_off
                          mod ~LENGTH[~TYP[#{arg3}]]
            -- pragma translate_on
                          ;
            ~SYM[6] : process(~ARG[#{arg1}])
            begin
              if ~IF ~ACTIVEEDGE[Rising][#{arg1}] ~THENrising_edge~ELSEfalling_edge~FI(~ARG[#{arg1}]) then
                if ~ARG[#{arg5}]~IF ~ISACTIVEENABLE[#{arg2}] ~THEN and ~ARG[#{arg2}]~ELSE~FI then
                  ~SYM[2](~SYM[5]) <= ~ARG[#{arg7}];
                end if;
                ~RESULT <= ~SYM[2](~SYM[4]);
              end if;
            end process;
          end block;
          -- blockRamBlob end
    |]) #-}

-- | Create a 'MemBlob' binding from a list of values
--
-- Since this uses Template Haskell, nothing in the arguments given to
-- 'createMemBlob' can refer to something defined in the same module.
--
-- === __Example__
--
-- @
-- 'createMemBlob' "content" 'Nothing' [15 :: Unsigned 8 .. 17]
--
-- ram clk en = 'blockRamBlob' clk en content
-- @
--
-- The 'Data.Maybe.Maybe' datatype has don't care bits, where the actual value
-- does not matter. But the bits need a defined value in the memory. Either 0 or
-- 1 can be used, and both are valid representations of the data.
--
-- >>> import qualified Prelude as P
-- >>> let es = [ Nothing, Just (7 :: Unsigned 8), Just 8 ]
-- >>> :{
-- createMemBlob "content0" (Just 0) es
-- createMemBlob "content1" (Just 1) es
-- x = 1
-- :}
--
-- >>> let pr = mapM_ (putStrLn . show)
-- >>> pr $ P.map pack es
-- 0b0_...._....
-- 0b1_0000_0111
-- 0b1_0000_1000
-- >>> pr $ unpackMemBlob content0
-- 0b0_0000_0000
-- 0b1_0000_0111
-- 0b1_0000_1000
-- >>> pr $ unpackMemBlob content1
-- 0b0_1111_1111
-- 0b1_0000_0111
-- 0b1_0000_1000
-- >>> :{
-- createMemBlob "contentN" Nothing es
-- x = 1
-- :}
-- <BLANKLINE>
-- <interactive>:...: error:...
--     packBVs: cannot convert don't care values. Please specify a mapping to a definite value.
--
-- Note how we hinted to @clashi@ that our multi-line command was a list of
-- declarations by including a dummy declaration @x = 1@. Without this trick,
-- @clashi@ would expect an expression and the Template Haskell would not work.
createMemBlob
  :: forall a f
   . ( Foldable f
     , BitPack a
     )
  => String
  -- ^ Name of the binding to generate
  -> Maybe Bit
  -- ^ Value to map don't care bits to. 'Nothing' means throwing an error on
  -- don't care bits.
  -> f a
  -- ^ The content for the 'MemBlob'
  -> DecsQ
createMemBlob name care es =
  case packed of
    Left err -> fail err
    Right _ -> sequence
      [ sigD name0 [t| MemBlob $(n) $(m) |]
      , valD (varP name0) (normalB [| MemBlob { memBlobRunsLen = $(runsLen)
                                              , memBlobRuns = $(runs)
                                              , memBlobEndsLen = $(endsLen)
                                              , memBlobEnds = $(ends)
                                              } |]) []
      ]
 where
  name0 = mkName name
  n = litT . numTyLit . toInteger $ len
  m = litT . numTyLit $ natToInteger @(BitSize a)
  runsLen = litE . integerL . toInteger $ L.length runsB
  runs = litE . stringPrimL $ L.unpack runsB
  endsLen = litE . integerL . toInteger $ L.length endsB
  ends = litE . stringPrimL $ L.unpack endsB
  (len, runsB, endsB) = either error id packed
  packed = packBVs care es

-- | Create a 'MemBlob' from a list of values
--
-- Since this uses Template Haskell, nothing in the arguments given to
-- 'memBlobTH' can refer to something defined in the same module.
--
-- === __Example__
--
-- @
-- ram clk en = 'blockRamBlob' clk en $(memBlobTH Nothing [15 :: Unsigned 8 .. 17])
-- @
--
-- The 'Data.Maybe.Maybe' datatype has don't care bits, where the actual value
-- does not matter. But the bits need a defined value in the memory. Either 0 or
-- 1 can be used, and both are valid representations of the data.
--
-- >>> import qualified Prelude as P
-- >>> let es = [ Nothing, Just (7 :: Unsigned 8), Just 8 ]
-- >>> content0 = $(memBlobTH (Just 0) es)
-- >>> content1 = $(memBlobTH (Just 1) es)
-- >>> let pr = mapM_ (putStrLn . show)
-- >>> pr $ P.map pack es
-- 0b0_...._....
-- 0b1_0000_0111
-- 0b1_0000_1000
-- >>> pr $ unpackMemBlob content0
-- 0b0_0000_0000
-- 0b1_0000_0111
-- 0b1_0000_1000
-- >>> pr $ unpackMemBlob content1
-- 0b0_1111_1111
-- 0b1_0000_0111
-- 0b1_0000_1000
-- >>> $(memBlobTH Nothing es)
-- <BLANKLINE>
-- <interactive>:...: error:...
--     • packBVs: cannot convert don't care values. Please specify a mapping to a definite value.
--     • In the untyped splice: $(memBlobTH Nothing es)
memBlobTH
  :: forall a f
   . ( Foldable f
     , BitPack a
     )
  => Maybe Bit
  -- ^ Value to map don't care bits to. 'Nothing' means throwing an error on
  -- don't care bits.
  -> f a
  -- ^ The content for the 'MemBlob'
  -> ExpQ
memBlobTH care es =
  case packed of
    Left err -> fail err
    Right _ -> [| MemBlob { memBlobRunsLen = $(runsLen)
                          , memBlobRuns = $(runs)
                          , memBlobEndsLen = $(endsLen)
                          , memBlobEnds = $(ends)
                          }
                    :: MemBlob $(n) $(m) |]
 where
  n = litT . numTyLit . toInteger $ len
  m = litT . numTyLit $ natToInteger @(BitSize a)
  runsLen = litE . integerL . toInteger $ L.length runsB
  runs = litE . stringPrimL $ L.unpack runsB
  endsLen = litE . integerL . toInteger $ L.length endsB
  ends = litE . stringPrimL $ L.unpack endsB
  (len, runsB, endsB) = either error id packed
  packed = packBVs care es
