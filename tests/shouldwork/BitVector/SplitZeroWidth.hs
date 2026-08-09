-- Regression test: a split half whose width reduces to zero must be just as
-- foldable as a literal BitVector 0. Keep the functions polymorphic until Clash
-- specializes them, so the split arguments still contain Mod 8 8.
--
-- Simulation cannot detect this regression: the leftover multiplexer still
-- produces the correct value. Check the netlist for complete constant folding.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}

module SplitZeroWidth where

import Clash.Prelude
import qualified Clash.Sized.Internal.BitVector as BV
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types (Component(..), Declaration(..))
import qualified Prelude as P

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

leftZero
  :: forall n
   . KnownNat n
  => BitVector (Mod n 8 + n)
  -> Unsigned 8
leftZero bv =
  case BV.fromEnum# (fst (BV.split# @n @(Mod n 8) bv) BV.++# (0xF0 :: BitVector 8)) of
    240 -> 1
    _ -> 2
{-# OPAQUE leftZero #-}

rightZero
  :: forall n
   . KnownNat n
  => BitVector (n + Mod n 8)
  -> Unsigned 8
rightZero bv =
  case BV.fromEnum# (snd (BV.split# @(Mod n 8) @n bv) BV.++# (0xF0 :: BitVector 8)) of
    240 -> 1
    _ -> 2
{-# OPAQUE rightZero #-}

topEntity :: BitVector 8 -> (Unsigned 8, Unsigned 8)
topEntity bv = (leftZero @8 bv, rightZero @8 bv)
{-# OPAQUE topEntity #-}

testPath :: FilePath
testPath = "tests/shouldwork/BitVector/SplitZeroWidth.hs"

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  case [Id.toText (componentName c) | (_, c) <- netlist, P.any isMux (declarations c)] of
    [] -> pure ()
    names -> error $
      "Expected computed zero-width splits to fold completely; multiplexers remain in "
        <> show names
 where
  isMux CondAssignment{} = True
  isMux _ = False
