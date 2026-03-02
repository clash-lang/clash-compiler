{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module T1439 where

import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
import Clash.Prelude
import Clash.Sized.Internal.BitVector

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

topEntity :: BitVector 32 -> BitVector 32
topEntity = rotate_right True

rotate_right ::
  forall n.
  (KnownNat n, 1 <= n) =>
  -- | Client request
  Bool ->
  -- | Object of shift operation
  BitVector n ->
  -- | Result.
  BitVector n
rotate_right bv =
  leToPlus @1 @n (rotate_right' bv)
{-# OPAQUE rotate_right #-}

rotate_right' ::
  forall n.
  (KnownNat n) =>
  -- | Client request
  Bool ->
  -- | Object of shift in operation
  BitVector (n + 1) ->
  -- | Result.
  BitVector (n + 1)
rotate_right' bool bv
  | bool = pack b ++# (fst $ split # bv)
  | otherwise = bv
 where
  b = lsb bv
{-# OPAQUE rotate_right' #-}

testPath :: FilePath
testPath = "tests/shouldwork/Issues/T1439.hs"

noRotateRight :: Component -> IO ()
noRotateRight (Component nm _ _ _)
  | Id.toText nm == "rotate_right" = error ("No component should be called rotate_right")
  | otherwise = pure ()

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (noRotateRight . snd) netlist
