{-# OPTIONS_GHC -O0 #-}

module T3232 where

import Clash.Prelude
import Data.Proxy

import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified Prelude as P

topEntity :: Int -> Proxy 32 -> Proxy 15 -> Unsigned 32 -> Unsigned 15
topEntity = inner
{-# OPAQUE topEntity #-}

myCmpNat ::
  forall (a :: Nat) (b :: Nat).
  (KnownNat a, KnownNat b) =>
  Int ->
  Proxy a ->
  Proxy b ->
  OrderingI a b
myCmpNat _ p1 p2 = cmpNat p1 p2
{-# OPAQUE myCmpNat #-}

inner
  :: forall inBits outBits
   . (KnownNat inBits, KnownNat outBits)
  => Int
  -> Proxy inBits
  -> Proxy outBits
  -> Unsigned inBits
  -> Unsigned outBits
inner n sInBits sOutBits _ =
  case myCmpNat n sInBits sOutBits of
    LTI -> 123
    EQI -> 456
    GTI -> 789

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- P.readFile (topDir </> P.show 'topEntity </> "topEntity.v")
  -- With both LTI and EQI branches recognized as absurd, the case scrutinee
  -- should be eliminated entirely and the result should be the constant 789.
  let assertContains needle =
        if needle `isInfixOf` content
          then pure ()
          else P.error $ "Expected Verilog to contain " <> P.show needle
                      <> ", got:\n" <> content
  let assertAbsent needle =
        if needle `isInfixOf` content
          then P.error $ "Did not expect Verilog to contain " <> P.show needle
                      <> ":\n" <> content
          else pure ()
  assertContains "15'd789"
  assertAbsent "15'd123"
  assertAbsent "15'd456"
  assertAbsent "myCmpNat"
