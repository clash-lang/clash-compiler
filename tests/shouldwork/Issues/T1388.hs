{-# LANGUAGE CPP #-}

module T1388 where

import Clash.Prelude hiding (Word)

import Clash.Netlist.Types
import Data.Maybe
import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

newtype Byte = Byte
  { _byte :: BitVector 8}
  deriving (Generic, NFDataX, Show, Eq)

newtype Word = Word
  { _word :: BitVector 16}
  deriving (Generic, NFDataX, Show, Eq)

type Bytes n = Vec n (Byte)

type Words n = Vec n (Word)

newtype TypeA = TypeA (Bytes 4)
  deriving (Generic, NFDataX, Show, Eq)

newtype TypeB = TypeB (Words 4)
  deriving (Generic, NFDataX, Show, Eq)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE bytesToWords #-}
bytesToWords :: Bytes 4 -> TypeB
bytesToWords = TypeB . fmap (\(Byte a) -> Word $ ((unpack . resize .  pack) a))

data TypeAs = Nop | TypeAS TypeA

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE convertTwoTypeAs #-}
convertTwoTypeAs :: TypeAs -> Maybe TypeB
convertTwoTypeAs op = case op of
  TypeAS (TypeA a) -> Just $ (bytesToWords a)
  Nop -> Nothing

topEntity :: TypeAs -> Maybe TypeB
topEntity = convertTwoTypeAs

-- Test

testPath :: FilePath
testPath = "tests/shouldwork/Issues/T1388.hs"

-- TODO: When toSimpleVar is made more liberal later, this should be changed
-- to check that only simple modifiers are used on ports.
--
assertNoSLVInPortMap :: Component -> IO ()
assertNoSLVInPortMap =
  mapM_ goDecl . declarations
 where
  goDecl :: Declaration -> IO ()
  goDecl (InstDecl _ _ _ _ _ _ (NamedPortMap ps))
    | all goPort ps = pure ()
    | otherwise = error ("Not all ports have simple modifiers: " <> show ps)
  goDecl _ = pure ()

  goPort (_, _, _, Identifier _ m)
    | isNothing m = True
  goPort p = False


mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertNoSLVInPortMap . snd) netlist
