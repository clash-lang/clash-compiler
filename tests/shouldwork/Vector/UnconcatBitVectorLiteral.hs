-- Test that `unconcatBitVector#` applied to a literal BitVector is fully
-- reduced at compile time: neither `unconcatBitVector#` itself nor any
-- residual `split#` call may survive into the netlist.
--
-- The result is consumed by `map`, whose pattern match makes each
-- `unconcatBitVector#` application a case subject; the evaluator only
-- reduces the primitive in subject position.

module UnconcatBitVectorLiteral where

import qualified Data.Text as T

import Clash.Netlist.Types (Component(..), Declaration(..))
import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

import Clash.Prelude
import qualified Clash.Sized.Vector as V

topEntity :: Vec 4 (BitVector 8)
topEntity = map complement (V.unconcatBitVector# 0xDEADBEEF)

testPath :: FilePath
testPath = "tests/shouldwork/Vector/UnconcatBitVectorLiteral.hs"

assertFullyReduced :: Component -> IO ()
assertFullyReduced = mapM_ checkDecl . declarations
 where
  checkDecl (BlackBoxD primName _ _ _ _ _)
    | primName `elem` mustReduce =
        error $ "Found unreduced primitive: " <> show primName
  checkDecl _ = return ()

  mustReduce = T.pack <$>
    [ "Clash.Sized.Vector.unconcatBitVector#"
    , "Clash.Sized.Internal.BitVector.split#"
    ]

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  mapM_ (assertFullyReduced . snd) netlist
