-- Test that noops are collapsed to their argument as specified by `WorkIdentity`

module T779 where

import Clash.Netlist.Types (Component(..),Declaration(..))
import qualified Data.Text as T
import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

import Clash.Prelude

topEntity :: Vec 4 Bit -> BitVector 4 -> (Vec 4 Bit,BitVector 4)
topEntity a b = (vecRoundTrip a,bvRoundTrip b)
  where vecRoundTrip = bv2v . v2bv
        bvRoundTrip  = v2bv . bv2v

testPath :: FilePath
testPath = "tests/shouldwork/Issues/T779.hs"

assertAllCollpased :: Component -> IO ()
assertAllCollpased = mapM_ checkCollapse . declarations
  where
    checkCollapse (BlackBoxD primName _ _ _ _ _)
      | primName `elem` toCollapse = error $ "Found uncollapsed noops: " <> show primName
    checkCollapse _                = return ()

    toCollapse = T.pack <$> ["Clash.Sized.Vector.map"
                            ,"Clash.Sized.Internal.BitVector.pack#"
                            ,"Clash.Sized.Internal.BitVector.unpack#"]
mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  mapM_ assertAllCollpased netlist
