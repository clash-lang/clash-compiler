import CLaSH.Driver
import CLaSH.Rewrite.Types
import CLaSH.GHC.Evaluator
import CLaSH.GHC.GenerateBindings
import CLaSH.GHC.NetlistTypes
import CLaSH.Primitives.Util
import CLaSH.Backend
import CLaSH.Backend.VHDL

genVHDL :: String
        -> IO ()
genVHDL = doHDL (initBackend :: VHDLState)


doHDL :: Backend s
       => s
       -> String
       -> IO ()
doHDL b src = do
  primMap <- generatePrimMap ["./clash-ghc/primitives"]
  (bindingsMap,tcm) <- generateBindings primMap src Nothing
  generateHDL bindingsMap (Just b) primMap tcm ghcTypeToHWType reduceConstant DebugFinal

main :: IO ()
main = genVHDL "./examples/CalculatorArrow.hs"
