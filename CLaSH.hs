import CLaSH.Driver
import CLaSH.Rewrite.Types
import CLaSH.GHC.Evaluator
import CLaSH.GHC.GenerateBindings
import CLaSH.GHC.NetlistTypes
import CLaSH.Primitives.Util
import CLaSH.Backend
import CLaSH.Backend.VHDL
import CLaSH.Backend.SystemVerilog

genVHDL :: String
        -> IO ()
genVHDL = doHDL (initBackend :: VHDLState)

genSystemVerilog :: String
           -> IO ()
genSystemVerilog = doHDL (initBackend :: SystemVerilogState)

doHDL :: Backend s
       => s
       -> String
       -> IO ()
doHDL b src = do
  pd      <- primDir b
  primMap <- generatePrimMap [pd,"."]
  (bindingsMap,tcm,topEntM) <- generateBindings primMap src Nothing
  generateHDL bindingsMap (Just b) primMap tcm ghcTypeToHWType reduceConstant topEntM DebugNone

main :: IO ()
main = genVHDL "./examples/FIR.hs"
