import CLaSH.Driver
import CLaSH.Rewrite.Types
import CLaSH.GHC.GenerateBindings
import CLaSH.Primitives.Util

genVHDL :: String
        -> IO ()
genVHDL src = do
  primMap <- generatePrimMap ["./primitives"]
  (bindingsMap,dfunMap,clsOpMap) <- generateBindings primMap src
  generateVHDL bindingsMap clsOpMap dfunMap primMap DebugFinal
