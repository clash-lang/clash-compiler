import CLaSH.Driver
import CLaSH.Rewrite.Types
import CLaSH.GHC.GenerateBindings
import CLaSH.GHC.NetlistTypes
import CLaSH.Primitives.Util

genVHDL :: String
        -> IO ()
genVHDL src = do
  primMap <- generatePrimMap ["./primitives"]
  bindingsMap <- generateBindings primMap src
  generateVHDL bindingsMap primMap ghcTypeToHWType DebugFinal
