module Main (main) where

import Test.Tasty
import Test.Tasty.Program

import qualified Data.List        as List
import           Data.Maybe       (fromMaybe)
import qualified System.Directory as Directory
import           System.FilePath  ((</>),(<.>))
import qualified System.IO.Unsafe as Unsafe

data BuildTarget
  = VHDL | SystemVerilog
  deriving Show

main :: IO ()
main =
  defaultMain $ testGroup "tests"
    [ testGroup "examples"
      [runTest "examples"             VHDL [] "ALU"          (Just ("topEntity",False))
      ,runTest "examples"             VHDL [] "Blinker"      (Just ("topEntity_0",False))
      ,runTest "examples"             VHDL [] "BlockRamTest" (Just ("topEntity",False))
      ,runTest "examples"             VHDL [] "Calculator"   (Just ("testbench",True ))
      ,runTest "examples"             VHDL [] "CochleaPlus"  (Just ("topEntity",False))
      ,runTest "examples"             VHDL [] "FIR"          (Just ("testbench",True ))
      ,runTest "examples"             VHDL [] "Fifo"         (Just ("topEntity",False))
      ,runTest "examples"             VHDL [] "MAC"          (Just ("testbench",True))
      ,runTest "examples"             VHDL [] "MatrixVect"   (Just ("testbench",True))
      ,runTest "examples"             VHDL [] "Queens"       (Just ("topEntity",False))
      ,runTest "examples"             VHDL [] "Reducer"      (Just ("topEntity",False))
      ,runTest "examples"             VHDL [] "Sprockell"    (Just ("topEntity",False))
      ,runTest "examples"             VHDL [] "Windows"      (Just ("topEntity",False))
      ,runTest ("examples" </> "i2c") VHDL [] "I2C"          (Just ("topEntity",False))
      ]
    , testGroup "unit-tests"
        [ testGroup "Basic"
            [ runTest ("tests" </> "shouldwork" </> "Basic") VHDL [] "ClassOps" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") VHDL [] "IrrefError" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") VHDL [] "NestedPrimitives" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") VHDL [] "NestedPrimitives2" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") VHDL [] "PatError" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") VHDL [] "RecordSumOfProducts" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") VHDL [] "Shift" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") VHDL [] "SimpleConstructor" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") VHDL [] "TagToEnum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") VHDL [] "TwoFunctions" (Just ("topEntity",False))
            ]
        , testGroup "BitVector"
            [ runTest ("tests" </> "shouldwork" </> "BitVector") VHDL [] "Box" (Just ("topEntity",False))
            ]
        , testGroup "BoxedFunctions"
            [ runTest ("tests" </> "shouldwork" </> "BoxedFunctions") VHDL [] "DeadRecursiveBoxed" (Just ("topEntity",False))
            ]
        , testGroup "CSignal"
            [ runTest ("tests" </> "shouldwork" </> "CSignal") VHDL [] "CBlockRamTest" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "CSignal") VHDL [] "MAC" (Just ("topEntity",False))
            ]
        , testGroup "Fixed"
            [ runTest ("tests" </> "shouldwork" </> "Fixed") VHDL [] "SFixedTest" (Just ("testbench",True))
            ]
        , testGroup "Numbers"
            [ runTest ("tests" </> "shouldwork" </> "Numbers") VHDL [] "Resize" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") VHDL [] "Resize2" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") VHDL [] "SatMult" (Just ("topEntity",False))
            ]
        , testGroup "Polymorphism"
            [ runTest ("tests" </> "shouldwork" </> "Polymorphism") VHDL [] "ExistentialBoxed" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Polymorphism") VHDL [] "LocalPoly" (Just ("topEntity",False))
            ]
        , testGroup "Signal"
            [ runTest ("tests" </> "shouldwork" </> "Signal") VHDL [] "AlwaysHigh" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") VHDL [] "BlockRamTest" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") VHDL [] "MAC" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") VHDL [] "SigP" (Just ("topEntity",False))
            ]
        , testGroup "Testbench"
            [ runTest ("tests" </> "shouldwork" </> "Testbench") VHDL ["-clash-inline-limit=0"] "TB" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Testbench") VHDL [] "SyncTB" (Just ("testbench",True))
            ]
        , testGroup "Vector"
            [ runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "EnumTypes" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "HOClock" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "HOCon" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "HOPrim" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "PatHOCon" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "Split" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "VACC" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "VIndex" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "VMapAccum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "VReplace" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "VScan" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "VZip" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "VecConst" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") VHDL [] "VecOfSum" (Just ("topEntity",False))
            ]
        ]
    ]

clashHDL :: BuildTarget -> FilePath -> [String] -> String -> TestTree
clashHDL t env extraArgs modName =
  testProgram ("CLaSH(" ++ show t ++ ")")
              "cabal"
              (concat [["exec","clash","--"
                       ,case t of { VHDL -> "--vhdl"
                                  ; SystemVerilog -> "--systemverilog"
                                  }
                       ]
                      ,extraArgs
                      ,[modName <.> "hs"]
                      ])
              (Just env)
              False

ghdlImport :: FilePath -> TestTree
ghdlImport dir = withResource (return dir) (const (return ()))
    (\d -> testProgram "GHDL (import)" "ghdl" ("-i":"--workdir=work":vhdlFiles d) (Just dir) False)
  where
    vhdlFiles :: IO FilePath -> [FilePath]
    vhdlFiles d =  Unsafe.unsafePerformIO
                $  filter (List.isSuffixOf "vhdl")
               <$> (Directory.getDirectoryContents =<< d)

ghdlMake :: String -> FilePath -> TestTree
ghdlMake entity env = testProgram "GHDL (make)" "ghdl" ["-m","--workdir=work",entity] (Just env) False

ghdlSim :: FilePath -> TestTree
ghdlSim env = testProgram "GHDL (sim)" "ghdl" ["-r","testbench","--assert-level=error"] (Just env) False

runTest :: FilePath
        -> BuildTarget
        -> [String]
        -> String
        -> Maybe (String,Bool)
        -> TestTree
runTest env VHDL extraArgs modName entNameM = withResource aquire release (const grp)
  where
    vhdlDir   = env </> "vhdl"
    modDir    = vhdlDir </> modName
    workdir   = modDir </> "work"
    aquire    = Directory.createDirectoryIfMissing True workdir
    release _ = Directory.removeDirectoryRecursive vhdlDir

    grp       = testGroup modName
                ( clashHDL VHDL env extraArgs modName
                : maybe [] doMakeAndRun entNameM
                )

    doMakeAndRun (entName,doRun) = [ ghdlImport modDir
                                   , ghdlMake entName modDir
                                   ] ++ if doRun then [ghdlSim modDir] else []

runTest env SystemVerilog extraArgs modName _ = clashHDL SystemVerilog env extraArgs modName
