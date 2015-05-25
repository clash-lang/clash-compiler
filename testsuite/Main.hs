module Main (main) where

import Test.Tasty
import Test.Tasty.Program

import qualified Data.List        as List
import           Data.Maybe       (fromMaybe)
import qualified System.Directory as Directory
import           System.FilePath  ((</>),(<.>))
import qualified System.IO.Unsafe as Unsafe

data BuildTarget
  = VHDL | Verilog
  deriving Show

main :: IO ()
main =
  defaultMain $ testGroup "tests"
    [ testGroup "examples"
      [runTest "examples"             Verilog [] "ALU"          (Just ("topEntity",False))
      ,runTest "examples"             Verilog [] "Blinker_v"    (Just ("topEntity_0",False))
      ,runTest "examples"             Verilog [] "BlockRamTest" (Just ("topEntity",False))
      ,runTest "examples"             Verilog [] "Calculator"   (Just ("testbench",True ))
      ,runTest "examples"             Verilog [] "CochleaPlus"  (Just ("topEntity",False))
      ,runTest "examples"             Verilog [] "FIR"          (Just ("testbench",True ))
      ,runTest "examples"             Verilog [] "Fifo"         (Just ("topEntity",False))
      ,runTest "examples"             Verilog [] "MAC"          (Just ("testbench",True))
      ,runTest "examples"             Verilog [] "MatrixVect"   (Just ("testbench",True))
      ,runTest "examples"             Verilog [] "Queens"       (Just ("topEntity",False))
      ,runTest "examples"             Verilog [] "Reducer"      (Just ("topEntity",False))
      ,runTest "examples"             Verilog [] "Sprockell"    (Just ("topEntity",False))
      ,runTest "examples"             Verilog [] "Windows"      (Just ("topEntity",False))
      ,runTest ("examples" </> "i2c") Verilog [] "I2C"          (Just ("topEntity",False))
      ]
    , testGroup "unit-tests"
        [ testGroup "Basic"
            [ runTest ("tests" </> "shouldwork" </> "Basic") Verilog [] "ClassOps" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Verilog [] "IrrefError" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Verilog [] "NestedPrimitives" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Verilog [] "NestedPrimitives2" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Verilog [] "PatError" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Verilog [] "RecordSumOfProducts" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Verilog [] "Shift" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Verilog [] "SimpleConstructor" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Verilog [] "TagToEnum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Verilog [] "TwoFunctions" (Just ("topEntity",False))
            ]
        , testGroup "BitVector"
            [ runTest ("tests" </> "shouldwork" </> "BitVector") Verilog [] "Box" (Just ("topEntity",False))
            ]
        , testGroup "BoxedFunctions"
            [ runTest ("tests" </> "shouldwork" </> "BoxedFunctions") Verilog [] "DeadRecursiveBoxed" (Just ("topEntity",False))
            ]
        , testGroup "CSignal"
            [ runTest ("tests" </> "shouldwork" </> "CSignal") Verilog [] "CBlockRamTest" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "CSignal") Verilog [] "MAC" (Just ("topEntity",False))
            ]
        , testGroup "Fixed"
            [ runTest ("tests" </> "shouldwork" </> "Fixed") Verilog [] "SFixedTest" (Just ("testbench",True))
            ]
        , testGroup "Numbers"
            [ runTest ("tests" </> "shouldwork" </> "Numbers") Verilog [] "Resize" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") Verilog [] "Resize2" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") Verilog [] "SatMult" (Just ("topEntity",False))
            ]
        , testGroup "Polymorphism"
            [ runTest ("tests" </> "shouldwork" </> "Polymorphism") Verilog [] "ExistentialBoxed" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Polymorphism") Verilog [] "LocalPoly" (Just ("topEntity",False))
            ]
        , testGroup "Signal"
            [ runTest ("tests" </> "shouldwork" </> "Signal") Verilog [] "AlwaysHigh" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Verilog [] "BlockRamTest" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Verilog [] "MAC" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Verilog [] "SigP" (Just ("topEntity",False))
            ]
        , testGroup "Testbench"
            [ runTest ("tests" </> "shouldwork" </> "Testbench") Verilog ["-clash-inline-limit=0"] "TB" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Testbench") Verilog [] "SyncTB" (Just ("testbench",True))
            ]
        , testGroup "Vector"
            [ runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "EnumTypes" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "HOClock" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "HOCon" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "HOPrim" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "PatHOCon" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "Split" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "VACC" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "VIndex" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "VMapAccum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "VReplace" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "VScan" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "VZip" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "VecConst" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Verilog [] "VecOfSum" (Just ("topEntity",False))
            ]
        ]
    ]

clashHDL :: BuildTarget -> FilePath -> [String] -> String -> TestTree
clashHDL t env extraArgs modName =
  testProgram ("CLaSH(" ++ show t ++ ")")
              "cabal"
              (concat [["exec","clash","--"
                       ,case t of { VHDL -> "--vhdl"
                                  ; Verilog -> "--verilog"
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

iverilog :: FilePath -> String -> TestTree
iverilog dir entity = withResource (return dir) (const (return ()))
    (\d -> testProgram "iverilog" "iverilog" ("-g2005":"-s":entity:"-o":entity:verilogFiles d) (Just dir) False)
  where
    verilogFiles :: IO FilePath -> [FilePath]
    verilogFiles d =  Unsafe.unsafePerformIO
                   $  filter (List.isSuffixOf "v")
                  <$> (Directory.getDirectoryContents =<< d)

vvp :: FilePath -> TestTree
vvp env = testProgram "vvp" "vvp" ["-N","testbench"] (Just env) False

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

runTest env Verilog extraArgs modName entNameM =
    withResource (return ()) release (const grp)
  where
    verilogDir = env </> "verilog"
    modDir     = verilogDir </> modName
    release _  = Directory.removeDirectoryRecursive verilogDir


    grp        = testGroup modName
                    ( clashHDL Verilog env extraArgs modName
                    : maybe [] doMakeAndRun entNameM
                    )

    doMakeAndRun (entName,doRun) = [ iverilog modDir entName
                                   ] ++ if doRun then [vvp modDir] else []
