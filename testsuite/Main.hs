module Main (main) where

import Test.Tasty
import Test.Tasty.Program

import qualified Data.List        as List
import           Data.Maybe       (fromMaybe)
import qualified System.Directory as Directory
import           System.FilePath  ((</>),(<.>))
import qualified System.IO.Unsafe as Unsafe

data BuildTarget
  = VHDL | Verilog | Both
  deriving Show

main :: IO ()
main =
  defaultMain $ testGroup "tests"
    [ testGroup "examples"
      [runTest "examples"             Both [] "ALU"          (Just ("topEntity",False))
      ,runTest "examples"             VHDL [] "Blinker"      (Just ("topEntity_0",False))
      ,runTest "examples"             Verilog [] "Blinker_v" (Just ("topEntity_0",False))
      ,runTest "examples"             Both [] "BlockRamTest" (Just ("topEntity",False))
      ,runTest "examples"             Both [] "Calculator"   (Just ("testbench",True ))
      ,runTest "examples"             Both [] "CochleaPlus"  (Just ("topEntity",False))
      ,runTest "examples"             Both [] "FIR"          (Just ("testbench",True ))
      ,runTest "examples"             Both [] "Fifo"         (Just ("topEntity",False))
      ,runTest "examples"             Both [] "MAC"          (Just ("testbench",True))
      ,runTest "examples"             Both [] "MatrixVect"   (Just ("testbench",True))
      ,runTest "examples"             Both [] "Queens"       (Just ("topEntity",False))
      ,runTest "examples"             Both [] "Reducer"      (Just ("topEntity",False))
      ,runTest "examples"             Both [] "Sprockell"    (Just ("topEntity",False))
      ,runTest "examples"             Both [] "Windows"      (Just ("topEntity",False))
      ,runTest ("examples" </> "i2c") Both [] "I2C"          (Just ("topEntity",False))
      ]
    , testGroup "unit-tests"
        [ testGroup "Basic"
            [ runTest ("tests" </> "shouldwork" </> "Basic") Both [] "ClassOps" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "IrrefError" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "NestedPrimitives" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "NestedPrimitives2" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "PatError" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "RecordSumOfProducts" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "Shift" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "SimpleConstructor" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "TagToEnum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "TwoFunctions" (Just ("topEntity",False))
            ]
        , testGroup "BitVector"
            [ runTest ("tests" </> "shouldwork" </> "BitVector") Both [] "Box" (Just ("topEntity",False))
            ]
        , testGroup "BoxedFunctions"
            [ runTest ("tests" </> "shouldwork" </> "BoxedFunctions") Both [] "DeadRecursiveBoxed" (Just ("topEntity",False))
            ]
        , testGroup "CSignal"
            [ runTest ("tests" </> "shouldwork" </> "CSignal") Both [] "CBlockRamTest" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "CSignal") Both [] "MAC" (Just ("topEntity",False))
            ]
        , testGroup "Fixed"
            [ runTest ("tests" </> "shouldwork" </> "Fixed") Both [] "SFixedTest" (Just ("testbench",True))
            ]
        , testGroup "Numbers"
            [ runTest ("tests" </> "shouldwork" </> "Numbers") Both [] "Resize" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") Both [] "Resize2" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") Both [] "SatMult" (Just ("topEntity",False))
            ]
        , testGroup "Polymorphism"
            [ runTest ("tests" </> "shouldwork" </> "Polymorphism") Both [] "ExistentialBoxed" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Polymorphism") Both [] "LocalPoly" (Just ("topEntity",False))
            ]
        , testGroup "Signal"
            [ runTest ("tests" </> "shouldwork" </> "Signal") Both [] "AlwaysHigh" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Both [] "BlockRamTest" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Both [] "MAC" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Both [] "SigP" (Just ("topEntity",False))
            ]
        , testGroup "Testbench"
            [ runTest ("tests" </> "shouldwork" </> "Testbench") Both ["-clash-inline-limit=0"] "TB" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Testbench") Both [] "SyncTB" (Just ("testbench",True))
            ]
        , testGroup "Vector"
            [ runTest ("tests" </> "shouldwork" </> "Vector") Both [] "EnumTypes" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "HOClock" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "HOCon" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "HOPrim" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "PatHOCon" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "Split" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VACC" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VIndex" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VMapAccum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VReplace" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VScan" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VZip" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VecConst" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VecOfSum" (Just ("topEntity",False))
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

runTest env Both extraArgs modName entNameM = testGroup "VHDL & Verilog"
  [ runTest env VHDL extraArgs modName entNameM
  , runTest env Verilog extraArgs modName entNameM
  ]
