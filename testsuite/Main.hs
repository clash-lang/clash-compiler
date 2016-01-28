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
      ,runTest "examples"             Verilog [] "Blinker_v" (Just ("blinker_0",False))
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
            [ runTest ("tests" </> "shouldwork" </> "Basic") Both [] "Trace" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "ByteSwap32" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "CharTest" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "ClassOps" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "CountTrailingZeros" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "IrrefError" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "NestedPrimitives" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "NestedPrimitives2" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "PatError" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "PopCount" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "RecordSumOfProducts" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "Shift" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "SimpleConstructor" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "TagToEnum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "TestIndex" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") Both [] "TwoFunctions" (Just ("topEntity",False))
            ]
        , testGroup "BitVector"
            [ runTest ("tests" </> "shouldwork" </> "BitVector") Both [] "Box" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "BitVector") Both [] "BoxGrow" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "BitVector") Both [] "RePack" (Just ("topEntity",False))
            ]
        , testGroup "BoxedFunctions"
            [ runTest ("tests" </> "shouldwork" </> "BoxedFunctions") Both [] "DeadRecursiveBoxed" (Just ("topEntity",False))
            ]
        , testGroup "CSignal"
            [ runTest ("tests" </> "shouldwork" </> "CSignal") Both [] "CBlockRamTest" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "CSignal") Both [] "MAC" (Just ("topEntity",False))
            ]
        , testGroup "Feedback"
            [ runTest ("tests" </> "shouldwork" </> "Feedback") Both [] "Fib" (Just ("testbench",True))
            ]
        , testGroup "Fixed"
            [ runTest ("tests" </> "shouldwork" </> "Fixed") Both [] "SFixedTest" (Just ("testbench",True))
            ]
        , testGroup "HOPrim"
            [ runTest ("tests" </> "shouldwork" </> "HOPrim") Both [] "TestMap" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "HOPrim") Both [] "Transpose" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "HOPrim") Both [] "VecFun" (Just ("testbench",True))
            ]
        , testGroup "Numbers"
            [ runTest ("tests" </> "shouldwork" </> "Numbers") Both [] "Bounds"  (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") Both [] "Resize"  (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") Both [] "Resize2" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") Both [] "SatMult" (Just ("topEntity",False))
            ]
        , testGroup "Polymorphism"
            [ runTest ("tests" </> "shouldwork" </> "Polymorphism") Both [] "ExistentialBoxed" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Polymorphism") Both [] "LocalPoly" (Just ("topEntity",False))
            ]
        , testGroup "Signal"
            [ runTest ("tests" </> "shouldwork" </> "Signal") Both [] "AlwaysHigh" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Both [] "BlockRamFile" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Signal") Both [] "BlockRamTest" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Both [] "MAC" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Both [] "SigP" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Both [] "Ram" (Just ("testbench",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") Both [] "RomFile" (Just ("testbench",False))
            ]
        , testGroup "Testbench"
            [ runTest ("tests" </> "shouldwork" </> "Testbench") Both ["-clash-inline-limit=0"] "TB" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Testbench") Both [] "SyncTB" (Just ("testbench",True))
            ]
        , testGroup "Vector"
            [ runTest ("tests" </> "shouldwork" </> "Vector") Both [] "EnumTypes" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "DFold" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "DFold2" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "FindIndex" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "Fold" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "Foldr" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "HOClock" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "HOCon" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "HOPrim" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "Minimum" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "PatHOCon" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "Split" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "ToList" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VACC" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VIndex" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VecConst" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VecOfSum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VFold" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VMapAccum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VMerge" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VReplace" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VReverse" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VScan" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VSelect" (Just ("testbench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") Both [] "VZip" (Just ("topEntity",False))
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
              False

ghdlImport :: FilePath -> TestTree
ghdlImport dir = withResource (return dir) (const (return ()))
    (\d -> testProgram "GHDL (import)" "ghdl" ("-i":"--workdir=work":"--std=93":vhdlFiles d) (Just dir) False False)
  where
    vhdlFiles :: IO FilePath -> [FilePath]
    vhdlFiles d =  Unsafe.unsafePerformIO
                $  filter (List.isSuffixOf "vhdl")
               <$> (Directory.getDirectoryContents =<< d)

ghdlMake :: FilePath -> String -> String -> TestTree
ghdlMake env modName entity = testProgram "GHDL (make)" "ghdl" ["-m","--workdir=work","--std=93",modName ++ "_" ++ entity] (Just env) False False

ghdlSim :: FilePath -> String -> TestTree
ghdlSim env modName = testProgram "GHDL (sim)" "ghdl" ["-r","--std=93",modName ++ "_testbench","--assert-level=error"] (Just env) False False

iverilog :: FilePath -> String -> String -> TestTree
iverilog dir modName entity = withResource (return dir) (const (return ()))
    (\d -> testProgram "iverilog" "iverilog" ("-g2":"-s":modEntity:"-o":modEntity:verilogFiles d) (Just dir) False False)
  where
    verilogFiles :: IO FilePath -> [FilePath]
    verilogFiles d =  Unsafe.unsafePerformIO
                   $  filter (List.isSuffixOf "v")
                  <$> (Directory.getDirectoryContents =<< d)

    modEntity = modName ++ "_" ++ entity

vvp :: FilePath -> String -> TestTree
vvp env modName = testProgram "vvp" "vvp" [modName ++ "_testbench"] (Just env) False True

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
                                   , ghdlMake modDir modName entName
                                   ] ++ if doRun then [ghdlSim modDir modName] else []

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

    doMakeAndRun (entName,doRun) = [ iverilog modDir modName entName
                                   ] ++ if doRun then [vvp modDir modName] else []

runTest env Both extraArgs modName entNameM = testGroup "VHDL & Verilog"
  [ runTest env VHDL extraArgs modName entNameM
  , runTest env Verilog extraArgs modName entNameM
  ]
