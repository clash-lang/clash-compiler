{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Program

import qualified Data.List        as List
import           Data.Maybe       (fromMaybe)
import qualified System.Directory as Directory
import           System.FilePath  ((</>),(<.>))
import qualified System.IO.Unsafe as Unsafe

data BuildTarget
  = VHDL | Verilog | SystemVerilog | Both | All
  deriving Show

defBuild :: BuildTarget
#ifdef TRAVISBUILD
defBuild = Both
#else
defBuild = All
#endif

main :: IO ()
main =
  defaultMain $ testGroup "tests"
    [ testGroup "examples"
      [runTest "examples"             defBuild [] "ALU"          (Just ("topEntity",False))
      ,runTest "examples"             VHDL     [] "Blinker"      (Just ("topEntity",False))
      ,runTest "examples"             defBuild [] "BlockRamTest" (Just ("topEntity",False))
      ,runTest "examples"             defBuild [] "Calculator"   (Just ("testBench",True ))
      ,runTest "examples"             defBuild [] "CochleaPlus"  (Just ("topEntity",False)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest "examples"             defBuild [] "DDR"          (Just ("testBench",True ))
      ,runTest "examples"             defBuild [] "FIR"          (Just ("testBench",True )) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest "examples"             defBuild [] "Fifo"         (Just ("topEntity",False)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest "examples"             defBuild [] "MAC"          (Just ("testBench",True))
      ,runTest "examples"             defBuild [] "MatrixVect"   (Just ("testBench",True))
      ,runTest "examples"             defBuild [] "Queens"       (Just ("topEntity",False)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest "examples"             defBuild [] "Reducer"      (Just ("topEntity",False))
      ,runTest "examples"             defBuild [] "Sprockell"    (Just ("topEntity",False))
      ,runTest "examples"             defBuild [] "Windows"      (Just ("topEntity",False)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest ("examples" </> "crc32") defBuild [] "CRC32"      (Just ("testBench",True))  -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest ("examples" </> "i2c") defBuild ["-O2"] "I2C"     (Just ("i2c"      ,False))
      ]
    , testGroup "unit-tests"
        [ testGroup "Basic"
            [ runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "BangData" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Trace" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "ByteSwap32" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "CharTest" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "ClassOps" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "CountTrailingZeros" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "DivMod" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "IrrefError" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "LambdaDrop" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "LotOfStates" (Just ("testBench",True)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NestedPrimitives" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NestedPrimitives2" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NORX" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "PatError" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "PopCount" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "RecordSumOfProducts" (Just ("topEntity",False)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Shift" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "SimpleConstructor" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TagToEnum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TestIndex" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TwoFunctions" (Just ("topEntity",False))
            ]
        , testGroup "BitVector"
            [ runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "Box" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "BoxGrow" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "RePack" (Just ("topEntity",False))
            ]
        , testGroup "BoxedFunctions"
            [ runTest ("tests" </> "shouldwork" </> "BoxedFunctions") defBuild [] "DeadRecursiveBoxed" (Just ("topEntity",False))
            ]
        , testGroup "CSignal"
            [ runTest ("tests" </> "shouldwork" </> "CSignal") defBuild [] "CBlockRamTest" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "CSignal") defBuild [] "MAC" (Just ("topEntity",False))
            ]
        , testGroup "Feedback" -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            [ runTest ("tests" </> "shouldwork" </> "Feedback") defBuild [] "Fib" (Just ("testBench",True))
            ]
        , testGroup "Fixed"
            [ runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "Mixer" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "SFixedTest" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "SatWrap" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "ZeroInt" (Just ("testBench",True))
            ]
        , testGroup "Floating"
            [ runTest ("tests" </> "shouldwork" </> "Floating") defBuild ["-clash-float-support"] "FloatPack" (Just ("topEntity",False))
            ]
        , testGroup "HOPrim"
            [ runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "HOImap" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "TestMap" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "Transpose" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "VecFun" (Just ("testBench",True))
            ]
        , testGroup "Numbers"
            [ runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Bounds"  (Just ("testBench",True)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize"  (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize2" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "SatMult" (Just ("topEntity",False))
            ]
        , testGroup "Polymorphism"
            [ runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "ExistentialBoxed" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "LocalPoly" (Just ("topEntity",False))
            ]
        , testGroup "RTree"
            [ runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TFold" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TZip" (Just ("topEntity",False))
            ]
        , testGroup "Signal"
            [ runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "AlwaysHigh" (Just ("topEntity",False)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamFile" (Just ("testBench",True)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamTest" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "MAC" (Just ("topEntity",False)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "NoCPR" (Just ("example",False))
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Ram" (Just ("testBench",True)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Rom" (Just ("testBench",True)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RomFile" (Just ("testBench",True)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "SigP" (Just ("topEntity",False)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            ]
        , testGroup "Testbench" -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/1152
            [ runTest ("tests" </> "shouldwork" </> "Testbench") defBuild ["-clash-inline-limit=0"] "TB" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Testbench") defBuild [] "SyncTB" (Just ("testBench",True))
            ]
        , testGroup "Vector"
            [ runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Concat" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DFold" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DFold2" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DTFold" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "EnumTypes" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FindIndex" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Fold" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Foldr" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOClock" (Just ("topEntity",False)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/1152
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOCon" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOPrim" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Minimum" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "MovingAvg" (Just ("topEntity", False)) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/1152
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "PatHOCon" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Scatter" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Split" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "ToList" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Unconcat" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VACC" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VIndex" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VFold" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VMapAccum" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VMerge" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VReplace" (Just ("testBench",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VReverse" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VRotate" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VScan" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VSelect" (Just ("testBench",True))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VZip" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VecConst" (Just ("topEntity",False))
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VecOfSum" (Just ("topEntity",False))
            ]
        ]
    ]

clashHDL :: BuildTarget -> FilePath -> [String] -> String -> TestTree
clashHDL t env extraArgs modName =
  testProgram ("CLaSH(" ++ show t ++ ")")
              "stack"
              (concat [["exec","clash","--"
                       ,case t of { VHDL -> "--vhdl"
                                  ; Verilog -> "--verilog"
                                  ; SystemVerilog -> "--systemverilog"
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
ghdlSim env modName = testProgram "GHDL (sim)" "ghdl" ["-r","--workdir=work","--std=93",modName ++ "_testBench","--assert-level=error"] (Just env) False False

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
vvp env modName = testProgram "vvp" "vvp" [modName ++ "_testBench"] (Just env) False True

vlog :: FilePath -> String -> TestTree
vlog dir modName = testGroup "vlog"
  [ testProgram "vlib" "vlib" ["work"] (Just dir) False False
  , testProgram "vlog" "vlog" ["-sv","-work","work",modName ++ "_types.sv","*.sv"] (Just dir) False False
  ]

vsim :: FilePath -> String -> String -> TestTree
vsim dir modName entName =
  testProgram "vsim" "vsim"
    ["-batch","-do",doScript,modName ++ "_" ++ entName] (Just dir) False False
  where
    doScript = List.intercalate ";"
      [ "run -all"
      , unwords
         ["if {[string equal ready [runStatus]]}"
         ,"then {quit -f}"
         ,"else {quit -code 1 -f}"
         ]
      , "quit -code 2 -f"
      ]

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

runTest env SystemVerilog extraArgs modName entNameM =
    withResource (return ()) release (const grp)
  where
    svDir     = env </> "systemverilog"
    modDir    = svDir </> modName
    release _ = Directory.removeDirectoryRecursive svDir

    grp = testGroup modName
            ( clashHDL SystemVerilog env extraArgs modName
            : maybe [] doMakeAndRun entNameM
            )

    doMakeAndRun (entName,doRun) =
      vlog modDir modName : if doRun then [vsim modDir modName entName] else []

runTest env Both extraArgs modName entNameM = testGroup "VHDL & Verilog"
  [ runTest env VHDL extraArgs modName entNameM
  , runTest env Verilog extraArgs modName entNameM
  ]

runTest env All extraArgs modName entNameM = testGroup "VHDL & Verilog & SystemVerilog"
  [ runTest env VHDL extraArgs modName entNameM
  , runTest env Verilog extraArgs modName entNameM
  , runTest env SystemVerilog extraArgs modName entNameM
  ]
