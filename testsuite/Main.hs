{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Exception (finally)
import Data.List (isSuffixOf, (\\))
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Process (readCreateProcessWithExitCode, proc)
import GHC.Conc (numCapabilities)

import Test.Tasty
import Test.Tasty.Clash

import System.FilePath ((</>))
import System.Environment (setEnv)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))

defBuild :: [BuildTarget]
#ifdef DISABLE_SV_TESTS
defBuild = [VHDL, Verilog]
#else
defBuild = [VHDL, Verilog, SystemVerilog]
#endif

clashTestRoot
  :: [[TestName] -> TestTree]
  -> TestTree
clashTestRoot testTrees =
  clashTestGroup "Tests" testTrees []

-- | `clashTestGroup` and `clashTestRoot` make sure that each test knows its
-- fully qualified test name at construction time. This is used to create
-- dependency patterns.
clashTestGroup
  :: TestName
  -> [[TestName] -> TestTree]
  -> ([TestName] -> TestTree)
clashTestGroup testName testTrees =
  \parentNames ->
    testGroup testName $
      zipWith ($) testTrees (repeat (testName : parentNames))


runClashTest :: IO ()
runClashTest =
  defaultMain $ clashTestRoot
    [ clashTestGroup "Examples"
      [ runTest "examples"               defBuild []                                        "ALU"          ([""],"topEntity",False)
      , runTest "examples"               [VHDL]   []                                        "Blinker"      (["blinker"],"blinker",False)
      , runTest "examples"               defBuild []                                        "BlockRamTest" ([""],"topEntity",False)
      , runTest "examples"               defBuild []                                        "Calculator"   (["","testBench"],"testBench",True )
      , runTest "examples"               defBuild []                                        "CHIP8"        ([""],"topEntity", False)
      , runTest "examples"               defBuild []                                        "CochleaPlus"  ([""],"topEntity",False)
      , runTest "examples"               defBuild ["-fclash-component-prefix","test"]       "FIR"          (["","test_testBench"],"test_testBench",True )
      , runTest "examples"               defBuild []                                        "Fifo"         ([""],"topEntity",False)
      , runTest "examples"               defBuild []                                        "MAC"          (["","testBench"],"testBench",True)
      , runTest "examples"               defBuild []                                        "MatrixVect"   (["","testBench"],"testBench",True)
      , runTest "examples"               defBuild []                                        "Queens"       ([""],"topEntity",False)
      , runTest "examples"               defBuild []                                        "Reducer"      ([""],"topEntity",False)
      , runTest "examples"               defBuild []                                        "Sprockell"    ([""],"topEntity",False)
      , runTest "examples"               defBuild []                                        "Windows"      ([""],"topEntity",False)
      , runTest ("examples" </> "crc32") defBuild []                                        "CRC32"        (["","testBench"],"testBench",True)
      , runTest ("examples" </> "i2c")   defBuild ["-O2","-fclash-component-prefix","test"] "I2C"          (["test_i2c","test_bitmaster","test_bytemaster"],"test_i2c",False)
      ]
    , clashTestGroup "Unit"
      [ clashTestGroup "AutoReg"
        [ outputTest ("tests" </> "shouldwork" </> "AutoReg") defBuild [] [] "AutoReg" "main"
        ]
      , clashTestGroup "Basic"
        [ -- TODO: Enable AES test on SystemVerilog. See issue #569.
          runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "AES"                 ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "BangData"            ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Trace"               ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "ByteSwap32"          (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "CharTest"            (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "ClassOps"            (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "CountTrailingZeros"  (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "DeepseqX"            (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "DivMod"              ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "IrrefError"          ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "LambdaDrop"          ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "LotOfStates"         (["","testBench"],"testBench",True)
#ifdef CLASH_MULTIPLE_HIDDEN
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "MultipleHidden"      (["","testBench"],"testBench",True)
#endif
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NameInstance"         ([""],"topEntity",False)
        , outputTest ("tests" </> "shouldwork" </> "Basic") defBuild [] [] "NameInstance" "main"
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NameOverlap"         (["nameoverlap"],"nameoverlap",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NestedPrimitives"    ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NestedPrimitives2"   ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NORX"                (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") [VHDL]   [] "Parameters"          (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "PatError"            ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "PopCount"            (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "RecordSumOfProducts" ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Replace"             (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Shift"               ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "SimpleConstructor"   ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TagToEnum"           ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TestIndex"           ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Time"                (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TwoFunctions"        ([""],"topEntity",False)
        ]
        , clashTestGroup "ShouldFail"
          [ runFailingTest ("tests" </> "shouldfail") [VHDL] [] "RecursiveBoxed" (Just "Callgraph after normalisation contains following recursive components")
          , runFailingTest ("tests" </> "shouldfail") [VHDL] [] "RecursiveDatatype" (Just "Not in normal form: no Letrec")
          , runFailingTest ("tests" </> "shouldfail" </> "InvalidPrimitive") [VHDL] ["-itests/shouldfail/InvalidPrimitive"] "InvalidPrimitive" (Just "InvalidPrimitive.json")
          -- Disabled, due to it eating gigabytes of memory:
          -- , runFailingTest ("tests" </> "shouldfail") defBuild [] "RecursivePoly" (Just "??")
          ]
      , clashTestGroup "BitVector"
        [ runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "Box"              (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "BoxGrow"          (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "CLZ"              (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "RePack"           ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "ReduceZero"       (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "ReduceOne"        (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "ExtendingNumZero" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild ["-fconstraint-solver-iterations=15"] "GenericBitPack"   (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "AppendZero"       (["","testBench"],"testBench",True)
        ]
      , clashTestGroup "BlackBox"
        [ outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "TemplateFunction"   "main"
        , outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "BlackBoxFunction"   "main"
        , runTest    ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   []    "BlackBoxFunctionHO" (["","testBench"],"testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "Signal")   defBuild [] [] "BlockRamLazy"       "main"
        , runFailingTest ("tests" </> "shouldfail" </> "BlackBox") [VHDL] [] "WrongReference" (Just "Function WrongReference.myMultiply was annotated with an inline primitive for WrongReference.myMultiplyX. These names should be the same.")
        ]
      , clashTestGroup "BoxedFunctions"
        [ runTest ("tests" </> "shouldwork" </> "BoxedFunctions") defBuild [] "DeadRecursiveBoxed" ([""],"topEntity",False)
        ]
      , clashTestGroup "CSignal"
        [ runTest ("tests" </> "shouldwork" </> "CSignal") defBuild [] "CBlockRamTest" ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "CSignal") defBuild [] "MAC"           ([""],"topEntity",False)
        ]
#ifdef COSIM
      , clashTestGroup "CoSim"
        [ runTest ("tests" </> "shouldwork" </> "CoSim") [Verilog] [] "Multiply" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CoSim") [Verilog] [] "Register" (["","testBench"],"testBench",True)
        ]
#endif
      , clashTestGroup "CustomReprs"
        [ runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "RotateC"                (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "ReprCompact"            (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "ReprCompactScrambled"   (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "ReprLastBitConstructor" (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       [VHDL]   [] "ReprStrangeMasks"       (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "ReprWide"               (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "RotateCScrambled"       (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateCNested") defBuild [] "RotateCNested"          (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "Rotate")        defBuild [] "Rotate"                 (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "Deriving")      defBuild [] "BitPackDerivation"      (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "Indexed")       defBuild [] "Indexed"                (["", "testBench"],"testBench",True)

        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "ZeroWidth") defBuild [] "ZeroWidth" ([""],"topEntity",False)
        , runFailingTest ("tests" </> "shouldwork" </> "CustomReprs" </> "ZeroWidth") defBuild [] "FailGracefully1" (Just "Unexpected projection of zero-width type")
        , runFailingTest ("tests" </> "shouldwork" </> "CustomReprs" </> "ZeroWidth") defBuild [] "FailGracefully2" (Just "Unexpected projection of zero-width type")
        , runFailingTest ("tests" </> "shouldwork" </> "CustomReprs" </> "ZeroWidth") defBuild [] "FailGracefully3" (Just "Unexpected projection of zero-width type")
        ]
      , clashTestGroup "DDR"
        [ runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRinGA" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRinGS" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRinUA" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRinUS" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRoutUA" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRoutUS" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRoutGA" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRoutGS" (["","testBench"],"testBench",True)
        ]
      , clashTestGroup "DSignal"
        [ runTest ("tests" </> "shouldwork" </> "DSignal") defBuild [] "DelayedFold" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DSignal") defBuild [] "DelayI"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DSignal") defBuild [] "DelayN"      (["","testBench"],"testBench",True)
        ]
      , clashTestGroup "Feedback"
        [ runTest ("tests" </> "shouldwork" </> "Feedback") defBuild [] "Fib" (["","testBench"],"testBench",True)
#ifdef CLASH_MULTIPLE_HIDDEN
        , runTest ("tests" </> "shouldwork" </> "Feedback") defBuild [] "MutuallyRecursive" (["","testBench"],"testBench",True)
#endif
        ]
      , clashTestGroup "Fixed"
        [ runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "Mixer"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "SFixedTest" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "SatWrap"    ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "ZeroInt"    (["","testBench"],"testBench",True)
        ]
      , clashTestGroup "Floating"
        [ runTest ("tests" </> "shouldwork" </> "Floating") defBuild ["-fclash-float-support"] "FloatPack" ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Floating") defBuild ["-fclash-float-support"] "FloatConstFolding" (["","testBench"],"testBench",True)
        ]
      , clashTestGroup "GADTs"
        [ runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "Constrained"          (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "Head"                 (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "HeadM"                (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "MonomorphicTopEntity" (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "Record"               (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "Tail"                 (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "TailM"                (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "TailOfTail"           (["", "testBench"],"testBench",True)
        ]
      , clashTestGroup "HOPrim"
        [ runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "HOIdx"     (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "HOImap"    (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "Map"       (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "Map2"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "TestMap"   (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "Transpose" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "VecFun"    (["","testBench"],"testBench",True)
      ]
      , clashTestGroup "Numbers"
        [ runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "BitInteger"   (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Bounds"       (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "DivideByZero" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-itests/shouldwork/Numbers", "-fconstraint-solver-iterations=15"] "ExpWithGhcCF"        (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-itests/shouldwork/Numbers", "-fconstraint-solver-iterations=15"] "ExpWithClashCF"        (["","testBench"],"testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-itests/shouldwork/Numbers"] ["-itests/shouldwork/Numbers"] "ExpWithClashCF"  "main"
        , runTest ("tests" </> "shouldwork" </> "Numbers") [VHDL] [] "HalfAsBlackboxArg" ([""],"topEntity",False)
        -- TODO: re-enable for Verilog
        , runTest ("tests" </> "shouldwork" </> "Numbers") (defBuild \\ [Verilog]) ["-itests/shouldwork/Numbers"] "NumConstantFoldingTB_1"        (["","testBench"],"testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-fconstraint-solver-iterations=15"] ["-itests/shouldwork/Numbers"] "NumConstantFolding_1"  "main"
        , runTest ("tests" </> "shouldwork" </> "Numbers") (defBuild \\ [Verilog]) ["-itests/shouldwork/Numbers"] "NumConstantFoldingTB_2"        (["","testBench"],"testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-fconstraint-solver-iterations=15"] ["-itests/shouldwork/Numbers"] "NumConstantFolding_2"  "main"
#if MIN_VERSION_base(4,12,0)
        -- Naturals are broken on GHC <= 8.4. See https://github.com/clash-lang/clash-compiler/pull/473
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Naturals"     (["","testBench"],"testBench",True)
#endif
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "NegativeLits" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize"       (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize2"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize3"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "SatMult"      ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-itests/shouldwork/Numbers"] "ShiftRotate"         (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "SignedProjectionTB"   (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "SignedZero"   (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Signum"   (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Strict"       (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "UnsignedZero" (["","testBench"],"testBench",True)
        ]
      , clashTestGroup "Polymorphism"
        [ runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "ExistentialBoxed"  ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "FunctionInstances" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "GADTExistential"   ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "LocalPoly"         ([""],"topEntity",False)
        ]
      , clashTestGroup "PrimitiveGuards"
        [ runFailingTest ("tests" </> "shouldfail" </> "PrimitiveGuards") defBuild [] "HasBlackBox" (Just "No BlackBox definition for 'HasBlackBox.primitive' even though this value was annotated with 'HasBlackBox'.")
        , runFailingTest ("tests" </> "shouldfail" </> "PrimitiveGuards") defBuild [] "DontTranslate" (Just "Clash was forced to translate 'DontTranslate.primitive', but this value was marked with DontTranslate. Did you forget to include a blackbox for one of the constructs using this?")
        , runWarningTest ("tests" </> "shouldwork" </> "PrimitiveGuards") [VHDL] [] "WarnAlways" (Just "You shouldn't use 'primitive'!")
        ]

      , clashTestGroup "PrimitiveReductions"
        [ runTest ("tests" </> "shouldwork" </> "PrimitiveReductions") defBuild [] "Lambda"     (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "PrimitiveReductions") defBuild [] "ReplaceInt" (["","testBench"],"testBench",True)
        ]
      , clashTestGroup "RTree"
        [ runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TFold"       ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TRepeat"  (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TRepeat2"  (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TZip"        ([""],"topEntity",False)
      ]
      , clashTestGroup "Signal"
        [ runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "AlwaysHigh"      ([""],"topEntity",False)
        , outputTest ("tests" </> "shouldwork" </> "Signal") defBuild [] [] "BlockRamLazy"    "main"
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamFile"    (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRam0"        (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRam1"        (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamTest"    ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Compression"     (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "DelayedReset"    (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "NoCPR"           (["example"],"example",False)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Oversample"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Ram"             (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RegisterAR"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RegisterSR"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RegisterAE"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RegisterSE"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "ResetGen"        (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "ResetLow"        (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Rom"             (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RomFile"         (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "SigP"            ([""],"topEntity",False)

        , runTest ("tests" </> "shouldwork" </> "Signal" </> "BiSignal") defBuild [] "Counter" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal" </> "BiSignal") defBuild [] "CounterHalfTuple" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal" </> "BiSignal") defBuild [] "CounterHalfTupleRev" (["","testBench"],"testBench",True)

        , runFailingTest ("tests" </> "shouldfail" </> "Signal") defBuild [] "MAC" (Just "Couldn't instantiate blackbox for Clash.Signal.Internal.register#")
        ]
      , clashTestGroup "SynthesisAttributes"
        [ outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") defBuild [] [] "Simple"  "main"
        , outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") defBuild [] [] "Product" "main"
        , runTest ("tests" </> "shouldwork" </> "SynthesisAttributes") defBuild [] "Product" (["", "testBench"],"testBench",True)
        , clashTestGroup "ShouldFail" [
            runFailingTest ("tests" </> "shouldfail" </> "SynthesisAttributes") defBuild [] "ProductInArgs"   (Just "Attempted to split Product into a number of HDL ports.")
          , runFailingTest ("tests" </> "shouldfail" </> "SynthesisAttributes") defBuild [] "ProductInResult" (Just "Attempted to split Product into a number of HDL ports.")
          ]
        ]
      , clashTestGroup "Testbench"
        [ runTest ("tests" </> "shouldwork" </> "Testbench") defBuild ["-fclash-inline-limit=0"] "TB" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Testbench") defBuild [] "SyncTB"                    (["","testBench"],"testBench",True)
        ]
      , clashTestGroup "Types"
        [ runTest ("tests" </> "shouldwork" </> "Types") defBuild [] "TypeFamilyReduction" ([""],"topEntity",False)
        ]
      , clashTestGroup "TopEntity"
        -- VHDL tests disabled for now: I can't figure out how to generate a static name whilst retaining the ability to actually test..
        [ runTest ("tests" </> "shouldwork" </> "TopEntity")    [Verilog] [] "PortNames" (["","PortNames_topEntity","PortNames_testBench"],"PortNames_testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNames" "main"
        , runTest ("tests" </> "shouldwork" </> "TopEntity")    [Verilog] [] "PortProducts" (["","PortProducts_topEntity","PortProducts_testBench"],"PortProducts_testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortProducts" "main"
        , runTest ("tests" </> "shouldwork" </> "TopEntity")    [Verilog] [] "PortProductsSum" (["","PortProductsSum_topEntity","PortProductsSum_testBench"],"PortProductsSum_testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortProductsSum" "main"
        , runTest ("tests" </> "shouldwork" </> "TopEntity")    [Verilog] [] "PortNamesWithUnit" (["","PortNamesWithUnit_topEntity","PortNamesWithUnit_testBench"],"PortNamesWithUnit_testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithUnit" "main"
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithSingletonVector" "main"
        , runTest ("tests" </> "shouldwork" </> "TopEntity")    [Verilog] [] "PortNamesWithVector" (["","PortNamesWithVector_topEntity","PortNamesWithVector_testBench"],"PortNamesWithVector_testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithVector" "main"
        , runTest ("tests" </> "shouldwork" </> "TopEntity")    [Verilog] [] "PortNamesWithRTree" (["","PortNamesWithRTree_topEntity","PortNamesWithRTree_testBench"],"PortNamesWithRTree_testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithRTree" "main"
        , runTest ("tests" </> "shouldwork" </> "TopEntity")    defBuild [] "TopEntHOArg" (["f","g"],"f",False)
        ]
      , clashTestGroup "Void"
        [ runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "Imap"                         (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithUnitVector"            (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithTupleWithUnitLeft"     (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithTupleWithUnitRight"    (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithTripleWithUnitMiddle"  (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithUnitSP"                (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithUnitSP2"               (["","testBench"],"testBench",True)
        ]
      , clashTestGroup "Vector"
        [ runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Concat"     (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DFold"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DFold2"     (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DTFold"     (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "EnumTypes"  ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FindIndex"  (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FirOddSize" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Fold"       (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FoldlFuns"  ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Foldr"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FoldrEmpty" (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOClock"    ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOCon"      ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOPrim"     ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "IndexInt"   (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Indices"    (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Minimum"    (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "MovingAvg"  ([""],"topEntity", False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "PatHOCon"   ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Scatter"    (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Split"      ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "ToList"     (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Unconcat"   (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VACC"       ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VEmpty"     (["", "testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VIndex"     ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VIndicesI"  (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VFold"      (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VMapAccum"  ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VMerge"     (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VReplace"   (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VReverse"   (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VRotate"    (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VScan"      ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VSelect"    (["","testBench"],"testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VZip"       ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VecConst"   ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VecOfSum"   ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "T452"       ([""],"topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") [VHDL]   [] "T895"       ([""],"topEntity",False)
        ]
      ]
    ]

main :: IO ()
main = do
  _ <- mapM (uncurry setEnv) [ ("clash_ghc_datadir", "./clash-ghc")
                             , ("clash_lib_datadir", "./clash-lib")
                             , ("clash_prelude_datadir", "./clash-prelude")
                             , ("clash_testsuite_datadir", "./testsuite")
                             ]

  putStrLn $ "Running in " ++ temporaryDirectory
  createDirectoryIfMissing True temporaryDirectory

  putStrLn $ "Making sure Clash is compiled.. "
  let flag = "--help"
  let cp = proc "cabal" ["--write-ghc-environment-files=always", "-v2", "new-run", "--", "clash", flag]
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode cp ""

  case exitCode of
    ExitSuccess -> do
      -- Execute test with found clash binary
      let cmd0 = head $ filter (isSuffixOf ("clash " ++ flag)) $ lines stdout
      let cmd1 = take (length cmd0 - (length flag + 1)) cmd0
      setEnv "clash_bin" cmd1

      putStrLn $ "Default number of threads: " ++ show numCapabilities
      setEnv "TASTY_NUM_THREADS" (show numCapabilities)

      finally
        runClashTest
        (do
          putStrLn $ "Cleaning up " ++ temporaryDirectory
          removeDirectoryRecursive temporaryDirectory
        )
    ExitFailure _ -> do
      -- Building clash failed
      putStrLn "'cabal new-run clash' failed"
      putStrLn ">>> stdout:"
      putStrLn stdout
      putStrLn ">>> stderr:"
      putStrLn stderr
      exitWith exitCode
