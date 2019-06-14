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
      [ runTest "examples"               defBuild []                                        "ALU"          ([""],"ALU_topEntity",False)
      , runTest "examples"               [VHDL]   []                                        "Blinker"      (["blinker"],"blinker",False)
      , runTest "examples"               defBuild []                                        "BlockRamTest" ([""],"BlockRamTest_topEntity",False)
      , runTest "examples"               defBuild []                                        "Calculator"   (["","Calculator_testBench"],"Calculator_testBench",True )
      , runTest "examples"               defBuild []                                        "CHIP8"        ([""],"CHIP8_topEntity", False)
      , runTest "examples"               defBuild []                                        "CochleaPlus"  ([""],"CochleaPlus_topEntity",False)
      , runTest "examples"               defBuild ["-fclash-component-prefix","test"]       "FIR"          (["","test_FIR_testBench"],"test_FIR_testBench",True )
      , runTest "examples"               defBuild []                                        "Fifo"         ([""],"Fifo_topEntity",False)
      , runTest "examples"               defBuild []                                        "MAC"          (["","MAC_testBench"],"MAC_testBench",True)
      , runTest "examples"               defBuild []                                        "MatrixVect"   (["","MatrixVect_testBench"],"MatrixVect_testBench",True)
      , runTest "examples"               defBuild []                                        "Queens"       ([""],"Queens_topEntity",False)
      , runTest "examples"               defBuild []                                        "Reducer"      ([""],"Reducer_topEntity",False)
      , runTest "examples"               defBuild []                                        "Sprockell"    ([""],"Sprockell_topEntity",False)
      , runTest "examples"               defBuild []                                        "Windows"      ([""],"Windows_topEntity",False)
      , runTest ("examples" </> "crc32") defBuild []                                        "CRC32"        (["","CRC32_testBench"],"CRC32_testBench",True)
      , runTest ("examples" </> "i2c")   defBuild ["-O2","-fclash-component-prefix","test"] "I2C"          (["test_i2c","test_bitmaster","test_bytemaster"],"test_i2c",False)
      ]
    , clashTestGroup "Unit"
      [ clashTestGroup "Basic"
        [ -- TODO: Enable AES test on SystemVerilog. See issue #569.
          runTest ("tests" </> "shouldwork" </> "Basic") (defBuild \\ [SystemVerilog]) [] "AES"                 ([""],"AES_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "BangData"            ([""],"BangData_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Trace"               ([""],"Trace_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "ByteSwap32"          (["","ByteSwap32_testBench"],"ByteSwap32_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "CharTest"            (["","CharTest_testBench"],"CharTest_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "ClassOps"            (["","ClassOps_testBench"],"ClassOps_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "CountTrailingZeros"  (["","CountTrailingZeros_testBench"],"CountTrailingZeros_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "DeepseqX"            (["","DeepseqX_testBench"],"DeepseqX_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "DivMod"              ([""],"DivMod_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "IrrefError"          ([""],"IrrefError_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "LambdaDrop"          ([""],"LambdaDrop_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "LotOfStates"         (["","LotOfStates_testBench"],"LotOfStates_testBench",True)
        -- TODO: Fix for VHDL. ModelSim / GHDL simulate _extremely_ slowly
        , runTest ("tests" </> "shouldwork" </> "Basic") (defBuild \\ [VHDL]) [] "MultipleHidden"      (["","MultipleHidden_testBench"],"MultipleHidden_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NameOverlap"         (["nameoverlap"],"nameoverlap",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NestedPrimitives"    ([""],"NestedPrimitives_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NestedPrimitives2"   ([""],"NestedPrimitives2_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NORX"                (["","NORX_testBench"],"NORX_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") [VHDL]   [] "Parameters"          (["","Parameters_testBench"],"Parameters_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "PatError"            ([""],"PatError_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "PopCount"            (["","PopCount_testBench"],"PopCount_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "RecordSumOfProducts" ([""],"RecordSumOfProducts_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Replace"             (["", "Replace_testBench"],"Replace_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Shift"               ([""],"Shift_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "SimpleConstructor"   ([""],"SimpleConstructor_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TagToEnum"           ([""],"TagToEnum_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TestIndex"           ([""],"TestIndex_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Time"                (["","Time_testBench"],"Time_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TwoFunctions"        ([""],"TwoFunctions_topEntity",False)
        ]
        , clashTestGroup "ShouldFail"
          [ runFailingTest ("tests" </> "shouldfail") [VHDL] [] "RecursiveBoxed" (Just "Callgraph after normalisation contains following recursive components")
          , runFailingTest ("tests" </> "shouldfail") [VHDL] [] "RecursiveDatatype" (Just "Not in normal form: no Letrec")
          , runFailingTest ("tests" </> "shouldfail" </> "InvalidPrimitive") [VHDL] ["-itests/shouldfail/InvalidPrimitive"] "InvalidPrimitive" (Just "InvalidPrimitive.json")
          -- Disabled, due to it eating gigabytes of memory:
          -- , runFailingTest ("tests" </> "shouldfail") defBuild [] "RecursivePoly" (Just "??")
          ]
      , clashTestGroup "BitVector"
        [ runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "Box"              (["","Box_testBench"],"Box_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "BoxGrow"          (["","BoxGrow_testBench"],"BoxGrow_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "RePack"           ([""],"RePack_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "ReduceZero"       (["","ReduceZero_testBench"],"ReduceZero_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "ReduceOne"        (["","ReduceOne_testBench"],"ReduceOne_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "ExtendingNumZero" (["","ExtendingNumZero_testBench"],"ExtendingNumZero_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild ["-fconstraint-solver-iterations=15"] "GenericBitPack"   (["","GenericBitPack_testBench"],"GenericBitPack_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "AppendZero"       (["","AppendZero_testBench"],"AppendZero_testBench",True)
        ]
      , clashTestGroup "BlackBox"
        [ outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "TemplateFunction" "main"
        , outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "BlackBoxFunction" "main"
        , outputTest ("tests" </> "shouldwork" </> "Signal")   defBuild [] [] "BlockRamLazy"     "main"
        ]
      , clashTestGroup "BoxedFunctions"
        [ runTest ("tests" </> "shouldwork" </> "BoxedFunctions") defBuild [] "DeadRecursiveBoxed" ([""],"DeadRecursiveBoxed_topEntity",False)
        ]
      , clashTestGroup "CSignal"
        [ runTest ("tests" </> "shouldwork" </> "CSignal") defBuild [] "CBlockRamTest" ([""],"CBlockRamTest_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "CSignal") defBuild [] "MAC"           ([""],"MAC_topEntity",False)
        ]
#ifdef COSIM
      , clashTestGroup "CoSim"
        [ runTest ("tests" </> "shouldwork" </> "CoSim") [Verilog] ["-i../../../clash-cosim/src/prims/verilog"] "Multiply" (["","Multiply_testBench"],"Multiply_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CoSim") [Verilog] ["-i../../../clash-cosim/src/prims/verilog"] "Register" (["","Register_testBench"],"Register_testBench",True)
        ]
#endif
      , clashTestGroup "CustomReprs"
        [ runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "RotateC"                (["", "RotateC_testBench"],"RotateC_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "ReprCompact"            (["", "ReprCompact_testBench"],"ReprCompact_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "ReprCompactScrambled"   (["", "ReprCompactScrambled_testBench"],"ReprCompactScrambled_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "ReprLastBitConstructor" (["", "ReprLastBitConstructor_testBench"],"ReprLastBitConstructor_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       [VHDL]   [] "ReprStrangeMasks"       (["", "ReprStrangeMasks_testBench"],"ReprStrangeMasks_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "ReprWide"               (["", "ReprWide_testBench"],"ReprWide_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC")       defBuild [] "RotateCScrambled"       (["", "RotateCScrambled_testBench"],"RotateCScrambled_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateCNested") defBuild [] "RotateCNested"          (["", "RotateCNested_testBench"],"RotateCNested_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "Rotate")        defBuild [] "Rotate"                 (["", "Rotate_testBench"],"Rotate_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "Deriving")      defBuild [] "BitPackDerivation"      (["", "BitPackDerivation_testBench"],"BitPackDerivation_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "Indexed")       defBuild [] "Indexed"                (["", "Indexed_testBench"],"Indexed_testBench",True)
        ]
      , clashTestGroup "DDR"
        [ runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRinGA" (["","DDRinGA_testBench"],"DDRinGA_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRinGS" (["","DDRinGS_testBench"],"DDRinGS_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRinUA" (["","DDRinUA_testBench"],"DDRinUA_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRinUS" (["","DDRinUS_testBench"],"DDRinUS_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRoutUA" (["","DDRoutUA_testBench"],"DDRoutUA_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRoutUS" (["","DDRoutUS_testBench"],"DDRoutUS_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRoutGA" (["","DDRoutGA_testBench"],"DDRoutGA_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DDR") defBuild [] "DDRoutGS" (["","DDRoutGS_testBench"],"DDRoutGS_testBench",True)
        ]
      , clashTestGroup "DSignal"
        [ runTest ("tests" </> "shouldwork" </> "DSignal") defBuild [] "DelayedFold" (["","DelayedFold_testBench"],"DelayedFold_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DSignal") defBuild [] "DelayI"      (["","DelayI_testBench"],"DelayI_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "DSignal") defBuild [] "DelayN"      (["","DelayN_testBench"],"DelayN_testBench",True)
        ]
      , clashTestGroup "Feedback"
        [ runTest ("tests" </> "shouldwork" </> "Feedback") defBuild [] "Fib" (["","Fib_testBench"],"Fib_testBench",True)
        ]
      , clashTestGroup "Fixed"
        [ runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "Mixer"      (["","Mixer_testBench"],"Mixer_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "SFixedTest" (["","SFixedTest_testBench"],"SFixedTest_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "SatWrap"    ([""],"SatWrap_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "ZeroInt"    (["","ZeroInt_testBench"],"ZeroInt_testBench",True)
        ]
      , clashTestGroup "Floating"
        [ runTest ("tests" </> "shouldwork" </> "Floating") defBuild ["-fclash-float-support"] "FloatPack" ([""],"FloatPack_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Floating") defBuild ["-fclash-float-support"] "FloatConstFolding" (["","FloatConstFolding_testBench"],"FloatConstFolding_testBench",True)
        ]
      , clashTestGroup "GADTs"
        [ runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "Constrained"          (["", "Constrained_testBench"],"Constrained_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "Head"                 (["", "Head_testBench"],"Head_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "HeadM"                (["", "HeadM_testBench"],"HeadM_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "MonomorphicTopEntity" (["", "MonomorphicTopEntity_testBench"],"MonomorphicTopEntity_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "Record"               (["", "Record_testBench"],"Record_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "Tail"                 (["", "Tail_testBench"],"Tail_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "TailM"                (["", "TailM_testBench"],"TailM_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "GADTs") defBuild [] "TailOfTail"           (["", "TailOfTail_testBench"],"TailOfTail_testBench",True)
        ]
      , clashTestGroup "HOPrim"
        [ runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "HOImap"    (["","HOImap_testBench"],"HOImap_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "Map"       (["","Map_testBench"],"Map_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "Map2"      (["","Map2_testBench"],"Map2_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "TestMap"   (["","TestMap_testBench"],"TestMap_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "Transpose" (["","Transpose_testBench"],"Transpose_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "VecFun"    (["","VecFun_testBench"],"VecFun_testBench",True)
      ]
      , clashTestGroup "Numbers"
        [ runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "BitInteger"   (["","BitInteger_testBench"],"BitInteger_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Bounds"       (["","Bounds_testBench"],"Bounds_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-itests/shouldwork/Numbers", "-fconstraint-solver-iterations=15"] "ExpWithGhcCF"        (["","ExpWithGhcCF_testBench"],"ExpWithGhcCF_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-itests/shouldwork/Numbers", "-fconstraint-solver-iterations=15"] "ExpWithClashCF"        (["","ExpWithClashCF_testBench"],"ExpWithClashCF_testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-itests/shouldwork/Numbers"] ["-itests/shouldwork/Numbers"] "ExpWithClashCF"  "main"
        -- TODO: re-enable for Verilog
        , runTest ("tests" </> "shouldwork" </> "Numbers") (defBuild \\ [Verilog]) ["-itests/shouldwork/Numbers","-fclash-inline-limit=300"] "NumConstantFoldingTB"       (["","NumConstantFoldingTB_testBench"],"NumConstantFoldingTB_testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-fclash-inline-limit=300", "-fconstraint-solver-iterations=15"] [] "NumConstantFolding"  "main"
#if MIN_VERSION_base(4,12,0)
        -- Naturals are broken on GHC <= 8.4. See https://github.com/clash-lang/clash-compiler/pull/473
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Naturals"     (["","Naturals_testBench"],"Naturals_testBench",True)
#endif
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "NegativeLits" (["","NegativeLits_testBench"],"NegativeLits_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize"       (["","Resize_testBench"],"Resize_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize2"      (["","Resize2_testBench"],"Resize2_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize3"      (["","Resize3_testBench"],"Resize3_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "SatMult"      ([""],"SatMult_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild ["-itests/shouldwork/Numbers"] "ShiftRotate"         (["","ShiftRotate_testBench"],"ShiftRotate_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "SignedProjectionTB"   (["","SignedProjectionTB_testBench"],"SignedProjectionTB_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "SignedZero"   (["","SignedZero_testBench"],"SignedZero_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Signum"   (["","Signum_testBench"],"Signum_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Strict"       (["","Strict_testBench"],"Strict_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "UnsignedZero" (["","UnsignedZero_testBench"],"UnsignedZero_testBench",True)
        ]
      , clashTestGroup "Polymorphism"
        [ runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "ExistentialBoxed"  ([""],"ExistentialBoxed_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "FunctionInstances" (["","FunctionInstances_testBench"],"FunctionInstances_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "GADTExistential"   ([""],"GADTExistential_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "LocalPoly"         ([""],"LocalPoly_topEntity",False)
        ]
      , clashTestGroup "PrimitiveGuards"
        [ runFailingTest ("tests" </> "shouldfail" </> "PrimitiveGuards") defBuild [] "HasBlackBox" (Just "No BlackBox definition for 'HasBlackBox.primitive' even though this value was annotated with 'HasBlackBox'.")
        , runFailingTest ("tests" </> "shouldfail" </> "PrimitiveGuards") defBuild [] "DontTranslate" (Just "Clash was forced to translate 'DontTranslate.primitive', but this value was marked with DontTranslate. Did you forget to include a blackbox for one of the constructs using this?")
        , runWarningTest ("tests" </> "shouldwork" </> "PrimitiveGuards") [VHDL] [] "WarnAlways" (Just "You shouldn't use 'primitive'!")
        ]

      , clashTestGroup "PrimitiveReductions"
        [ runTest ("tests" </> "shouldwork" </> "PrimitiveReductions") defBuild [] "Lambda"     (["","Lambda_testBench"],"Lambda_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "PrimitiveReductions") defBuild [] "ReplaceInt" (["","ReplaceInt_testBench"],"ReplaceInt_testBench",True)
        ]
      , clashTestGroup "RTree"
        [ runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TFold"       ([""],"TFold_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TRepeat"  ([""],"TRepeat_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TZip"        ([""],"TZip_topEntity",False)
      ]
      , clashTestGroup "Signal"
        [ runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "AlwaysHigh"      ([""],"AlwaysHigh_topEntity",False)
        , outputTest ("tests" </> "shouldwork" </> "Signal") defBuild [] [] "BlockRamLazy"    "main"
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamFile"    (["","BlockRamFile_testBench"],"BlockRamFile_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamTest"    ([""],"BlockRamTest_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "DelayedReset"    (["","DelayedReset_testBench"],"DelayedReset_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "NoCPR"           (["example"],"example",False)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Ram"             (["","Ram_testBench"],"Ram_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RegisterAR"      (["","RegisterAR_testBench"],"RegisterAR_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RegisterSR"      (["","RegisterSR_testBench"],"RegisterSR_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RegisterAE"      (["","RegisterAE_testBench"],"RegisterAE_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RegisterSE"      (["","RegisterSE_testBench"],"RegisterSE_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "ResetLow"        (["","ResetLow_testBench"],"ResetLow_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Rom"             (["","Rom_testBench"],"Rom_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RomFile"         (["","RomFile_testBench"],"RomFile_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "SigP"            ([""],"SigP_topEntity",False)

        , runTest ("tests" </> "shouldwork" </> "Signal" </> "BiSignal") defBuild [] "Counter" (["","Counter_testBench"],"Counter_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal" </> "BiSignal") defBuild [] "CounterHalfTuple" (["","CounterHalfTuple_testBench"],"CounterHalfTuple_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Signal" </> "BiSignal") defBuild [] "CounterHalfTupleRev" (["","CounterHalfTupleRev_testBench"],"CounterHalfTupleRev_testBench",True)

        , runFailingTest ("tests" </> "shouldfail" </> "Signal") defBuild [] "MAC" (Just "Can't match template for \"Clash.Signal.Internal.register#\"")
        ]
      , clashTestGroup "SynthesisAttributes"
        [ outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") defBuild [] [] "Simple"  "main"
        , outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") defBuild [] [] "Product" "main"
        , runTest ("tests" </> "shouldwork" </> "SynthesisAttributes") defBuild [] "Product" (["", "Product_testBench"],"Product_testBench",True)
        , clashTestGroup "ShouldFail" [
            runFailingTest ("tests" </> "shouldfail" </> "SynthesisAttributes") defBuild [] "ProductInArgs"   (Just "Attempted to split Product into a number of HDL ports.")
          , runFailingTest ("tests" </> "shouldfail" </> "SynthesisAttributes") defBuild [] "ProductInResult" (Just "Attempted to split Product into a number of HDL ports.")
          ]
        ]
      , clashTestGroup "Testbench"
        [ runTest ("tests" </> "shouldwork" </> "Testbench") defBuild ["-fclash-inline-limit=0"] "TB" (["","TB_testBench"],"TB_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Testbench") defBuild [] "SyncTB"                    (["","SyncTB_testBench"],"SyncTB_testBench",True)
        ]
      , clashTestGroup "Types"
        [ runTest ("tests" </> "shouldwork" </> "Types") defBuild [] "TypeFamilyReduction" ([""],"TypeFamilyReduction_topEntity",False)
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
        , runTest ("tests" </> "shouldwork" </> "TopEntity")    [Verilog] [] "PortNamesWithVector" (["","PortNamesWithVector_topEntity","PortNamesWithVector_testBench"],"PortNamesWithVector_testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithVector" "main"
        , runTest ("tests" </> "shouldwork" </> "TopEntity")    [Verilog] [] "PortNamesWithRTree" (["","PortNamesWithRTree_topEntity","PortNamesWithRTree_testBench"],"PortNamesWithRTree_testBench",True)
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithRTree" "main"
        , runTest ("tests" </> "shouldwork" </> "TopEntity")    defBuild [] "TopEntHOArg" (["f","g"],"f",False)
        ]
      , clashTestGroup "Void"
        [ runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "Imap"                         (["","Imap_testBench"],"Imap_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithUnitVector"            (["","ZipWithUnitVector_testBench"],"ZipWithUnitVector_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithTupleWithUnitLeft"     (["","ZipWithTupleWithUnitLeft_testBench"],"ZipWithTupleWithUnitLeft_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithTupleWithUnitRight"    (["","ZipWithTupleWithUnitRight_testBench"],"ZipWithTupleWithUnitRight_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithTripleWithUnitMiddle"  (["","ZipWithTripleWithUnitMiddle_testBench"],"ZipWithTripleWithUnitMiddle_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithUnitSP"                (["","ZipWithUnitSP_testBench"],"ZipWithUnitSP_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Unit") defBuild [] "ZipWithUnitSP2"               (["","ZipWithUnitSP2_testBench"],"ZipWithUnitSP2_testBench",True)
        ]
      , clashTestGroup "Vector"
        [ runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Concat"     (["","Concat_testBench"],"Concat_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DFold"      (["","DFold_testBench"],"DFold_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DFold2"     (["","DFold2_testBench"],"DFold2_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DTFold"     (["","DTFold_testBench"],"DTFold_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "EnumTypes"  ([""],"EnumTypes_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FindIndex"  (["","FindIndex_testBench"],"FindIndex_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FirOddSize" (["","FirOddSize_testBench"],"FirOddSize_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Fold"       (["","Fold_testBench"],"Fold_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FoldlFuns"  ([""],"FoldlFuns_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Foldr"      (["","Foldr_testBench"],"Foldr_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FoldrEmpty" (["","FoldrEmpty_testBench"],"FoldrEmpty_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOClock"    ([""],"HOClock_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOCon"      ([""],"HOCon_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOPrim"     ([""],"HOPrim_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "IndexInt"   (["","IndexInt_testBench"],"IndexInt_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Indices"    (["","Indices_testBench"],"Indices_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Minimum"    (["","Minimum_testBench"],"Minimum_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "MovingAvg"  ([""],"MovingAvg_topEntity", False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "PatHOCon"   ([""],"PatHOCon_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Scatter"    (["","Scatter_testBench"],"Scatter_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Split"      ([""],"Split_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "ToList"     (["","ToList_testBench"],"ToList_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Unconcat"   (["","Unconcat_testBench"],"Unconcat_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VACC"       ([""],"VACC_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VEmpty"     (["", "VEmpty_testBench"],"VEmpty_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VIndex"     ([""],"VIndex_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VIndicesI"  (["","VIndicesI_testBench"],"VIndicesI_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VFold"      (["","VFold_testBench"],"VFold_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VMapAccum"  ([""],"VMapAccum_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VMerge"     (["","VMerge_testBench"],"VMerge_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VReplace"   (["","VReplace_testBench"],"VReplace_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VReverse"   (["","VReverse_testBench"],"VReverse_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VRotate"    (["","VRotate_testBench"],"VRotate_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VScan"      ([""],"VScan_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VSelect"    (["","VSelect_testBench"],"VSelect_testBench",True)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VZip"       ([""],"VZip_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VecConst"   ([""],"VecConst_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VecOfSum"   ([""],"VecOfSum_topEntity",False)
        , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "T452"       ([""],"T452_topEntity",False)
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
