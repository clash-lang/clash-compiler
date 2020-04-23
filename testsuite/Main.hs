{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import qualified Clash.Util.Interpolate    as I

import           Control.Exception         (finally)
import qualified Data.Text                 as Text
import           Data.Default              (def)
import           Data.List                 (isSuffixOf, (\\))
import           System.Directory
  (createDirectoryIfMissing, removeDirectoryRecursive)
import           System.Environment        (setEnv)
import           System.Exit
  (exitWith, ExitCode(ExitSuccess, ExitFailure))
import           System.FilePath           ((</>))
import           System.Process            (readCreateProcessWithExitCode, proc)
import           GHC.Conc                  (numCapabilities)

import           Test.Tasty
import           Test.Tasty.Clash


clashTestRoot
  :: [[TestName] -> TestTree]
  -> TestTree
clashTestRoot testTrees =
  clashTestGroup "." testTrees []

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
runClashTest = defaultMain $ clashTestRoot
  [ clashTestGroup "netlist"
    [ clashLibTest ("tests" </> "shouldwork" </> "Netlist") allTargets [] "Identity" "main"
#if !EXPERIMENTAL_EVALUATOR
    , clashLibTest ("tests" </> "shouldwork" </> "Netlist") [VHDL] [] "NoDeDup" "main"
#endif
    ]
  , clashTestGroup "examples"
    [ runTest "ALU" def{hdlSim=False}
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
    , runTest "Blinker" def{
        hdlSim=False
      , hdlTargets=[VHDL]
      , entities=Entities [["blinker"]]
      , topEntities=TopEntities ["blinker"]
      }
    , runTest "BlockRamTest" def{hdlSim=False}
    , runTest "Calculator" def
    , runTest "CHIP8" def{hdlSim=False}
    , runTest "CochleaPlus" def{hdlSim=False}
#endif
#if !EXPERIMENTAL_EVALUATOR
    , runTest "FIR" def{
        clashFlags=["-fclash-component-prefix", "test"]
      , entities=Entities [["","test_testBench"]]
      , topEntities=TopEntities ["test_testBench"]
      }
#endif
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
    , runTest "Fifo" def{hdlSim=False}
    , runTest "MAC" def
    , runTest "MatrixVect" def
    , runTest "Queens" def{hdlSim=False}
    , runTest "Reducer" def{hdlSim=False}
    , runTest "Sprockell" def{hdlSim=False}
    , runTest "Windows" def{hdlSim=False}
    , clashTestGroup "crc32" [ runTest "CRC32" def ]
    , clashTestGroup "i2c"
      [ runTest "I2C" def{
          clashFlags=["-O2","-fclash-component-prefix","test"]
        , entities=Entities [["test_i2c","test_bitmaster","test_bytemaster"]]
        , topEntities=TopEntities ["test_i2c"]
        , hdlSim=False
        }
#endif
#if !EXPERIMENTAL_EVALUATOR
      , runTest "I2Ctest" def {
          entities=Entities [[ ".." </> "I2C" </> "i2c"
                             , ".." </> "I2C" </> "bitmaster"
                             , ".." </> "I2C" </> "bytemaster"
                             , "configi2c"
                             , "slave"
                             , "system"
                             ]]
        , topEntities=TopEntities ["system"]
        , hdlTargets=[Verilog]
        , hdlSim=True
        , vvpStderrEmptyFail=False
        }
#endif
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
      ]
#endif
    ]
  , clashTestGroup "tests"
    [ clashTestGroup "shouldfail"
      [ clashTestGroup "BlackBox"
        [ runTest "WrongReference" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, Text.pack [I.i|
              Function WrongReference.myMultiply was annotated with an inline
              primitive for WrongReference.myMultiplyX. These names should be
              the same. |])
          }
        ]
      , clashTestGroup "InvalidPrimitive"
        [ runTest "InvalidPrimitive" def{
            hdlTargets=[VHDL]
          , clashFlags=["-itests/shouldfail/InvalidPrimitive"]
          , expectClashFail=Just (def, "InvalidPrimitive.json")
          }
        ]
      , clashTestGroup "GADTs"
        [ runTest "T1311" def {
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, Text.pack [I.i|
            Can't translate data types with unconstrained existentials|])
          }
        ]
      , clashTestGroup "PrimitiveGuards"
        [ runTest "DontTranslate" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, Text.pack [I.i|
              Clash was forced to translate 'DontTranslate.primitive', but this
              value was marked with DontTranslate. Did you forget to include a
              blackbox for one of the constructs using this?
            |])
          }
        , runTest "HasBlackBox" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, Text.pack [I.i|
              No BlackBox definition for 'HasBlackBox.primitive' even though
              this value was annotated with 'HasBlackBox'.
            |])
          }
        ]
      , clashTestGroup "Signal"
        [ runTest "MAC" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "Couldn't instantiate blackbox for Clash.Signal.Internal.register#")
          }
        ]
      , clashTestGroup "SynthesisAttributes"
        [ runTest "ProductInArgs" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "Attempted to split Product into a number of HDL ports.")
          }
        , runTest "ProductInResult" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "Attempted to split Product into a number of HDL ports.")
          }
        ]
      , clashTestGroup "TopEntity"
        [ runTest "T1033" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "PortProduct \"wrong\" []")
          }
        , runTest "T1063" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "Ports were annotated as product, but type wasn't one")
          }
        ]
      , clashTestGroup "Verification"
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
        [ let n = 9 in -- GHDL only has VERY basic PSL support
          runTest "NonTemporalPSL" def{
            hdlTargets=[VHDL]
          , entities=Entities [["fails" ++ show i] | i <- [(1::Int)..n]]
          , topEntities=TopEntities ["fails" ++ show i | i <- [(1::Int)..n]]
          , expectSimFail=Just (def, "psl assertion failed")
          }
        , let n = 13 in
          runTest "NonTemporalPSL" def{
            hdlTargets=[SystemVerilog]
          , entities=Entities [["fails" ++ show i] | i <- [(1::Int)..n]]
          , topEntities=TopEntities ["fails" ++ show i | i <- [(1::Int)..n]]
          -- Only QuestaSim supports simulating SVA/PSL, but ModelSim does check
          -- for syntax errors.
          , hdlSim=False
          }
#else
        [ let n = 13 in
          runTest "NonTemporalPSL" def{
            hdlTargets=[SystemVerilog]
          , entities=Entities [["fails" ++ show i] | i <- [(1::Int)..n]]
          , topEntities=TopEntities ["fails" ++ show i | i <- [(1::Int)..n]]
          -- Only QuestaSim supports simulating SVA/PSL, but ModelSim does check
          -- for syntax errors.
          , hdlSim=False
          }
#endif
        , let is = [(1::Int)..13] \\ [4, 6, 7, 8, 10, 11, 12] in
          runTest "NonTemporalSVA" def{
            hdlTargets=[SystemVerilog]
          , entities=Entities [["fails" ++ show i] | i <- is]
          , topEntities=TopEntities ["fails" ++ show i | i <- is]
          -- Only QuestaSim supports simulating SVA/PSL, but ModelSim does check
          -- for syntax errors.
          , hdlSim=False
          }
        ]
      , clashTestGroup "ZeroWidth"
        [ runTest "FailGracefully1" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "Unexpected projection of zero-width type")
          }
        , runTest "FailGracefully2" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "Unexpected projection of zero-width type")
          }
        , runTest "FailGracefully3" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "Unexpected projection of zero-width type")
          }
        ]
      , runTest "LiftRecursiveGroup" def{
          hdlTargets=[VHDL]
        , expectClashFail=Just (def,"Callgraph after normalization contains following recursive components:")
        }
      , runTest "Poly" def{
          hdlTargets=[VHDL]
        , expectClashFail=Just (def, "Clash can only normalize monomorphic functions, but this is polymorphic:")
        }
      , runTest "Poly2" def{
          hdlTargets=[VHDL]
        , clashFlags=["-fclash-error-extra"]
        , expectClashFail=Just (def, "Even after applying type equality constraints it remained polymorphic:")
        }
      , runTest "RecursiveBoxed" def{
          hdlTargets=[VHDL]
        , expectClashFail=Just (def, " already inlined 20 times in:RecursiveBoxed.topEntity")
        }
      , runTest "RecursiveDatatype" def{
          hdlTargets=[VHDL]
        , expectClashFail=Just (def, "This bndr has a non-representable return type and can't be normalized:")
        }
--        Disabled, due to it eating gigabytes of memory:
--      , runTest "RecursivePoly" def{
--          hdlTargets=[VHDL]
--        , expectClashFail=Just (def, "??")
--        }
      ]
    , clashTestGroup "shouldwork"
#if !EXPERIMENTAL_EVALUATOR
      [ clashTestGroup "AutoReg"
        [ outputTest ("tests" </> "shouldwork" </> "AutoReg") allTargets [] [] "AutoReg" "main"
        ]
      , clashTestGroup "Basic"
#else
      [ clashTestGroup "Basic"
#endif

#if !EXPERIMENTAL_EVALUATOR
        [ runTest "AES" def{hdlSim=False}
        , runTest "BangData" def{hdlSim=False}
        , runTest "Trace" def{hdlSim=False}
#elif __GLASGOW_HASKELL__ >= 865
        [ runTest "BangData" def{hdlSim=False}
        , runTest "Trace" def{hdlSim=False}
#else
        [ runTest "Trace" def{hdlSim=False}
#endif
#if !EXPERIMENTAL_EVALUATOR
        , runTest "DivMod" def{hdlSim=False}
        , runTest "LambdaDrop" def{hdlSim=False}
#endif
        , runTest "IrrefError" def{hdlSim=False}
#ifdef CLASH_MULTIPLE_HIDDEN
        , runTest "MultipleHidden" def
#endif
        , outputTest ("tests" </> "shouldwork" </> "Basic") allTargets [] [] "NameInlining" "main"
        , runTest "NameInstance" def{hdlSim=False}
        , outputTest ("tests" </> "shouldwork" </> "Basic") allTargets [] [] "NameInstance" "main"
        , outputTest ("tests" </> "shouldwork" </> "Basic") [VHDL] [] [] "SetName" "main"
        , runTest "PatError" def{hdlSim=False}
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
        , runTest "ByteSwap32" def
        , runTest "CharTest" def
        , runTest "ClassOps" def
        , runTest "CountTrailingZeros" def
        , runTest "DeepseqX" def
        , runTest "LotOfStates" def
        , runTest "NameOverlap" def{
            entities=Entities [["nameoverlap"]]
          , topEntities=TopEntities ["nameoverlap"]
          , hdlSim=False
          }
        , runTest "NestedPrimitives" def{hdlSim=False}
        , runTest "NestedPrimitives2" def{hdlSim=False}
        , runTest "NORX" def
        , runTest "Parameters" def{hdlTargets=[VHDL]}
        , runTest "PopCount" def
        , runTest "RecordSumOfProducts" def{hdlSim=False}
        , runTest "Replace" def
        , runTest "T1242" def{hdlSim=False}
        , runTest "TestIndex" def{hdlSim=False}
        , runTest "Time" def
#endif
        , runTest "Shift" def{hdlSim=False}
        , runTest "SimpleConstructor" def{hdlSim=False}
        , runTest "TyEqConstraints" def{
            hdlSim=False
          , entities=Entities [["top1"]]
          , topEntities=TopEntities ["top1"]
          }
#if !EXPERIMENTAL_EVALUATOR
        , runTest "T1012" def{hdlSim=False}
        , runTest "T1240" def{hdlSim=False}
#endif
        , runTest "T1242" def{hdlSim=False}
        , runTest "T1254" def{hdlTargets=[VHDL,SystemVerilog],hdlSim=False}
        , runTest "T1292" def{hdlTargets=[VHDL]}
        , runTest "T1297" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "T1304" def{
            hdlTargets=[VHDL]
          , hdlLoad=False
          }
        , runTest "T1305" def{
            hdlTargets=[VHDL]
          , hdlSim=False
          , clashFlags=["-main-is", "plus"]
          , topEntities=TopEntities ["plus"]
          }
        , runTest "T1316" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "T1322" def{hdlTargets=[VHDL]}
        , runTest "T1340" def{hdlTargets=[VHDL], hdlSim=False}
#if MIN_VERSION_ghc(8,6,1)
          -- GHC 8.4 doesn't constant fold constructs on naturals. This tricks
          -- Clash into thinking binders variables aren't constant, while in
          -- reality the are. A proper solution would be to:
          --
          --   1. Normalize any global binders applied to constant-only arguments
          --      before finishing normalizing binders they're used in.
          --   2. Implement a proper partial evaluator.
          --
          -- As (2) is in the works, we've decided to not persue (1) for now and
          -- simply advice users encountering this bug to use >8.4.
        , runTest "T1354A" def{hdlTargets=[VHDL], hdlSim=False}
#endif
        , runTest "T1354B" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "T1402" def{clashFlags=["-O"]}
        , runTest "T1402b" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "TagToEnum" def{hdlSim=False}
        , runTest "TwoFunctions" def{hdlSim=False}
        ]
#if !EXPERIMENTAL_EVALUATOR
      , clashTestGroup "BitVector"
        [ runTest "Box" def
        , runTest "BoxGrow" def
        , runTest "CLZ" def
        , runTest "RePack" def{hdlSim=False}
        , runTest "ReduceZero" def
        , runTest "ReduceOne" def
        , runTest "ExtendingNumZero" def
        , runTest "AppendZero" def
        , runTest "GenericBitPack" def{clashFlags=["-fconstraint-solver-iterations=15"]}
        , runTest "UnpackUndefined" def{hdlSim=False}
        ]
#else
#if __GLASGOW_HASKELL__ >= 865
      , clashTestGroup "BitVector"
        [ runTest "Box" def
        , runTest "BoxGrow" def
        , runTest "CLZ" def
        , runTest "RePack" def{hdlSim=False}
        , runTest "ReduceZero" def
        , runTest "ReduceOne" def
        , runTest "ExtendingNumZero" def
        , runTest "AppendZero" def
        ]
#endif
#endif
      , clashTestGroup "BlackBox"
        [ outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "TemplateFunction"   "main"
        , outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "BlackBoxFunction"   "main"
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
        , runTest "BlackBoxFunctionHO" def{hdlTargets=[VHDL]}
        , outputTest ("tests" </> "shouldwork" </> "Signal")   allTargets [] [] "BlockRamLazy"       "main"
#endif
        , outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "ZeroWidth"          "main"
#if !EXPERIMENTAL_EVALUATOR
        , runTest "T919" def{hdlSim=False}
#endif
        ]
      , clashTestGroup "BoxedFunctions"
        [ runTest "DeadRecursiveBoxed" def{hdlSim=False}
        ]
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
      , clashTestGroup "CSignal"
        [ runTest "MAC" def{hdlSim=False}
        , runTest "CBlockRamTest" def{hdlSim=False}
        ]
#endif
#ifdef COSIM
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
      , clashTestGroup "CoSim"
        [ runTest "Multiply" def{hdlTargets=[Verilog]}
        , runTest "Register" def{hdlTargets=[Verilog]}
        ]
#endif
#endif
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
      , clashTestGroup "CustomReprs"
        [ clashTestGroup "RotateC"
          [ runTest "RotateC" def
          , runTest "ReprCompact" def
          , runTest "ReprCompactScrambled"   def
          , runTest "ReprLastBitConstructor" def
          , runTest "ReprStrangeMasks" def{hdlTargets=[VHDL,Verilog]}
          , runTest "ReprWide" def
          , runTest "RotateCScrambled" def
          ]
        , clashTestGroup "RotateCNested" [ runTest "RotateCNested" def ]
        , clashTestGroup "Rotate" [ runTest "Rotate" def ]
        , clashTestGroup "Deriving" [ runTest "BitPackDerivation" def ]
        , clashTestGroup "Indexed" [ runTest "Indexed" def ]

        , clashTestGroup "ZeroWidth"
          [ runTest "ZeroWidth" def{hdlSim=False}
          ]
        , runTest "T694" def{hdlSim=False,hdlTargets=[VHDL]}
        ]
#else
      , clashTestGroup "CustomReprs"
        [ clashTestGroup "ZeroWidth"
          [ runTest "ZeroWidth" def{hdlSim=False}
          ]
        , runTest "T694" def{hdlSim=False,hdlTargets=[VHDL]}
        ]
#endif
#if !EXPERIMENTAL_EVALUATOR
      , clashTestGroup "DDR"
        [ runTest "DDRinGA" def
        , runTest "DDRinGS" def
        , runTest "DDRinUA" def
        , runTest "DDRinUS" def
        , runTest "DDRoutUA" def
        , runTest "DDRoutUS" def
        , runTest "DDRoutGA" def
        , runTest "DDRoutGS" def
        ]
      , clashTestGroup "DSignal"
        [ runTest "DelayedFold" def
        , runTest "DelayI" def
        , runTest "DelayN" def
        ]
#endif
      , clashTestGroup "Feedback"
        [
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
          runTest "Fib" def
#endif
#ifdef CLASH_MULTIPLE_HIDDEN
        , runTest "MutuallyRecursive" def
#endif
        ]
      , clashTestGroup "Fixed"
#if !EXPERIMENTAL_EVALUATOR
        [ runTest "Mixer" def
        , runTest "SFixedTest" def
        , runTest "SatWrap" def{hdlSim=False}
        , runTest "ZeroInt" def
#else
        [ runTest "SatWrap" def{hdlSim=False}
#endif
        ]
      , clashTestGroup "Floating"
        [ runTest "FloatPack" def{hdlSim=False, clashFlags=["-fclash-float-support"]}
#if !EXPERIMENTAL_EVALUATOR
        , runTest "FloatConstFolding" def{clashFlags=["-fclash-float-support"]}
#endif
        ]
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
      , clashTestGroup "GADTs"
        [ runTest "Constrained" def
        , runTest "Head" def
        , runTest "HeadM" def
        , runTest "MonomorphicTopEntity" def
        , runTest "Record" def
        , runTest "Tail" def
        , runTest "TailM" def
        , runTest "TailOfTail" def
        , runTest "T1310" def{hdlSim=False}
        ]
      , clashTestGroup "HOPrim"
        [ runTest "HOIdx" def
        , runTest "HOImap" def
        , runTest "Map" def
        , runTest "Map2" def
        , runTest "TestMap" def
        , runTest "Transpose" def
        , runTest "VecFun" def
      ]
#endif
#if !EXPERIMENTAL_EVALUATOR
      , clashTestGroup "Issues"
        [ runTest "T1187" def{hdlSim=False, hdlTargets=[Verilog]}
        , clashLibTest ("tests" </> "shouldwork" </> "Issues") [VHDL] [] "T1388" "main"
        , outputTest ("tests" </> "shouldwork" </> "Issues") allTargets [] [] "T1171" "main"
        ]
#endif
      , clashTestGroup "Naming"
        [ runTest "T967a" def{hdlSim=False}
        , runTest "T967b" def{hdlSim=False}
        , runTest "T967c" def{hdlSim=False}
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
        , clashLibTest ("tests" </> "shouldwork" </> "Naming") allTargets [] "T1041" "main"
#endif
        ]
#if !EXPERIMENTAL_EVALUATOR
      , clashTestGroup "Numbers"
        [ runTest "NegativeLits" def
#if MIN_VERSION_base(4,14,0)
        , runTest "BitReverse" def
#endif
        , runTest "Resize" def
        , runTest "Resize2" def
        , runTest "Resize3" def
        , runTest "SatMult" def{hdlSim=False}
        , runTest "ShiftRotate" def{clashFlags=["-itests/shouldwork/Numbers"]}
        , runTest "SignedProjectionTB" def
        , runTest "SignedZero" def
        , runTest "Signum" def
        , runTest "Strict" def
        , runTest "UnsignedZero" def
        , runTest "IntegralTB" def{clashFlags=["-itests/shouldwork/Numbers"]}
        , runTest "HalfAsBlackboxArg" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "BitInteger" def
        , runTest "ExpWithGhcCF" def{clashFlags=["-itests/shouldwork/Numbers", "-fconstraint-solver-iterations=15"]}
        , runTest "ExpWithClashCF" def{clashFlags=["-itests/shouldwork/Numbers", "-fconstraint-solver-iterations=15"]}
        , runTest "Bounds" def
        , runTest "DivideByZero" def
        , outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets ["-itests/shouldwork/Numbers"] ["-itests/shouldwork/Numbers"] "ExpWithClashCF"  "main"
        , runTest "NumConstantFoldingTB_1" def{clashFlags=["-itests/shouldwork/Numbers"]}
        , outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets ["-fconstraint-solver-iterations=15"] ["-itests/shouldwork/Numbers"] "NumConstantFolding_1"  "main"
        , runTest "NumConstantFoldingTB_2" def{clashFlags=["-itests/shouldwork/Numbers"]}
        , outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets ["-fconstraint-solver-iterations=15"] ["-itests/shouldwork/Numbers"] "NumConstantFolding_2"  "main"
#if MIN_VERSION_base(4,12,0)
        -- Naturals are broken on GHC <= 8.4. See https://github.com/clash-lang/clash-compiler/pull/473
        , runTest "Naturals" def
#endif
        ]
#else
      , clashTestGroup "Numbers"
        [
#if __GLASGOW_HASKELL__ >= 865
          runTest "NegativeLits" def
        , runTest "Resize" def
        , runTest "Resize2" def
        , runTest "Resize3" def
        , runTest "SatMult" def{hdlSim=False}
        , runTest "ShiftRotate" def{clashFlags=["-itests/shouldwork/Numbers"]}
        , runTest "SignedProjectionTB" def
        , runTest "SignedZero" def
        , runTest "Signum" def
        , runTest "Strict" def
#if !EXPERIMENTAL_EVALUATOR
        , runTest "T1019" def{hdlSim=False}
        , runTest "T1351" def
        , outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets [] ["-itests/shouldwork/Numbers"] "UndefinedConstantFolding"  "main"
#endif
        , runTest "UnsignedZero" def
        , runTest "IntegralTB" def{clashFlags=["-itests/shouldwork/Numbers"]}
        , runTest "HalfAsBlackboxArg" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "BitInteger" def
        , runTest "ExpWithGhcCF" def{clashFlags=["-itests/shouldwork/Numbers", "-fconstraint-solver-iterations=15"]}
        , runTest "ExpWithClashCF" def{clashFlags=["-itests/shouldwork/Numbers", "-fconstraint-solver-iterations=15"]}
        -- Naturals are broken on GHC <= 8.4. See https://github.com/clash-lang/clash-compiler/pull/473
        , runTest "Naturals" def
#endif
        ]
#endif
      , clashTestGroup "Polymorphism"
        [ runTest "ExistentialBoxed" def{hdlSim=False}
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
        , runTest "FunctionInstances" def
#endif
        , runTest "GADTExistential" def{hdlSim=False}
        , runTest "LocalPoly" def{hdlSim=False}
        ]
      , clashTestGroup "PrimitiveGuards"
        [ runTest "WarnAlways" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (NoTestExitCode, "You shouldn't use 'primitive'!")
          }
        ]
#if !EXPERIMENTAL_EVALUATOR
      , clashTestGroup "PrimitiveReductions"
        [ runTest "Lambda" def
        , runTest "ReplaceInt" def
        ]
#endif
      , clashTestGroup "RTree"
        [ runTest "TZip" def{hdlSim=False}
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
        , runTest "TFold" def{hdlSim=False}
        , runTest "TRepeat" def
        , runTest "TRepeat2" def
#endif
        ]
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
      , clashTestGroup "Shadowing"
        [ runTest "T990" def
        ]
#endif
      , clashTestGroup "Signal"
        [ runTest "AlwaysHigh" def{hdlSim=False}
#if !EXPERIMENTAL_EVALUATOR
        , runTest "BlockRamFile" def
        , runTest "BlockRam0" def
        , runTest "BlockRam1" def
        , runTest "Ram" def
        , runTest "ResetGen" def
        , runTest "RomFile" def
#endif
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
        , outputTest ("tests" </> "shouldwork" </> "Signal") allTargets [] [] "BlockRamLazy"    "main"
        , runTest "BlockRamTest" def{hdlSim=False}
        , runTest "Compression" def
        , runTest "DelayedReset" def
        , runTest "NoCPR" def{
            entities=Entities [["example"]]
          , topEntities=TopEntities ["example"]
          , hdlSim=False
          }
        , runTest "Oversample" def
        , runTest "RegisterAR" def
        , runTest "RegisterSR" def
        , runTest "RegisterAE" def
        , runTest "RegisterSE" def
        , runTest "ResetLow" def
        , runTest "Rom" def
#endif
        , runTest "SigP" def{hdlSim=False}
        , outputTest ("tests" </> "shouldwork" </> "Signal") [VHDL] [] [] "T1102A" "main"
        , outputTest ("tests" </> "shouldwork" </> "Signal") [VHDL] [] [] "T1102B" "main"

#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
        , clashTestGroup "BiSignal"
          [ runTest "Counter" def
          , runTest "CounterHalfTuple" def
          , runTest "CounterHalfTupleRev" def
          ]
#endif
        , runTest "T1007" def{hdlSim=False}
        ]
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
      , clashTestGroup "SimIO"
        [ runTest "Test00" def {
            hdlTargets=[Verilog]
          , vvpStderrEmptyFail=False
          , topEntities=TopEntities ["topEntity"]
          }
        ]
      , clashTestGroup "SynthesisAttributes"
        [ outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") allTargets [] [] "Simple"  "main"
        , outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") allTargets [] [] "Product" "main"
        , runTest "Product" def
        ]
      , clashTestGroup "Testbench"
        [ runTest "TB" def{clashFlags=["-fclash-inline-limit=0"]}
        , runTest "SyncTB" def
        ]
#endif
      , clashTestGroup "Types"
        [ runTest "TypeFamilyReduction" def{hdlSim=False}
#if !EXPERIMENTAL_EVALUATOR
        , runTest "NatExp" def{hdlSim=False}
#endif
        ]
      , clashTestGroup "TopEntity"
        -- VHDL tests disabled for now: I can't figure out how to generate a static name whilst retaining the ability to actually test..
        [ outputTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] [] "PortGeneration" "main"
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithSingletonVector" "main"
        , runTest "TopEntHOArg" def{entities=Entities [["f", "g"]], topEntities=TopEntities ["f"], hdlSim=False}
        , runTest "T701" def {hdlSim=False,entities=Entities [["mynot", ""]]}
        , runTest "T1033" def {hdlSim=False,entities=Entities [["top", ""]], topEntities=TopEntities ["top"]}
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] [] "T1033" "main"
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] [] "T1072" "main"
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] [] "T1074" "main"
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [SystemVerilog] ["-main-is", "topEntity1"] [] "Multiple" "main1"
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [VHDL] ["-main-is", "topEntity3"] [] "Multiple" "main3"
        , runTest "T1139" def{hdlSim=False}
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
        , runTest "PortNames" def{hdlTargets=[Verilog],entities=Entities [["", "PortNames_topEntity", "PortNames_testBench"]], topEntities=TopEntities ["PortNames_testBench"]}
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNames" "main"
        , runTest "PortProducts" def{hdlTargets=[Verilog],entities=Entities [["", "PortProducts_topEntity", "PortProducts_testBench"]], topEntities=TopEntities ["PortProducts_testBench"]}
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortProducts" "main"
        , runTest "PortProductsSum" def{hdlTargets=[Verilog],entities=Entities [["", "PortProductsSum_topEntity", "PortProductsSum_testBench"]], topEntities=TopEntities ["PortProductsSum_testBench"]}
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortProductsSum" "main"
        , runTest "PortNamesWithUnit" def{hdlTargets=[Verilog],entities=Entities [["", "PortNamesWithUnit_topEntity", "PortNamesWithUnit_testBench"]], topEntities=TopEntities ["PortNamesWithUnit_testBench"]}
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithUnit" "main"
        , runTest "PortNamesWithVector" def{hdlTargets=[Verilog],entities=Entities [["", "PortNamesWithVector_topEntity", "PortNamesWithVector_testBench"]], topEntities=TopEntities ["PortNamesWithVector_testBench"]}
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithVector" "main"
        , runTest "PortNamesWithRTree" def{hdlTargets=[Verilog],entities=Entities [["", "PortNamesWithRTree_topEntity", "PortNamesWithRTree_testBench"]], topEntities=TopEntities ["PortNamesWithRTree_testBench"]}
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithRTree" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] "T1182A" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] "T1182B" "main"
#endif
        ]
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
      , clashTestGroup "Unit"
        [ runTest "Imap" def
        , runTest "ZipWithUnitVector" def
        , runTest "ZipWithTupleWithUnitLeft" def
        , runTest "ZipWithTupleWithUnitRight" def
        , runTest "ZipWithTripleWithUnitMiddle" def
        , runTest "ZipWithUnitSP" def
        , runTest "ZipWithUnitSP2" def
        ]
#endif
      , clashTestGroup "Vector"
        [ runTest "EnumTypes" def{hdlSim=False}
        , runTest "HOCon" def{hdlSim=False}
        , runTest "VMapAccum" def{hdlSim=False}
        , runTest "VScan" def{hdlSim=False}
        , runTest "VZip" def{hdlSim=False}
        , runTest "VecConst" def{hdlSim=False}
#if !EXPERIMENTAL_EVALUATOR
        , runTest "FirOddSize" def
        , runTest "IndexInt" def
#endif
#if !EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
        , runTest "Concat" def
        , runTest "DFold" def
        , runTest "DFold2" def
        , runTest "DTFold" def
        , runTest "FindIndex" def
        , runTest "Fold" def
        , runTest "FoldlFuns" def{hdlSim=False}
        , runTest "Foldr" def
        , runTest "FoldrEmpty" def
        , runTest "HOClock" def{hdlSim=False}
        , runTest "HOPrim" def{hdlSim=False}
        , runTest "Indices" def
        , runTest "Iterate" def
        , outputTest ("tests" </> "shouldwork" </> "Vector") [VHDL] [] [] "IterateCF" "main"
        , runTest "Minimum" def
        , runTest "MovingAvg" def{hdlSim=False}
        , runTest "PatHOCon" def{hdlSim=False}
        , runTest "Scatter" def
        , runTest "Split" def{hdlSim=False}
        , runTest "ToList" def
        , runTest "Unconcat" def
        , runTest "VACC" def{hdlSim=False}
        , runTest "VEmpty" def
        , runTest "VIndex" def{hdlSim=False}
        , runTest "VIndicesI" def
        , runTest "VFold" def
        , runTest "VMerge" def
        , runTest "VReplace" def
        , runTest "VReverse" def
        , runTest "VRotate" def
        , runTest "VSelect" def
        , runTest "VecOfSum" def{hdlSim=False}
        , runTest "T452" def{hdlSim=False}
        , runTest "T895" def{hdlSim=False,hdlTargets=[VHDL]}
        , runTest "T1360" def{hdlSim=False, hdlTargets=[VHDL], clashFlags=["-fclash-hdlsyn", "Vivado"]}
#endif
        ] -- end vector
      , clashTestGroup "XOptimization"
#if !EXPERIMENTAL_EVALUATOR
        [ outputTest  ("tests" </> "shouldwork" </> "XOptimization") allTargets [] [] "Conjunction"  "main"
        , outputTest  ("tests" </> "shouldwork" </> "XOptimization") allTargets [] [] "Disjunction"  "main"
        , clashLibTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "OneDefinedDataPat" "main"
#else
        [ clashLibTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "OneDefinedDataPat" "main"
#endif
        , clashLibTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "OneDefinedLitPat" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "OneDefinedDefaultPat" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "ManyDefined" "main"
        ]
      ] -- end shouldwork
    ] -- end tests
  ] -- end .

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
