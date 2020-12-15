{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import qualified Clash.Util.Interpolate    as I

import           Control.Exception         (finally)
import qualified Data.Text                 as Text
import           Data.Default              (def)
import           Data.List                 (isSuffixOf)
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

-- Define these dummy macros so the rest of the file can match master more closely,
-- creating less conflicts when backporting stuff.
#define NEEDS_PRIMS(x) (x)
#define NEEDS_PRIMS_GHC(x) (x)

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
    [ netlistTest ("tests" </> "shouldwork" </> "Netlist") allTargets [] "Identity" "main"
    , NEEDS_PRIMS(netlistTest ("tests" </> "shouldwork" </> "Netlist") [VHDL] [] "NoDeDup" "main")
    ]
  , clashTestGroup "examples"
    [ runTest "ALU" def{hdlSim=False}
    , let _opts = def { hdlSim=False
                      , hdlTargets=[VHDL]
                      , entities=Entities [["blinker"]]
                      , topEntities=TopEntities ["blinker"]
                      }
       in NEEDS_PRIMS_GHC(runTest "Blinker" _opts)
    , NEEDS_PRIMS_GHC (runTest "BlockRamTest" def{hdlSim=False})
    , NEEDS_PRIMS_GHC(runTest "Calculator" def)
    , NEEDS_PRIMS_GHC(runTest "CHIP8" def{hdlSim=False})
    , NEEDS_PRIMS_GHC(runTest "CochleaPlus" def{hdlSim=False})
    , let _opts = def { clashFlags=["-fclash-component-prefix", "test"]
                      , entities=Entities [["","test_testBench"]]
                      , topEntities=TopEntities ["test_testBench"]
                      }
       in NEEDS_PRIMS(runTest "FIR" _opts)
    , NEEDS_PRIMS_GHC(runTest "Fifo" def{hdlSim=False})
    , NEEDS_PRIMS_GHC(runTest "MAC" def)
    , NEEDS_PRIMS_GHC(runTest "MatrixVect" def)
    , NEEDS_PRIMS_GHC(runTest "Queens" def{hdlSim=False})
    , NEEDS_PRIMS_GHC(runTest "Reducer" def{hdlSim=False})
    , NEEDS_PRIMS_GHC(runTest "Sprockell" def{hdlSim=False})
    , NEEDS_PRIMS_GHC(runTest "Windows" def{hdlSim=False})
    , clashTestGroup "crc32"
        [ NEEDS_PRIMS_GHC(runTest "CRC32" def)
        ]
    , clashTestGroup "i2c"
        [ let _opts = def { clashFlags=["-O2","-fclash-component-prefix","test"]
                        , entities=Entities [["test_i2c","test_bitmaster","test_bytemaster"]]
                        , topEntities=TopEntities ["test_i2c"]
                        , hdlSim=False
                        }
           in NEEDS_PRIMS_GHC(runTest "I2C" _opts)
        , let _opts = def { entities = Entities [[ ".." </> "I2C" </> "i2c"
                                                 , ".." </> "I2C" </> "bitmaster"
                                                 , ".." </> "I2C" </> "bytemaster"
                                                 , "configi2c"
                                                 , "slave"
                                                 , "system"
                                                 ]]
                          , topEntities = TopEntities ["system"]
                          , hdlTargets = [Verilog]
                          , hdlSim = True
                          , vvpStderrEmptyFail = False
                          }
           in NEEDS_PRIMS(runTest "I2Ctest" _opts)
        ]
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
      [ clashTestGroup "AutoReg"
        [ NEEDS_PRIMS(outputTest ("tests" </> "shouldwork" </> "AutoReg") allTargets [] [] "AutoReg" "main")
        , NEEDS_PRIMS(runTest "T1507" def{hdlSim=False})
        ]
      , clashTestGroup "Basic"
        [ NEEDS_PRIMS(runTest "AES" def{hdlSim=False})
        , NEEDS_PRIMS(runTest "BangData" def{hdlSim=False})
        , runTest "Trace" def{hdlSim=False}
        , NEEDS_PRIMS(runTest "DivMod" def{hdlSim=False})
        , NEEDS_PRIMS(runTest "LambdaDrop" def{hdlSim=False})
        , runTest "IrrefError" def{hdlSim=False}
#ifdef CLASH_MULTIPLE_HIDDEN
        , runTest "MultipleHidden" def
#endif
        , outputTest ("tests" </> "shouldwork" </> "Basic") allTargets [] [] "NameInlining" "main"
        , runTest "NameInstance" def{hdlSim=False}
        , outputTest ("tests" </> "shouldwork" </> "Basic") allTargets [] [] "NameInstance" "main"
        , outputTest ("tests" </> "shouldwork" </> "Basic") [VHDL] [] [] "SetName" "main"
        , runTest "PatError" def{hdlSim=False}
        , NEEDS_PRIMS_GHC(runTest "ByteSwap32" def)
        , NEEDS_PRIMS_GHC(runTest "CharTest" def)
        , NEEDS_PRIMS_GHC(runTest "ClassOps" def)
        , NEEDS_PRIMS_GHC(runTest "CountTrailingZeros" def)
        , NEEDS_PRIMS_GHC(runTest "DeepseqX" def)
        , NEEDS_PRIMS_GHC(runTest "LotOfStates" def)
        , let _opts = def { entities = Entities [["nameoverlap"]]
                          , topEntities = TopEntities ["nameoverlap"]
                          , hdlSim = False
                          }
           in NEEDS_PRIMS_GHC(runTest "NameOverlap" _opts)
        , NEEDS_PRIMS_GHC(runTest "NestedPrimitives" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "NestedPrimitives2" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "NORX" def)
        , NEEDS_PRIMS_GHC(runTest "Parameters" def{hdlTargets=[VHDL]})
        , NEEDS_PRIMS_GHC(runTest "PopCount" def)
        , NEEDS_PRIMS_GHC(runTest "RecordSumOfProducts" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "Replace" def)
        , NEEDS_PRIMS_GHC(runTest "TestIndex" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "Time" def)
        , runTest "Shift" def{hdlSim=False}
        , runTest "SimpleConstructor" def{hdlSim=False}
        , runTest "TyEqConstraints" def{
            hdlSim=False
          , entities=Entities [["top1"]]
          , topEntities=TopEntities ["top1"]
          }
        , NEEDS_PRIMS(runTest "T1012" def{hdlSim=False})
        , NEEDS_PRIMS(runTest "T1240" def{hdlSim=False})
        , let _opts = def {hdlTargets = [VHDL], hdlSim = False}
           in NEEDS_PRIMS(runTest "T1297" _opts)
        , runTest "T1254" def{hdlTargets=[VHDL,SystemVerilog],hdlSim=False}
        , NEEDS_PRIMS_GHC(runTest "T1242" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "T1292" def{hdlTargets=[VHDL]})
        , let _opts = def { hdlTargets = [VHDL], hdlLoad = False }
           in NEEDS_PRIMS_GHC(runTest "T1304" _opts)
        , let _opts = def { hdlTargets=[VHDL]
                          , hdlSim=False
                          , clashFlags=["-main-is", "plus"]
                          , topEntities=TopEntities ["plus"]
                          }
           in NEEDS_PRIMS_GHC(runTest "T1305" _opts)
        , let _opts = def {hdlTargets = [VHDL], hdlSim = False}
           in NEEDS_PRIMS_GHC(runTest "T1316" _opts)
        , NEEDS_PRIMS_GHC(runTest "T1322" def{hdlTargets=[VHDL]})
        , let _opts = def {hdlTargets = [VHDL], hdlSim = False}
           in NEEDS_PRIMS_GHC(runTest "T1340" _opts)

-- Following two tests fail after reverting:
--
--   https://github.com/clash-lang/clash-compiler/pull/1354
--
-- See:
--
--   https://github.com/clash-lang/clash-compiler/pull/XXXX
--
-- TODO: Enable tests.
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
--         , let _opts = def { hdlTargets = [VHDL], hdlSim = False}
--            in NEEDS_PRIMS_GHC(runTest "T1354A" _opts)
#endif
--         , let _opts = def { hdlTargets = [VHDL], hdlSim = False}
--            in NEEDS_PRIMS_GHC(runTest "T1354B" _opts)
        , runTest "T1402" def{clashFlags=["-O"]}
        , runTest "T1402b" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "T1591" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "TagToEnum" def{hdlSim=False}
        , runTest "TwoFunctions" def{hdlSim=False}
        ]
      , clashTestGroup "BitVector"
        [ NEEDS_PRIMS_GHC(runTest "Box" def)
        , NEEDS_PRIMS_GHC(runTest "BoxGrow" def)
        , NEEDS_PRIMS_GHC(runTest "CLZ" def)
        , NEEDS_PRIMS_GHC(runTest "RePack" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "ReduceZero" def)
        , NEEDS_PRIMS_GHC(runTest "ReduceOne" def)
        , NEEDS_PRIMS_GHC(runTest "ExtendingNumZero" def)
        , NEEDS_PRIMS_GHC(runTest "AppendZero" def)
        , NEEDS_PRIMS(runTest "GenericBitPack" def{clashFlags=["-fconstraint-solver-iterations=15"]})
        , NEEDS_PRIMS(runTest "UnpackUndefined" def{hdlSim=False})
        ]
      , clashTestGroup "BlackBox"
        [ outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "TemplateFunction"   "main"
        , outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "BlackBoxFunction"   "main"
        , NEEDS_PRIMS_GHC(runTest "BlackBoxFunctionHO" def{hdlTargets=[VHDL]})
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "Signal") allTargets [] [] "BlockRamLazy" "main")
        , outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "ZeroWidth"          "main"
        , NEEDS_PRIMS(runTest "T919" def{hdlSim=False})
        , NEEDS_PRIMS(runTest "T1524" def)
        ]
      , clashTestGroup "BoxedFunctions"
        [ runTest "DeadRecursiveBoxed" def{hdlSim=False}
        ]
      , clashTestGroup "CSignal"
        [ NEEDS_PRIMS_GHC(runTest "MAC" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "CBlockRamTest" def{hdlSim=False})
        ]
#ifdef COSIM
      , clashTestGroup "CoSim"
        [ NEEDS_PRIMS_GHC(runTest "Multiply" def{hdlTargets=[Verilog]})
        , NEEDS_PRIMS_GHC(runTest "Register" def{hdlTargets=[Verilog]})
        ]
#endif
      , clashTestGroup "CustomReprs"
        [ clashTestGroup "RotateC"
          [ NEEDS_PRIMS_GHC(runTest "RotateC" def)
          , NEEDS_PRIMS_GHC(runTest "ReprCompact" def)
          , NEEDS_PRIMS_GHC(runTest "ReprCompactScrambled"   def)
          , NEEDS_PRIMS_GHC(runTest "ReprLastBitConstructor" def)
          , let _opts = def { hdlTargets = [VHDL, Verilog] }
             in NEEDS_PRIMS_GHC(runTest "ReprStrangeMasks" _opts)
          , NEEDS_PRIMS_GHC(runTest "ReprWide" def)
          , NEEDS_PRIMS_GHC(runTest "RotateCScrambled" def)
          ]
        , clashTestGroup "RotateCNested"
          [ NEEDS_PRIMS_GHC(runTest "RotateCNested" def)
          ]
        , clashTestGroup "Rotate"
          [ NEEDS_PRIMS_GHC(runTest "Rotate" def)
          ]
        , clashTestGroup "Deriving"
          [ NEEDS_PRIMS_GHC(runTest "BitPackDerivation" def)
          ]
        , clashTestGroup "Indexed"
          [ NEEDS_PRIMS_GHC(runTest "Indexed" def)
          ]
        , clashTestGroup "ZeroWidth"
          [ runTest "ZeroWidth" def{hdlSim=False}
          ]
        , runTest "T694" def{hdlSim=False,hdlTargets=[VHDL]}
        ]
      , clashTestGroup "DDR"
        [ NEEDS_PRIMS(runTest "DDRinGA" def)
        , NEEDS_PRIMS(runTest "DDRinGS" def)
        , NEEDS_PRIMS(runTest "DDRinUA" def)
        , NEEDS_PRIMS(runTest "DDRinUS" def)
        , NEEDS_PRIMS(runTest "DDRoutUA" def)
        , NEEDS_PRIMS(runTest "DDRoutUS" def)
        , NEEDS_PRIMS(runTest "DDRoutGA" def)
        , NEEDS_PRIMS(runTest "DDRoutGS" def)
        ]
      , clashTestGroup "DSignal"
        [ NEEDS_PRIMS(runTest "DelayedFold" def)
        , NEEDS_PRIMS(runTest "DelayI" def)
        , NEEDS_PRIMS(runTest "DelayN" def)
        ]
      , clashTestGroup "Feedback"
        [ NEEDS_PRIMS_GHC(runTest "Fib" def)
#ifdef CLASH_MULTIPLE_HIDDEN
        , runTest "MutuallyRecursive" def
#endif
        ]
      , clashTestGroup "Fixed"
        [ NEEDS_PRIMS(runTest "Mixer" def)
        , NEEDS_PRIMS(runTest "SFixedTest" def)
        , NEEDS_PRIMS(runTest "SatWrap" def{hdlSim=False})
        , NEEDS_PRIMS(runTest "ZeroInt" def)
        ]
      , clashTestGroup "Floating"
        [ runTest "FloatPack" def{hdlSim=False, clashFlags=["-fclash-float-support"]}
        , NEEDS_PRIMS(runTest "FloatConstFolding" def{clashFlags=["-fclash-float-support"]})
        ]
      , clashTestGroup "GADTs"
        [ NEEDS_PRIMS_GHC(runTest "Constrained" def)
        , NEEDS_PRIMS_GHC(runTest "Head" def)
        , NEEDS_PRIMS_GHC(runTest "HeadM" def)
        , NEEDS_PRIMS_GHC(runTest "MonomorphicTopEntity" def)
        , NEEDS_PRIMS_GHC(runTest "Record" def)
        , NEEDS_PRIMS_GHC(runTest "Tail" def)
        , NEEDS_PRIMS_GHC(runTest "TailM" def)
        , NEEDS_PRIMS_GHC(runTest "TailOfTail" def)
        , NEEDS_PRIMS_GHC(runTest "T1310" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "T1536" def{hdlSim=False})
        ]
      , clashTestGroup "HOPrim"
        [ NEEDS_PRIMS_GHC(runTest "HOIdx" def)
        , NEEDS_PRIMS_GHC(runTest "HOImap" def)
        , NEEDS_PRIMS_GHC(runTest "Map" def)
        , NEEDS_PRIMS_GHC(runTest "Map2" def)
        , NEEDS_PRIMS_GHC(runTest "TestMap" def)
        , NEEDS_PRIMS_GHC(runTest "Transpose" def)
        , NEEDS_PRIMS_GHC(runTest "VecFun" def)
      ]
      , clashTestGroup "Issues"
        [ let _opts = def { hdlSim = False, hdlTargets = [Verilog] }
           in NEEDS_PRIMS(runTest "T1187" _opts)
        , netlistTest ("tests" </> "shouldwork" </> "Issues") [VHDL] [] "T1388" "main"
        , netlistTest ("tests" </> "shouldwork" </> "Issues") [VHDL] [] "T1439" "main"
        ]
      , clashTestGroup "Naming"
        [ runTest "T967a" def{hdlSim=False}
        , runTest "T967b" def{hdlSim=False}
        , runTest "T967c" def{hdlSim=False}
        , NEEDS_PRIMS_GHC(netlistTest ("tests" </> "shouldwork" </> "Naming") allTargets [] "T1041" "main")
        ]
      , clashTestGroup "Numbers"
        [ NEEDS_PRIMS_GHC(runTest "BitInteger" def)
#if MIN_VERSION_base(4,14,0)
        , runTest "BitReverse" def
#endif
        , NEEDS_PRIMS(runTest "Bounds" def)
        , NEEDS_PRIMS(runTest "DivideByZero" def)
        , let _opts = def { clashFlags=["-itests/shouldwork/Numbers", "-fconstraint-solver-iterations=15"] }
           in NEEDS_PRIMS_GHC(runTest "ExpWithGhcCF" _opts)
        , let _opts = def { clashFlags=["-itests/shouldwork/Numbers", "-fconstraint-solver-iterations=15"] }
           in NEEDS_PRIMS_GHC(runTest "ExpWithClashCF" _opts)
        , NEEDS_PRIMS(outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets ["-itests/shouldwork/Numbers"] ["-itests/shouldwork/Numbers"] "ExpWithClashCF" "main")
        , let _opts = def { hdlTargets = [VHDL], hdlSim = False }
           in NEEDS_PRIMS_GHC(runTest "HalfAsBlackboxArg" _opts)
        , NEEDS_PRIMS_GHC(runTest "IntegralTB" def{clashFlags=["-itests/shouldwork/Numbers"]})
        , NEEDS_PRIMS(runTest "NumConstantFoldingTB_1" def{clashFlags=["-itests/shouldwork/Numbers"]})
        , NEEDS_PRIMS(outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets ["-fconstraint-solver-iterations=15"] ["-itests/shouldwork/Numbers"] "NumConstantFolding_1" "main")
        , NEEDS_PRIMS(runTest "NumConstantFoldingTB_2" def{clashFlags=["-itests/shouldwork/Numbers"]})
        , NEEDS_PRIMS(outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets ["-fconstraint-solver-iterations=15"] ["-itests/shouldwork/Numbers"] "NumConstantFolding_2" "main")
#if MIN_VERSION_base(4,12,0)
        -- Naturals are broken on GHC <= 8.4. See https://github.com/clash-lang/clash-compiler/pull/473
        , NEEDS_PRIMS_GHC(runTest "Naturals" def)
        , NEEDS_PRIMS_GHC(runTest "NaturalToInteger" def{hdlSim=False})
#endif
        , NEEDS_PRIMS_GHC(runTest "NegativeLits" def)
        , NEEDS_PRIMS_GHC(runTest "Resize" def)
        , NEEDS_PRIMS_GHC(runTest "Resize2" def)
        , NEEDS_PRIMS_GHC(runTest "Resize3" def)
        , NEEDS_PRIMS_GHC(runTest "SatMult" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "ShiftRotate" def{clashFlags=["-itests/shouldwork/Numbers"]})
        , NEEDS_PRIMS_GHC(runTest "SignedProjectionTB" def)
        , NEEDS_PRIMS_GHC(runTest "SignedZero" def)
        , NEEDS_PRIMS_GHC(runTest "Signum" def)
        , NEEDS_PRIMS_GHC(runTest "Strict" def)
        , NEEDS_PRIMS(runTest "T1019" def{hdlSim=False})
        , NEEDS_PRIMS(runTest "T1351" def)
        , NEEDS_PRIMS(outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets [] ["-itests/shouldwork/Numbers"] "UndefinedConstantFolding" "main")
        , NEEDS_PRIMS_GHC(runTest "UnsignedZero" def)
        ]
      , clashTestGroup "Polymorphism"
        [ runTest "ExistentialBoxed" def{hdlSim=False}
        , NEEDS_PRIMS_GHC(runTest "FunctionInstances" def)
        , runTest "GADTExistential" def{hdlSim=False}
        , runTest "LocalPoly" def{hdlSim=False}
        ]
      , clashTestGroup "PrimitiveGuards"
        [ runTest "WarnAlways" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (NoTestExitCode, "You shouldn't use 'primitive'!")
          }
        ]
      , clashTestGroup "PrimitiveReductions"
        [ NEEDS_PRIMS(runTest "Lambda" def)
        , NEEDS_PRIMS(runTest "ReplaceInt" def)
        ]
      , clashTestGroup "RTree"
        [ runTest "TZip" def{hdlSim=False}
        , NEEDS_PRIMS_GHC(runTest "TFold" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "TRepeat" def)
        , NEEDS_PRIMS_GHC(runTest "TRepeat2" def)
        ]
      , clashTestGroup "Shadowing"
        [ NEEDS_PRIMS_GHC(runTest "T990" def)
        ]
      , clashTestGroup "Signal"
        [ runTest "AlwaysHigh" def{hdlSim=False}
        , runTest "BangPatterns" def
        , NEEDS_PRIMS(runTest "BlockRamFile" def)
        , NEEDS_PRIMS(runTest "BlockRam0" def)
        , NEEDS_PRIMS(runTest "BlockRam1" def)
        , NEEDS_PRIMS(runTest "Ram" def)
        , NEEDS_PRIMS(runTest "ResetGen" def)
        , NEEDS_PRIMS(runTest "RomFile" def)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "Signal") allTargets [] [] "BlockRamLazy" "main")
        , NEEDS_PRIMS_GHC(runTest "BlockRamTest" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "Compression" def)
        , NEEDS_PRIMS_GHC(runTest "DelayedReset" def)
        , let _opts = def { entities=Entities [["example"]]
                          , topEntities=TopEntities ["example"]
                          , hdlSim=False
                          }
           in NEEDS_PRIMS_GHC(runTest "NoCPR" _opts)
        , NEEDS_PRIMS_GHC(runTest "Oversample" def)
        , NEEDS_PRIMS_GHC(runTest "RegisterAR" def)
        , NEEDS_PRIMS_GHC(runTest "RegisterSR" def)
        , NEEDS_PRIMS_GHC(runTest "RegisterAE" def)
        , NEEDS_PRIMS_GHC(runTest "RegisterSE" def)
        , NEEDS_PRIMS_GHC(runTest "ResetLow" def)
        , NEEDS_PRIMS_GHC(runTest "Rom" def)
        , runTest "SigP" def{hdlSim=False}
        , outputTest ("tests" </> "shouldwork" </> "Signal") [VHDL] [] [] "T1102A" "main"
        , outputTest ("tests" </> "shouldwork" </> "Signal") [VHDL] [] [] "T1102B" "main"

        , clashTestGroup "BiSignal"
          [ NEEDS_PRIMS_GHC(runTest "Counter" def)
          , NEEDS_PRIMS_GHC(runTest "CounterHalfTuple" def)
          , NEEDS_PRIMS_GHC(runTest "CounterHalfTupleRev" def)
          ]
        , runTest "T1007" def{hdlSim=False}
        ]
      , clashTestGroup "SimIO"
        [ let _opts = def { hdlTargets=[Verilog]
                          , vvpStderrEmptyFail=False
                          , topEntities=TopEntities ["topEntity"]
                          }
           in NEEDS_PRIMS_GHC(runTest "Test00" _opts)
        ]
      , clashTestGroup "SynthesisAttributes"
        [ NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") allTargets [] [] "Simple" "main")
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") allTargets [] [] "Product" "main")
        , NEEDS_PRIMS_GHC(runTest "Product" def)
        ]
      , clashTestGroup "Testbench"
        [ NEEDS_PRIMS_GHC(runTest "TB" def{clashFlags=["-fclash-inline-limit=0"]})
        , NEEDS_PRIMS_GHC(runTest "SyncTB" def)
        ]
      , clashTestGroup "Types"
        [ runTest "TypeFamilyReduction" def{hdlSim=False}
        , NEEDS_PRIMS(runTest "NatExp" def{hdlSim=False})
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
        , let _opts = def { hdlTargets=[Verilog]
                          , entities=Entities [["", "PortNames_topEntity", "PortNames_testBench"]]
                          , topEntities=TopEntities ["PortNames_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortNames" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNames" "main")
        , let _opts = def { hdlTargets=[Verilog]
                          , entities=Entities [["", "PortProducts_topEntity", "PortProducts_testBench"]]
                          , topEntities=TopEntities ["PortProducts_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortProducts" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortProducts" "main")
        , let _opts = def { hdlTargets=[Verilog]
                          , entities=Entities [["", "PortProductsSum_topEntity", "PortProductsSum_testBench"]]
                          , topEntities=TopEntities ["PortProductsSum_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortProductsSum" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortProductsSum" "main")
        , let _opts = def { hdlTargets=[Verilog]
                          , entities=Entities [["", "PortNamesWithUnit_topEntity", "PortNamesWithUnit_testBench"]]
                          , topEntities=TopEntities ["PortNamesWithUnit_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortNamesWithUnit" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithUnit" "main")
        , let _opts = def { hdlTargets=[Verilog]
                          , entities=Entities [["", "PortNamesWithVector_topEntity", "PortNamesWithVector_testBench"]]
                          , topEntities=TopEntities ["PortNamesWithVector_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortNamesWithVector" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithVector" "main")
        , let _opts = def { hdlTargets=[Verilog]
                          , entities=Entities [["", "PortNamesWithRTree_topEntity", "PortNamesWithRTree_testBench"]]
                          , topEntities=TopEntities ["PortNamesWithRTree_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortNamesWithRTree" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithRTree" "main")
        , NEEDS_PRIMS_GHC(netlistTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] "T1182A" "main")
        , NEEDS_PRIMS_GHC(netlistTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] "T1182B" "main")
        ]
      , clashTestGroup "Unit"
        [ NEEDS_PRIMS_GHC(runTest "Imap" def)
        , NEEDS_PRIMS_GHC(runTest "ZipWithUnitVector" def)
        , NEEDS_PRIMS_GHC(runTest "ZipWithTupleWithUnitLeft" def)
        , NEEDS_PRIMS_GHC(runTest "ZipWithTupleWithUnitRight" def)
        , NEEDS_PRIMS_GHC(runTest "ZipWithTripleWithUnitMiddle" def)
        , NEEDS_PRIMS_GHC(runTest "ZipWithUnitSP" def)
        , NEEDS_PRIMS_GHC(runTest "ZipWithUnitSP2" def)
        ]
      , clashTestGroup "Vector"
        [ runTest "EnumTypes" def{hdlSim=False}
        , runTest "HOCon" def{hdlSim=False}
        , runTest "VMapAccum" def{hdlSim=False}
        , runTest "VScan" def{hdlSim=False}
        , runTest "VZip" def{hdlSim=False}
        , runTest "VecConst" def{hdlSim=False}
        , NEEDS_PRIMS(runTest "FirOddSize" def)
        , NEEDS_PRIMS(runTest "IndexInt" def)
        , NEEDS_PRIMS_GHC(runTest "Concat" def)
        , NEEDS_PRIMS_GHC(runTest "DFold" def)
        , NEEDS_PRIMS_GHC(runTest "DFold2" def)
        , NEEDS_PRIMS_GHC(runTest "DTFold" def)
        , NEEDS_PRIMS_GHC(runTest "FindIndex" def)
        , NEEDS_PRIMS_GHC(runTest "Fold" def)
        , NEEDS_PRIMS_GHC(runTest "FoldlFuns" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "Foldr" def)
        , NEEDS_PRIMS_GHC(runTest "FoldrEmpty" def)
        , NEEDS_PRIMS_GHC(runTest "HOClock" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "HOPrim" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "Indices" def)
        , NEEDS_PRIMS_GHC(runTest "Minimum" def)
        , NEEDS_PRIMS_GHC(runTest "MovingAvg" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "PatHOCon" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "Scatter" def)
        , NEEDS_PRIMS_GHC(runTest "Split" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "ToList" def)
        , NEEDS_PRIMS_GHC(runTest "Unconcat" def)
        , NEEDS_PRIMS_GHC(runTest "VACC" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "VEmpty" def)
        , NEEDS_PRIMS_GHC(runTest "VIndex" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "VIndicesI" def)
        , NEEDS_PRIMS_GHC(runTest "VFold" def)
        , NEEDS_PRIMS_GHC(runTest "VMerge" def)
        , NEEDS_PRIMS_GHC(runTest "VReplace" def)
        , NEEDS_PRIMS_GHC(runTest "VReverse" def)
        , NEEDS_PRIMS_GHC(runTest "VRotate" def)
        , NEEDS_PRIMS_GHC(runTest "VSelect" def)
        , NEEDS_PRIMS_GHC(runTest "VecOfSum" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "T452" def{hdlSim=False})
        , let _opts = def { hdlSim = False, hdlTargets = [VHDL]}
           in NEEDS_PRIMS_GHC(runTest "T895" _opts)
        , let _opts = def { hdlSim = False, hdlTargets = [VHDL], clashFlags = ["-fclash-hdlsyn", "Vivado"]}
           in NEEDS_PRIMS_GHC(runTest "T1360" _opts)
        ] -- end vector
      , clashTestGroup "XOptimization"
        [ NEEDS_PRIMS(outputTest  ("tests" </> "shouldwork" </> "XOptimization") allTargets [] [] "Conjunction" "main")
        , NEEDS_PRIMS(outputTest  ("tests" </> "shouldwork" </> "XOptimization") allTargets [] [] "Disjunction" "main")
        , netlistTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "OneDefinedDataPat" "main"
        , netlistTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "OneDefinedLitPat" "main"
        , netlistTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "OneDefinedDefaultPat" "main"
        , netlistTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "ManyDefined" "main"
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
