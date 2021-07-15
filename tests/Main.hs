{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Clash.Util.Interpolate    as I

import           Clash.Annotations.Primitive (HDL(..))
import           Control.Exception         (finally)
import qualified Data.Text                 as Text
import           Data.Default              (def)
import           Data.List                 ((\\), intercalate)
import           Data.Version              (versionBranch)
import           System.Directory
  (createDirectoryIfMissing, removeDirectoryRecursive, getCurrentDirectory,
   doesDirectoryExist, makeAbsolute)
import           System.Environment
import           System.FilePath           ((</>))
import           System.Info
import           GHC.Conc                  (numCapabilities)
import           GHC.Stack
import           GHC.IO.Unsafe             (unsafePerformIO)
import           Text.Printf               (printf)

import           Test.Tasty
import           Test.Tasty.Common
import           Test.Tasty.Clash

#if EXPERIMENTAL_EVALUATOR
import           Test.Tasty.HUnit
#endif

-- We want to selectively disable these tests while certain primitives are not
-- implemented in the new evaluator. These macros are used to prevent the
-- testsuite becoming a mess of CPP blocks.
#if EXPERIMENTAL_EVALUATOR
#define NEEDS_PRIMS(x) (const $ testCase "DISABLED" (True @?= True))
#else
#define NEEDS_PRIMS(x) (x)
#endif

#if EXPERIMENTAL_EVALUATOR || __GLASGOW_HASKELL__ >= 865
#define NEEDS_PRIMS_GHC(x) (NEEDS_PRIMS(x))
#else
#define NEEDS_PRIMS_GHC(x) (x)
#endif

-- | GHC version as major.minor.patch1. For example: 8.10.2.
ghcVersion3 :: String
ghcVersion3 =
#ifdef __GLASGOW_HASKELL_PATCHLEVEL2__
  let ghc_p1 = __GLASGOW_HASKELL_PATCHLEVEL1__
      ghc_p2 = __GLASGOW_HASKELL_PATCHLEVEL2__ in
  intercalate "." (map show (versionBranch compilerVersion <> [ghc_p1,ghc_p2]))
#else
  let ghc_p1 = __GLASGOW_HASKELL_PATCHLEVEL1__ in
  intercalate "." (map show (versionBranch compilerVersion <> [ghc_p1]))
#endif

-- Directory clash binary is expected to live in
cabalClashBinDir :: IO String
cabalClashBinDir = makeAbsolute rel_path
 where
  rel_path = printf templ platform ghcVersion3 (VERSION_clash_ghc :: String)
  platform = "x86_64-linux" :: String -- XXX: Hardcoded
  templ = "dist-newstyle/build/%s/ghc-%s/clash-ghc-%s/x/clash/build/clash/" :: String

-- | Set GHC_PACKAGE_PATH for local Cabal install. Currently hardcoded for Unix.
setCabalPackagePaths :: IO ()
setCabalPackagePaths = do
  home <- getEnv "HOME"
  here <- getCurrentDirectory
  setEnv "GHC_PACKAGE_PATH" $
       home <> "/.cabal/store/ghc-" <> ghcVersion3 <> "/package.db"
    <> ":"
    <> here <> "/dist-newstyle/packagedb/ghc-" <> ghcVersion3
    <> ":"

-- | See 'compiledWith'
data RunWith
  = Stack
  | Cabal
  | Global
  deriving (Show, Eq)

-- | Detects Clash binary the testsuite should use (in order):
--
--     * If USE_GLOBAL_CLASH=1, use globally installed Clash
--     * If STACK_EXE is present, use Stack's Clash
--     * If dist-newstyle is present, use Cabal's Clash
--     * Use globally installed Clash
--
compiledWith :: RunWith
compiledWith = unsafePerformIO $ do
  clash_global <- lookupEnv "USE_GLOBAL_CLASH"
  stack_exe <- lookupEnv "STACK_EXE"
  distNewstyleExists <- doesDirectoryExist "dist-newstyle"

  pure $ case (clash_global, stack_exe, distNewstyleExists) of
    (Just "1", Just _, _   ) -> error "Can't use global clash with 'stack run'"
    (Just "1", _,      _   ) -> Global
    (_,        Just _, _   ) -> Stack
    (_,        _     , True) -> Cabal
    (_,        _     , _   ) -> Global
{-# NOINLINE compiledWith #-}

-- | Set environment variables that allow Clash to be executed by simply calling
-- 'clash' without extra arguments.
setClashEnvs :: HasCallStack => RunWith -> IO ()
setClashEnvs Global = setEnv "GHC_ENVIRONMENT" "-"
setClashEnvs Stack = pure ()
setClashEnvs Cabal = do
  binDir <- cabalClashBinDir
  path <- getEnv "PATH"
  setEnv "PATH" (binDir <> ":" <> path)
  setCabalPackagePaths

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
    , NEEDS_PRIMS(clashLibTest ("tests" </> "shouldwork" </> "Netlist") [VHDL] [] "NoDeDup" "main")
    , clashLibTest ("tests" </> "shouldwork" </> "Netlist") allTargets [] "T1766" "main"
    ]
  , clashTestGroup "examples"
    [ runTest "ALU" def{hdlSim=False}
    , let _opts = def { hdlSim=False
                      , hdlTargets=[VHDL]
                      , buildTargets=BuildSpecific ["blinker"]
                      }
       in NEEDS_PRIMS_GHC(runTest "Blinker" _opts)
    , NEEDS_PRIMS_GHC (runTest "BlockRamTest" def{hdlSim=False})
    , NEEDS_PRIMS_GHC(runTest "Calculator" def)
    , NEEDS_PRIMS_GHC(runTest "CHIP8" def{hdlSim=False})
    , NEEDS_PRIMS_GHC(runTest "CochleaPlus" def{hdlSim=False})
    , let _opts = def { clashFlags=["-fclash-component-prefix", "test"]
                      , buildTargets=BuildSpecific ["test_testBench"]
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
                        , buildTargets=BuildSpecific ["test_i2c"]
                        , hdlSim=False
                        }
           in NEEDS_PRIMS_GHC(runTest "I2C" _opts)
        , let _opts = def { buildTargets = BuildSpecific ["system"]
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
          , expectClashFail=Just (def, "InvalidPrimitive.primitives")
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
          , expectClashFail=Just (def, "Cannot use attribute annotations on product types of top entities")
          }
        , runTest "ProductInResult" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "Cannot use attribute annotations on product types of top entities")
          }
        ]
      , clashTestGroup "TopEntity"
        [ runTest "T1033" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "PortProduct \"wrong\" []")
          }
        , runTest "T1063" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "Saw a PortProduct in a Synthesize annotation")
          }
        ]
      , clashTestGroup "Verification"
        [ let n = 9 -- GHDL only has VERY basic PSL support
              _opts = def { hdlTargets=[VHDL]
                          , buildTargets=BuildSpecific ["fails" <> show i | i <- [(1::Int)..n]]
                          , expectSimFail=Just (def, "psl assertion failed")
                          }
           in NEEDS_PRIMS_GHC(runTest "NonTemporalPSL" _opts)
        , let n = 13
              _opts = def { hdlTargets=[SystemVerilog]
                          , buildTargets=BuildSpecific ["fails" <> show i | i <- [(1::Int)..n]]
                          -- Only QuestaSim supports simulating SVA/PSL, but ModelSim does check
                          -- for syntax errors.
                          , hdlSim=False
                          }
           in NEEDS_PRIMS_GHC(runTest "NonTemporalPSL" _opts)
        , let is = [(1::Int)..13] \\ [4, 6, 7, 8, 10, 11, 12] in
          runTest "NonTemporalSVA" def{
            hdlTargets=[SystemVerilog]
          , buildTargets=BuildSpecific ["fails" <> show i | i <- is]
          -- Only QuestaSim supports simulating SVA/PSL, but ModelSim does check
          -- for syntax errors.
          , hdlSim=False
          }
        , runTest "SymbiYosys" def{
            hdlTargets=[Verilog, SystemVerilog]
          , buildTargets=BuildSpecific ["topEntity"]
          , hdlLoad=False
          , verificationTool=Just SymbiYosys
          , expectVerificationFail=Just (def, "Unreached cover statement at B")
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
        , expectClashFail=Just (def, " already inlined 20 times in: RecursiveBoxed.topEntity")
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
        , let _opts = def{hdlSim=False, hdlTargets=[VHDL]}
           in NEEDS_PRIMS(runTest "T1632" _opts)
        ]
      , clashTestGroup "Basic"
        [ NEEDS_PRIMS(runTest "AES" def{hdlSim=False})
        , NEEDS_PRIMS(runTest "BangData" def{hdlSim=False})
        , runTest "Trace" def{hdlSim=False}
        , NEEDS_PRIMS(runTest "DivMod" def{hdlSim=False})
        , NEEDS_PRIMS(runTest "DivZero" def)
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
        , let _opts = def { buildTargets = BuildSpecific ["nameoverlap"]
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
          , buildTargets=BuildSpecific ["top1"]
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
                          , buildTargets=BuildSpecific ["plus"]
                          }
           in NEEDS_PRIMS_GHC(runTest "T1305" _opts)
        , let _opts = def {hdlTargets = [VHDL], hdlSim = False}
           in NEEDS_PRIMS_GHC(runTest "T1316" _opts)
        , NEEDS_PRIMS_GHC(runTest "T1322" def{hdlTargets=[VHDL]})
        , let _opts = def {hdlTargets = [VHDL], hdlSim = False}
           in NEEDS_PRIMS_GHC(runTest "T1340" _opts)
        , let _opts = def { hdlTargets = [VHDL], hdlSim = False}
           in NEEDS_PRIMS_GHC(runTest "T1354A" _opts)
        , let _opts = def { hdlTargets = [VHDL], hdlSim = False}
           in NEEDS_PRIMS_GHC(runTest "T1354B" _opts)
        , runTest "T1402" def{clashFlags=["-O"]}
        , runTest "T1402b" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "T1556" def
        , runTest "T1591" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "TagToEnum" def{hdlSim=False}
        , runTest "TwoFunctions" def{hdlSim=False}
        , runTest "XToError" def{hdlSim=False}
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
        , outputTest ("tests" </> "shouldwork" </> "BlackBox") [VHDL]   [] [] "MultiResult"        "main"
        , NEEDS_PRIMS(runTest "MultiResult" def)
        , NEEDS_PRIMS(runTest "T919" def{hdlSim=False})
        , NEEDS_PRIMS(runTest "T1524" def)
        , runTest "T1786" def{
            hdlTargets=[VHDL]
          , buildTargets=BuildSpecific ["testEnableTB", "testBoolTB"]
          }
        ]
      , clashTestGroup "BoxedFunctions"
        [ runTest "DeadRecursiveBoxed" def{hdlSim=False}
        ]
      -- The Cores.Xilinx.Floating tests require Vivado (and take much time to
      -- run).
      --
      -- , clashTestGroup "Cores"
      --   [ clashTestGroup "Xilinx"
      --     [ runTest "Floating" def{ clashFlags=["-fclash-float-support"]
      --                             , buildTargets=[ "addBasicTB"
      --                                            , "addEnableTB"
      --                                            , "addShortPLTB"
      --                                            , "subBasicTB"
      --                                            , "mulBasicTB"
      --                                            , "divBasicTB"]}
      --     ]
      --   ]
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
        ]
      , clashTestGroup "CustomReprs"
        [ clashTestGroup "ZeroWidth"
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
        , runTest "T1803" def{clashFlags=["-fclash-float-support"]}
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
      , clashTestGroup "Issues" $
        [ let _opts = def { hdlSim = False, hdlTargets = [Verilog] }
           in NEEDS_PRIMS(runTest "T1187" _opts)
        , clashLibTest ("tests" </> "shouldwork" </> "Issues") [VHDL] [] "T1388" "main"
        , outputTest ("tests" </> "shouldwork" </> "Issues") allTargets [] [] "T1171" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "Issues") [VHDL] [] "T1439" "main"
        , runTest "T1477" def{hdlSim=False}
        , runTest "T1506A" def{hdlSim=False, clashFlags=["-fclash-aggressive-x-optimization-blackboxes"]}
        , outputTest ("tests" </> "shouldwork" </> "Issues") allTargets ["-fclash-aggressive-x-optimization-blackboxes"] ["-itests/shouldwork/Issues"] "T1506B" "main"
        , runTest "T1615" def{hdlSim=False, hdlTargets=[Verilog]}
        , runTest "T1663" def{hdlTargets=[VHDL], hdlSim=False}
        , runTest "T1669_DEC" def{hdlTargets=[VHDL]}
        , runTest "T1715" def
        , runTest "T1721" def{hdlSim=False}
        , runTest "T1606A" def{hdlSim=False}
        , runTest "T1606B" def{hdlSim=False}
        , runTest "T1742" def{hdlSim=False, buildTargets=BuildSpecific ["shell"]}
        , runTest "T1756" def{hdlSim=False}
        ] <>
        if compiledWith == Cabal then
          -- This tests fails without environment files present, which are only
          -- generated by Cabal. It complains it is trying to import "BasicTypes"
          -- which is a member of the hidden package 'ghc'. Passing
          -- '-package ghc' doesn't seem to help though. TODO: Investigate.
          [clashLibTest ("tests" </> "shouldwork" </> "Issues") allTargets [] "T1568" "main"]
        else
          []
      , clashTestGroup "LoadModules"
        [ runTest "T1796" def{hdlSim=False}
        ]
      , clashTestGroup "Naming"
        [ runTest "T967a" def{hdlSim=False}
        , runTest "T967b" def{hdlSim=False}
        , runTest "T967c" def{hdlSim=False}
        , NEEDS_PRIMS_GHC(clashLibTest ("tests" </> "shouldwork" </> "Naming") allTargets [] "T1041" "main")
        , clashLibTest ("tests" </> "shouldwork" </> "Naming") [VHDL,Verilog] [] "NameHint" "main"
        ]
      , clashTestGroup "Numbers"
        [ NEEDS_PRIMS_GHC(runTest "BitInteger" def)
#if MIN_VERSION_base(4,14,0)
        , runTest "BitReverse" def
#endif
        , NEEDS_PRIMS(runTest "Bounds" def)
        , NEEDS_PRIMS(runTest "DivideByZero" def)
        , let _opts = def { clashFlags=["-fconstraint-solver-iterations=15"] }
           in NEEDS_PRIMS_GHC(runTest "ExpWithGhcCF" _opts)
        , let _opts = def { clashFlags=["-fconstraint-solver-iterations=15"] }
           in NEEDS_PRIMS_GHC(runTest "ExpWithClashCF" _opts)
        , NEEDS_PRIMS(outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets [] ["-itests/shouldwork/Numbers"] "ExpWithClashCF" "main")
        , let _opts = def { hdlTargets = [VHDL], hdlSim = False }
           in NEEDS_PRIMS_GHC(runTest "HalfAsBlackboxArg" _opts)
        , NEEDS_PRIMS_GHC(runTest "IntegralTB" def)
        , NEEDS_PRIMS(runTest "NumConstantFoldingTB_1" def{clashFlags=["-itests/shouldwork/Numbers"]})
        , NEEDS_PRIMS(outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets ["-fconstraint-solver-iterations=15"] ["-itests/shouldwork/Numbers"] "NumConstantFolding_1" "main")
        , NEEDS_PRIMS(runTest "NumConstantFoldingTB_2" def{clashFlags=["-itests/shouldwork/Numbers"]})
        , NEEDS_PRIMS(outputTest ("tests" </> "shouldwork" </> "Numbers") allTargets ["-fconstraint-solver-iterations=15"] ["-itests/shouldwork/Numbers"] "NumConstantFolding_2" "main")
        , NEEDS_PRIMS_GHC(runTest "Naturals" def)
        , NEEDS_PRIMS_GHC(runTest "NaturalToInteger" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "NegativeLits" def)
        , NEEDS_PRIMS_GHC(runTest "Resize" def)
        , NEEDS_PRIMS_GHC(runTest "Resize2" def)
        , NEEDS_PRIMS_GHC(runTest "Resize3" def)
        , NEEDS_PRIMS_GHC(runTest "SatMult" def{hdlSim=False})
        , NEEDS_PRIMS_GHC(runTest "ShiftRotate" def)
        , runTest "ShiftRotateNegative" def{hdlTargets=[VHDL]}
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
        , let _opts = def { buildTargets=BuildSpecific ["example"]
                          , hdlSim=False
                          }
           in NEEDS_PRIMS_GHC(runTest "NoCPR" _opts)
        , NEEDS_PRIMS_GHC(runTest "Oversample" def)
        , NEEDS_PRIMS_GHC(runTest "RegisterAR" def)
        , NEEDS_PRIMS_GHC(runTest "RegisterSR" def)
        , NEEDS_PRIMS_GHC(runTest "RegisterAE" def)
        , NEEDS_PRIMS_GHC(runTest "RegisterSE" def)
        , NEEDS_PRIMS_GHC(runTest "ResetSynchronizer" def)
        , NEEDS_PRIMS_GHC(runTest "ResetSynchronizerSync" def)
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
                          , buildTargets=BuildSpecific ["topEntity"]
                          }
           in NEEDS_PRIMS_GHC(runTest "Test00" _opts)
        ]
      , clashTestGroup "SynthesisAttributes"
        [ NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") allTargets [] [] "Simple" "main")
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") allTargets [] [] "Product" "main")
        ,                 outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") allTargets [] [] "InstDeclAnnotations" "main"
        , NEEDS_PRIMS_GHC(runTest "Product" def)
        , outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") allTargets [] [] "T1771" "main"
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
        , runTest "TopEntHOArg" def{buildTargets=BuildSpecific ["f"], hdlSim=False}
        , runTest "T701" def {hdlSim=False}
        , runTest "T1033" def {hdlSim=False,buildTargets=BuildSpecific ["top"]}
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] [] "T1033" "main"
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] [] "T1072" "main"
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] [] "T1074" "main"
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [SystemVerilog] ["-main-is", "topEntity1"] [] "Multiple" "main1"
        , outputTest ("tests" </> "shouldwork" </> "TopEntity") [VHDL] ["-main-is", "topEntity3"] [] "Multiple" "main3"
        , runTest "T1139" def{hdlSim=False}
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortNames_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortNames" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNames" "main")
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortProducts_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortProducts" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortProducts" "main")
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortProductsSum_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortProductsSum" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortProductsSum" "main")
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortNamesWithUnit_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortNamesWithUnit" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithUnit" "main")
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortNamesWithVector_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortNamesWithVector" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithVector" "main")
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortNamesWithRTree_testBench"]
                          }
           in NEEDS_PRIMS_GHC(runTest "PortNamesWithRTree" _opts)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "TopEntity") [Verilog] [] [] "PortNamesWithRTree" "main")
        , NEEDS_PRIMS_GHC(clashLibTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] "T1182A" "main")
        , NEEDS_PRIMS_GHC(clashLibTest ("tests" </> "shouldwork" </> "TopEntity") allTargets [] "T1182B" "main")
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
        , NEEDS_PRIMS(runTest "IndexInt2" def)
        , NEEDS_PRIMS(outputTest ("tests" </> "shouldwork" </> "Vector") [Verilog] [] [] "IndexInt2" "main")
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
        , NEEDS_PRIMS_GHC(runTest "Iterate" def)
        , NEEDS_PRIMS_GHC(outputTest ("tests" </> "shouldwork" </> "Vector") [VHDL] [] [] "IterateCF" "main")
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
      , clashTestGroup "Verification" [
          runTest "SymbiYosys" def{
            hdlTargets=[Verilog, SystemVerilog]
          , buildTargets=BuildSpecific ["topEntity"]
          , hdlLoad=False
          , verificationTool=Just SymbiYosys
          }
        ]
      , clashTestGroup "XOptimization"
        [ NEEDS_PRIMS(outputTest  ("tests" </> "shouldwork" </> "XOptimization") allTargets [] [] "Conjunction" "main")
        , NEEDS_PRIMS(outputTest  ("tests" </> "shouldwork" </> "XOptimization") allTargets [] [] "Disjunction" "main")
        , clashLibTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "OneDefinedDataPat" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "OneDefinedLitPat" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "OneDefinedDefaultPat" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "XOptimization") allTargets [] "ManyDefined" "main"
        ]
#if EXPERIMENTAL_EVALUATOR
      , clashTestGroup "PartialEvaluation"
        [ clashLibTest ("tests" </> "shouldwork" </> "PartialEvaluation") allTargets [] "EtaExpansion" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "PartialEvaluation") allTargets [] "KnownCase" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "PartialEvaluation") allTargets [] "CaseOfCase" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "PartialEvaluation") allTargets [] "LazyEvaluation" "main"
        , clashLibTest ("tests" </> "shouldwork" </> "PartialEvaluation") allTargets [] "MutualRecursion" "main"
        ]
#endif
      ] -- end shouldwork
    ] -- end tests
  ] -- end .

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" (show numCapabilities)
  createDirectoryIfMissing True temporaryDirectory
  setClashEnvs compiledWith
  finally runClashTest (removeDirectoryRecursive temporaryDirectory)
