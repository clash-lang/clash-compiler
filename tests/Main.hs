{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Clash.Util.Interpolate    as I

import           Clash.Annotations.Primitive (HDL(..))
import qualified Data.Text                 as Text
import           Data.Default              (def)
import           Data.List                 ((\\), intercalate)
import           Data.Version              (versionBranch)
import           System.Directory
  (getCurrentDirectory, doesDirectoryExist, makeAbsolute)
import           System.Environment
import           System.Info
import           GHC.Conc                  (numCapabilities)
import           GHC.Stack
import           GHC.IO.Unsafe             (unsafePerformIO)
import           Text.Printf               (printf)

import           Test.Tasty
import           Test.Tasty.Common
import           Test.Tasty.Clash

-- | GHC version as major.minor.patch1. For example: 8.10.2.
ghcVersion3 :: String
ghcVersion3 =
#ifdef __GLASGOW_HASKELL_PATCHLEVEL2__
  let ghc_p1 = __GLASGOW_HASKELL_PATCHLEVEL1__
      ghc_p2 = __GLASGOW_HASKELL_PATCHLEVEL2__ in
  case ghc_p2 of
    0 ->
      intercalate "." (map show (versionBranch compilerVersion <> [ghc_p1]))
    _ ->
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
  platform :: String -- XXX: Hardcoded
  platform = case os of
     "mingw32" -> arch <> "-windows"
     _ -> arch <> "-" <> os
  templ = "dist-newstyle/build/%s/ghc-%s/clash-ghc-%s/x/clash/build/clash/" :: String

-- | Set GHC_PACKAGE_PATH for local Cabal install. Currently hardcoded for Unix;
-- override by setting @store_dir@ to point to local cabal installation.
setCabalPackagePaths :: IO ()
setCabalPackagePaths = do
  ch <- lookupEnv "store_dir"
  storeDir <- case ch of
    Just dir -> pure dir
    Nothing -> case os of
      "mingw32" -> pure "C:/cabal/store" -- default ghcup location
      _ ->  (<> "/.cabal/store") <$> getEnv "HOME"
  here <- getCurrentDirectory
  setEnv "GHC_PACKAGE_PATH" $
       storeDir <> "/ghc-" <> ghcVersion3 <> "/package.db"
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
  let seperator = case os of { "mingw32" -> ";"; _ -> ":" }
  setEnv "PATH" (binDir <> seperator <> path)
  setCabalPackagePaths

clashTestRoot
  :: [[TestName] -> TestTree]
  -> TestTree
clashTestRoot testTrees =
  clashTestGroup "." testTrees []

-- | `clashTestGroup` and `clashTestRoot` make sure that each test knows its
-- fully qualified test name at construction time. This is used to pass -i flags
-- to Clash as the test layout matches the layout in @shouldwork/@.
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
  [ clashTestGroup "examples"
    [ runTest "ALU" def{hdlSim=[]}
    , let _opts = def { hdlSim=[]
                      , hdlTargets=[VHDL]
                      , buildTargets=BuildSpecific ["blinker"]
                      }
       in runTest "Blinker" _opts
    , runTest "BlockRamTest" def{hdlSim=[]}
    , runTest "Calculator" def
    , runTest "CHIP8" def{hdlSim=[]}
    , runTest "CochleaPlus" def{hdlSim=[]}
    ,
      -- Vivado segfaults
      let _opts = def { clashFlags=["-fclash-component-prefix", "test"]
                      , buildTargets=BuildSpecific ["test_testBench"]
                      , hdlSim=hdlSim def \\ [Vivado]
                      }
       in runTest "FIR" _opts

    , runTest "Fifo" def{hdlSim=[]}
    , runTest "MAC" def
    , runTest "MatrixVect" def
    , runTest "Queens" def{hdlSim=[]}
    , runTest "Reducer" def{hdlSim=[]}
    , runTest "Sprockell" def{hdlSim=[]}
    , runTest "Windows" def{hdlSim=[]}
    , clashTestGroup "crc32"
        [ runTest "CRC32" def
        ]
    , clashTestGroup "i2c"
        [ let _opts = def { clashFlags=["-O2","-fclash-component-prefix","test"]
                        , buildTargets=BuildSpecific ["test_i2c"]
                        , hdlSim=[]
                        }
           in runTest "I2C" _opts
        ,
          -- TODO: this uses finish_and_return, with is Icarus Verilog only.
          -- see: https://github.com/clash-lang/clash-compiler/issues/2265
          let _opts = def { buildTargets = BuildSpecific ["system"]
                          , hdlTargets = [Verilog]
                          , hdlLoad = [IVerilog]
                          , hdlSim = [IVerilog]
                          , vvpStdoutNonEmptyFail = False
                          }
           in runTest "I2Ctest" _opts

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
        , runTest "T1945" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (def, "Template function for returned False")
          }
        ]
      , clashTestGroup "Cores"
        [ clashTestGroup "Xilinx"
          [ clashTestGroup "VIO"
            [ runTest "OutputBusWidthExceeded" def{
                hdlTargets=[VHDL, Verilog, SystemVerilog]
              , expectClashFail=Just (def, "Probe signals must be been between 1 and 256 bits wide.")
              }
            , runTest "OutputProbesExceeded" def{
                hdlTargets=[VHDL, Verilog, SystemVerilog]
              , expectClashFail=Just (def, "At most 256 input/output probes are supported.")
              }
            , runTest "InputBusWidthExceeded" def{
                hdlTargets=[VHDL, Verilog, SystemVerilog]
              , expectClashFail=Just (def, "Probe signals must be been between 1 and 256 bits wide.")
              }
            , runTest "InputProbesExceeded" def{
                hdlTargets=[VHDL, Verilog, SystemVerilog]
              , expectClashFail=Just (def, "At most 256 input/output probes are supported.")
              }
            ]
          ]
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
      , clashTestGroup "Testbench"
        [ runTest "UnsafeOutputVerifier" def{
            expectClashFail=Just ( TestSpecificExitCode 0
                                 , "Clash.Explicit.Testbench.unsafeSimSynchronizer is not safely synthesizable!")
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
                          , hdlLoad=[GHDL]
                          , hdlSim=[GHDL]
                          , expectSimFail=Just (def, "psl assertion failed")
                          }
           in runTest "NonTemporalPSL" _opts
        , let n = 13
              _opts = def { hdlTargets=[SystemVerilog]
                          , buildTargets=BuildSpecific ["fails" <> show i | i <- [(1::Int)..n]]
                          -- Only QuestaSim supports simulating SVA/PSL, but ModelSim does check
                          -- for syntax errors.
                          , hdlLoad=[ModelSim]
                          , hdlSim=[]
                          }
           in runTest "NonTemporalPSL" _opts
        , let is = [(1::Int)..13] \\ [4, 6, 7, 8, 10, 11, 12] in
          runTest "NonTemporalSVA" def{
            hdlTargets=[SystemVerilog]
          , buildTargets=BuildSpecific ["fails" <> show i | i <- is]
          -- Only QuestaSim supports simulating SVA/PSL, but ModelSim does check
          -- for syntax errors.
          , hdlLoad=[ModelSim]
          , hdlSim=[]
          }
        , runTest "SymbiYosys" def{
            hdlTargets=[Verilog, SystemVerilog]
          , hdlLoad=[]
          , hdlSim=[]
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
        , expectClashFail=Just (def, " already inlined 20 times in: ")  -- (RecursiveBoxed\.)?topEntity
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
        [ outputTest "AutoReg" def
        , runTest "T1507" def{hdlSim=[]}
        , let _opts = def{hdlSim=[], hdlTargets=[VHDL]}
           in runTest "T1632" _opts
        ]
      , clashTestGroup "Basic"
        [ runTest "AES" def{hdlSim=[]}
        , runTest "BangData" def{hdlSim=[]}
        , runTest "CaseOfErr" def{hdlTargets=[VHDL],hdlSim=[]}
        , runTest "Trace" def{hdlSim=[]}
        , runTest "DivMod" def{hdlSim=[]}
        , runTest "DivZero" def
        , runTest "LambdaDrop" def{hdlSim=[]}
        , runTest "IrrefError" def{hdlSim=[]}
#ifdef CLASH_MULTIPLE_HIDDEN
        , runTest "MultipleHidden" def
#endif
        , outputTest "NameInlining" def
        , runTest "NameInstance" def{hdlSim=[]}
        , outputTest "NameInstance" def
        , outputTest "SetName" def{hdlTargets=[VHDL]}
        , outputTest "SimulationMagic" def{hdlTargets=[VHDL]}
        , runTest "PatError" def{hdlSim=[]}
        , runTest "ByteSwap32" def
        , runTest "CharTest" def
        , runTest "ClassOps" def
        , runTest "CountTrailingZeros" def
        , runTest "DeepseqX" def
        , runTest "LotOfStates" def
        , let _opts = def { buildTargets = BuildSpecific ["nameoverlap"]
                          , hdlSim = []
                          }
           in runTest "NameOverlap" _opts
        , runTest "NestedPrimitives" def{hdlSim=[]}
        , runTest "NestedPrimitives2" def{hdlSim=[]}
        , runTest "NORX" def
        , runTest "Parameters" def{hdlTargets=[VHDL]}
        , runTest "PopCount" def
        , runTest "RecordSumOfProducts" def{hdlSim=[]}
        , runTest "Replace" def
        , runTest "TestIndex" def{hdlSim=[]}
        , runTest "Time" def
        , runTest "Shift" def{hdlSim=[]}
        , runTest "SimOnly" def{hdlTargets=[VHDL],hdlSim=[]}
        , runTest "SimpleConstructor" def{hdlSim=[]}
        , runTest "SomeNatVal" def{hdlTargets=[VHDL],hdlSim=[]}
        , runTest "TyEqConstraints" def{
            hdlSim=[]
          , buildTargets=BuildSpecific ["top1"]
          }
        , runTest "T1012" def{hdlSim=[]}
        , runTest "T1240" def{hdlSim=[]}
        , let _opts = def {hdlTargets = [VHDL], hdlSim = []}
           in runTest "T1297" _opts
        , runTest "T1254" def{hdlTargets=[VHDL,SystemVerilog],hdlSim=[]}
        , runTest "T1242" def{hdlSim=[]}
        , runTest "T1292" def{hdlTargets=[VHDL]}
        , let _opts = def { hdlTargets = [VHDL], hdlLoad = [], hdlSim=[] }
           in runTest "T1304" _opts
        , let _opts = def { hdlTargets=[VHDL]
                          , hdlSim=[]
                          , clashFlags=["-main-is", "plus"]
                          , buildTargets=BuildSpecific ["plus"]
                          }
           in runTest "T1305" _opts
        , let _opts = def {hdlTargets = [VHDL], hdlSim = []}
           in runTest "T1316" _opts
        , runTest "T1322" def{hdlTargets=[VHDL]}
        , let _opts = def {hdlTargets = [VHDL], hdlSim = []}
           in runTest "T1340" _opts
        , let _opts = def { hdlTargets = [VHDL], hdlSim = []}
           in runTest "T1354A" _opts
        , let _opts = def { hdlTargets = [VHDL], hdlSim = []}
           in runTest "T1354B" _opts
        , runTest "T1402" def{clashFlags=["-O"]}
        , runTest "T1402b" def{hdlTargets=[VHDL], hdlSim=[]}
        , runTest "T1556" def
        , runTest "T1591" def{hdlTargets=[VHDL], hdlSim=[]}
        , runTest "TagToEnum" def{hdlSim=[]}
        , runTest "TwoFunctions" def{hdlSim=[]}
        , runTest "XToError" def{hdlSim=[]}
        ]
      , clashTestGroup "BitVector"
        [ runTest "Box" def
        , runTest "BoxGrow" def
        , runTest "CLZ" def
        , runTest "RePack" def{hdlSim=[]}
        , runTest "ReduceZero" def
        , runTest "ReduceOne" def
        , runTest "ExtendingNumZero" def
        , runTest "AppendZero" def
        , runTest "GenericBitPack" def{clashFlags=["-fconstraint-solver-iterations=15"]}
        , runTest "UnpackUndefined" def{hdlSim=[]}
        ]
      , clashTestGroup "BlackBox"
        [ outputTest "TemplateFunction" def{hdlTargets=[VHDL]}
        , outputTest "BlackBoxFunction" def{hdlTargets=[VHDL]}
        , runTest "BlackBoxFunctionHO" def{hdlTargets=[VHDL]}
        , outputTest "ExternalPrimitive" def{hdlTargets=[VHDL]}
        , outputTest "ZeroWidth" def{hdlTargets=[VHDL]}
        , outputTest "MultiResult" def{hdlTargets=[VHDL]}
        , runTest "DSL" def
        , runTest "MultiResult" def
        , runTest "T919" def{hdlSim=[]}
        , runTest "T1524" def
        , runTest "T1786" def{
            hdlTargets=[VHDL]
          , buildTargets=BuildSpecific ["testEnableTB", "testBoolTB"]
          }
        , outputTest "LITrendering" def{hdlTargets=[Verilog]}
        , runTest "T2117" def{
            clashFlags=["-fclash-aggressive-x-optimization-blackboxes"]
          , hdlTargets=[VHDL]
          , buildTargets=BuildSpecific [ "testBenchUndefBV"
                                       , "testBenchUndefTup"
                                       , "testBenchPartialDefTup"]}
        ]
      , clashTestGroup "BoxedFunctions"
        [ runTest "DeadRecursiveBoxed" def{hdlSim=[]}
        ]
      , clashTestGroup "Cores"
        [ clashTestGroup "Xilinx"
          [ runTest "TdpBlockRam" def
            { -- Compiling with VHDL gives:
              --   https://github.com/clash-lang/clash-compiler/issues/2446
              hdlTargets = [Verilog]
            , hdlLoad = [Vivado]
            , hdlSim = [Vivado]
            , clashFlags=["-fclash-hdlsyn", "Vivado"]
            , buildTargets=BuildSpecific [ "normalWritesTB", "writeEnableWritesTB" ]
            }
          , let _opts = def{ hdlTargets=[VHDL, Verilog]
                           , hdlLoad=[Vivado]
                           , hdlSim=[Vivado]
                             -- addShortPLTB now segfaults :-(
                           , buildTargets=BuildSpecific [ "addBasicTB"
                                                        , "addEnableTB"
                                                        -- , "addShortPLTB"
                                                        , "subBasicTB"
                                                        , "mulBasicTB"
                                                        , "divBasicTB"
                                                        , "compareBasicTB"
                                                        , "compareEnableTB"
                                                        , "fromUBasicTB"
                                                        , "fromUEnableTB"
                                                        , "fromSBasicTB"
                                                        , "fromSEnableTB"
                                                        ]
                           }
            in runTest "Floating" _opts
          , runTest "XpmCdcArraySingle" $ def
              { hdlTargets=[VHDL, Verilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..7]]
              }
          , runTest "XpmCdcGray" $ def
              { hdlTargets=[VHDL, Verilog]
              , hdlLoad=[]
              , hdlSim=[Vivado]
              , buildTargets=BuildSpecific ["tb" <> show n | n <- [(1::Int)..7]]
              }
          , runTest "XpmCdcSingle" $ def
              { hdlTargets=[VHDL, Verilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..7]]
              }
          , clashTestGroup "DcFifo"
            [ let _opts =
                    def{ hdlTargets=[VHDL, Verilog]
                       , hdlLoad=[]
                       , hdlSim=[Vivado]
                       }
              in runTest "Basic" _opts
            , let _opts = def{ hdlTargets=[VHDL, Verilog]
                             , hdlLoad=[]
                             , hdlSim=[Vivado]
                             , buildTargets=BuildSpecific [ "testBench_17_2"
                                                          , "testBench_2_17"
                                                          , "testBench_2_2"
                                                          ]
                             }
              in runTest "Lfsr" _opts
            ]
          , let _opts =
                  def{ hdlTargets=[VHDL, Verilog, SystemVerilog]
                     , hdlLoad=[Vivado]
                     , hdlSim=[Vivado]
                     , buildTargets=BuildSpecific [ "noInputTrue"
                                                  , "noInputFalse"
                                                  , "noInputLow"
                                                  , "noInputHigh"
                                                  , "noInputSigned"
                                                  , "noInputUnsigned"
                                                  , "noInputBitVector"
                                                  , "noInputPair"
                                                  , "noInputVec"
                                                  , "noInputCustom"
                                                  , "noInputNested"
                                                  , "singleInputBool"
                                                  , "singleInputBit"
                                                  , "singleInputSigned"
                                                  , "singleInputUnsigned"
                                                  , "singleInputBitVector"
                                                  , "singleInputPair"
                                                  , "singleInputVec"
                                                  , "singleInputCustom"
                                                  , "singleInputNested"
                                                  , "multipleInputs"
                                                  , "inputsAndOutputs"
                                                  ]
                     }
            in runTest "VIO" _opts
          ]
        ]
      , clashTestGroup "CSignal"
        [ runTest "MAC" def{hdlSim=[]}
        , runTest "CBlockRamTest" def{hdlSim=[]}
        ]
#ifdef COSIM
      , clashTestGroup "CoSim"
        [ runTest "Multiply" def{hdlTargets=[Verilog]}
        , runTest "Register" def{hdlTargets=[Verilog]}
        ]
#endif
      , clashTestGroup "CustomReprs"
        [ clashTestGroup "RotateC"
          [ runTest "RotateC" def
          , runTest "ReprCompact" def
          , runTest "ReprCompactScrambled"   def
          , runTest "ReprLastBitConstructor" def
          , let _opts = def { hdlTargets = [VHDL, Verilog] }
             in runTest "ReprStrangeMasks" _opts
          , runTest "ReprWide" def
          , runTest "RotateCScrambled" def
          ]
        , clashTestGroup "RotateCNested"
          [ runTest "RotateCNested" def
          ]
        , clashTestGroup "Rotate"
          [ runTest "Rotate" def
          ]
        , clashTestGroup "Deriving"
          [ runTest "BitPackDerivation" def
          ]
        , clashTestGroup "Indexed"
          [ runTest "Indexed" def
          ]
        ]
      , clashTestGroup "CustomReprs"
        [ clashTestGroup "ZeroWidth"
          [ runTest "ZeroWidth" def{hdlSim=[]}
          ]
        , runTest "T694" def{hdlSim=[],hdlTargets=[VHDL]}
        ]
      , clashTestGroup "DDR"
        [ let _opts = def{ buildTargets = BuildSpecific [ "testBenchGA"
                                                        , "testBenchGS"
                                                        , "testBenchUA"
                                                        , "testBenchUS"
                                                        ]}
          in runTest "DDRin" _opts
        , let _opts = def{ buildTargets = BuildSpecific [ "testBenchUA"
                                                        , "testBenchUS"
                                                        , "testBenchGA"
                                                        , "testBenchGS"
                                                        ]}
          in runTest "DDRout" _opts
        ]
      , clashTestGroup "DSignal"
        [ runTest "DelayedFold" def
        , runTest "DelayI" def
        , runTest "DelayN" def
        ]
      , clashTestGroup "Feedback"
        [ runTest "Fib" def
#ifdef CLASH_MULTIPLE_HIDDEN
        , runTest "MutuallyRecursive" def
#endif
        ]
      , clashTestGroup "Fixed"
        [ runTest "Mixer" def
        , runTest "SFixedTest" def
        , runTest "SatWrap" def{hdlSim=[]}
        , runTest "ZeroInt" def
        ]
      , clashTestGroup "Floating"
        [ runTest "FloatPack" def{hdlSim=[], clashFlags=["-fclash-float-support"]}
        , runTest "FloatConstFolding" def{clashFlags=["-fclash-float-support"]}
        , runTest "T1803" def{clashFlags=["-fclash-float-support"]}
        ]
      , clashTestGroup "GADTs"
        [ runTest "Constrained" def
        , runTest "Head" def
        , runTest "HeadM" def
        , runTest "MonomorphicTopEntity" def
        , runTest "Record" def
        , runTest "Tail" def
        , runTest "TailM" def
        , runTest "TailOfTail" def
        , runTest "T1310" def{hdlSim=[]}
        , runTest "T1536" def{hdlSim=[]}
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
      , clashTestGroup "Issues" $
        [ clashLibTest "T508" def
        , let _opts = def { hdlSim = [], hdlTargets = [Verilog] }
           in runTest "T1187" _opts
        , clashLibTest "T1388" def{hdlTargets=[VHDL]}
        , outputTest "T1171" def
        , clashLibTest "T1439" def{hdlTargets=[VHDL]}
        , runTest "T1477" def{hdlSim=[]}
        , runTest "T1506A" def{hdlSim=[], clashFlags=["-fclash-aggressive-x-optimization-blackboxes"]}
        , outputTest "T1506B" def
            { clashFlags=["-fclash-aggressive-x-optimization-blackboxes"]
            , ghcFlags=["-itests/shouldwork/Issues"]
            }
        , runTest "T1615" def{hdlSim=[], hdlTargets=[Verilog]}
        , runTest "T1663" def{hdlTargets=[VHDL], hdlSim=[]}
        , runTest "T1669_DEC" def{hdlTargets=[VHDL]}
        , runTest "T1715" def
        , runTest "T1721" def{hdlSim=[]}
        , runTest "T1606A" def{hdlSim=[]}
        , runTest "T1606B" def{hdlSim=[]}
        , runTest "T1742" def{hdlSim=[], buildTargets=BuildSpecific ["shell"]}
        , runTest "T1756" def{hdlSim=[]}
        , outputTest "T431" def{hdlTargets=[VHDL]}
        , clashLibTest "T779" def{hdlTargets=[Verilog]}
        , outputTest "T1881" def{hdlSim=[]}
        , runTest "T1921" def{hdlTargets=[Verilog], hdlSim=[]}
        , runTest "T1933" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (NoTestExitCode, "NOT:WARNING")
          }
        , outputTest "T1996" def{hdlTargets=[VHDL]}
        , runTest "T2040" def{hdlTargets=[VHDL],clashFlags=["-fclash-compile-ultra"]}
        -- TODO I wanted to call this T2046A since there are multiple tests
        -- for T2046. However, doing so completely breaks HDL loading because
        -- it completely ignores the BuildSpecific...
        , runTest "T2046" def
            { hdlSim=[]
            , clashFlags=["-Werror"]
            , buildTargets=BuildSpecific["top_bit", "top_bitvector", "top_index", "top_signed", "top_unsigned"]
            }
        , runTest "T2046B" def{clashFlags=["-Werror"]}
        , runTest "T2046C" def{hdlSim=[],clashFlags=["-Werror"]}
        , runTest "T2097" def{hdlSim=[]}
        , runTest "T2154" def{hdlTargets=[VHDL], hdlSim=[]}
        , runTest "T2220_toEnumOOB" def{hdlTargets=[VHDL]}
        , runTest "T2272" def{hdlTargets=[VHDL], hdlSim=[]}
        , outputTest "T2334" def{hdlTargets=[VHDL]}
        , outputTest "T2325" def{hdlTargets=[VHDL]}
        , outputTest "T2325f" def{hdlTargets=[VHDL]}
        , runTest "T2342A" def{hdlSim=[]}
        , runTest "T2342B" def{hdlSim=[]}
        , runTest "T2360" def{hdlSim=[],clashFlags=["-fclash-force-undefined=0"]}
        , outputTest "T2242" def{hdlTargets=[Verilog], clashFlags=["-fclash-sanitize-netlist-ids"]}
        ] <>
        if compiledWith == Cabal then
          -- This tests fails without environment files present, which are only
          -- generated by Cabal. It complains it is trying to import "BasicTypes"
          -- which is a member of the hidden package 'ghc'. Passing
          -- '-package ghc' doesn't seem to help though. TODO: Investigate.
          [clashLibTest "T1568" def]
        else
          []
      , clashTestGroup "LoadModules"
        [ runTest "T1796" def{hdlSim=[]}
        ]
      , clashTestGroup "Naming"
        [ runTest "T967a" def{hdlSim=[]}
        , runTest "T967b" def{hdlSim=[]}
        , runTest "T967c" def{hdlSim=[]}
        , clashLibTest "T1041" def
        , clashLibTest "NameHint" def{hdlTargets=[VHDL,Verilog]}
        ]
      , clashTestGroup "Netlist"
          [ clashLibTest "Identity" def
          , clashLibTest "NoDeDup" def{hdlTargets=[VHDL]}
          , clashLibTest "T1766" def
          , clashLibTest "T1935" def
          ]
      , clashTestGroup "Numbers"
        [ runTest "BitInteger" def
#if MIN_VERSION_base(4,14,0)
        , runTest "BitReverse" def
#endif
        ,
          -- vivado segfaults
          runTest "Bounds" def { hdlSim=hdlSim def \\ [Vivado] }

        , runTest "DivideByZero" def
        , let _opts = def { clashFlags=["-fconstraint-solver-iterations=15"] }
           in runTest "ExpWithGhcCF" _opts
        , let _opts = def { clashFlags=["-fconstraint-solver-iterations=15"] }
           in runTest "ExpWithClashCF" _opts
        , outputTest "ExpWithClashCF" def{ghcFlags=["-itests/shouldwork/Numbers"]}
        , let _opts = def { hdlTargets = [VHDL], hdlSim = [] }
           in runTest "HalfAsBlackboxArg" _opts
        ,
          -- see https://github.com/clash-lang/clash-compiler/issues/2262,
          -- Vivado's mod misbehaves on negative dividend
          runTest "IntegralTB" def{hdlSim=hdlSim def \\ [Vivado]}

        , runTest "NumConstantFoldingTB_1" def{clashFlags=["-itests/shouldwork/Numbers"]}
        , outputTest "NumConstantFolding_1" def
            { clashFlags=["-fconstraint-solver-iterations=15"]
            , ghcFlags=["-itests/shouldwork/Numbers"]
            }
        , let _opts = def { clashFlags=["-itests/shouldwork/Numbers"]
                          , hdlLoad = hdlLoad def \\ [Verilator]
                          , hdlSim = hdlSim def \\ [Verilator]
                          }
          in runTest "NumConstantFoldingTB_2" _opts
        , outputTest "NumConstantFolding_2" def
            { clashFlags=["-fconstraint-solver-iterations=15"]
            , ghcFlags=["-itests/shouldwork/Numbers"]
            }
        , runTest "Naturals" def
        , runTest "NaturalToInteger" def{hdlSim=[]}
        , runTest "NegativeLits" def
        , runTest "Resize" def
        , runTest "Resize2" def
        , runTest "Resize3" def
        , runTest "SatMult" def{hdlSim=[]}
        , runTest "ShiftRotate" def
        , runTest "ShiftRotateNegative" def{hdlTargets=[VHDL]}
        , runTest "SignedProjectionTB" def
        , runTest "SignedZero" def
        , runTest "Signum" def
        ,
          -- vivado segfaults
          runTest "Strict" def{hdlSim=hdlSim def \\ [Vivado]}

        , runTest "T1019" def{hdlSim=[]}
        , runTest "T1351" def
        , runTest "T2149" def
        , outputTest "UndefinedConstantFolding" def{ghcFlags=["-itests/shouldwork/Numbers"]}
        , runTest "UnsignedZero" def
        ]
      , clashTestGroup "Polymorphism"
        [ runTest "ExistentialBoxed" def{hdlSim=[]}
        , runTest "FunctionInstances" def
        , runTest "GADTExistential" def{hdlSim=[]}
        , runTest "LocalPoly" def{hdlSim=[]}
        ]
      , clashTestGroup "PrimitiveGuards"
        [ runTest "WarnAlways" def{
            hdlTargets=[VHDL]
          , expectClashFail=Just (NoTestExitCode, "You shouldn't use 'primitive'!")
          }
        ]
      , clashTestGroup "PrimitiveReductions"
        [ runTest "Lambda" def
        , runTest "ReplaceInt" def
        ]
      , clashTestGroup "RTree"
        [ runTest "TZip" def{hdlSim=[]}
        , runTest "TFold" def{hdlSim=[]}
        , runTest "TRepeat" def
        , runTest "TRepeat2" def
        ]
      , clashTestGroup "Shadowing"
        [ runTest "T990" def
        ]
      , clashTestGroup "Signal"
        [ runTest "AlwaysHigh" def{hdlSim=[]}
        , runTest "BangPatterns" def
        ,
          -- TODO: we do not support memory files in Vivado
          --
          -- see: https://github.com/clash-lang/clash-compiler/issues/2269
          runTest "BlockRamFile" def{hdlSim=hdlSim def \\ [Vivado]}

        , runTest "BlockRam0" def
        , runTest "BlockRam1" def
        , clashTestGroup "BlockRam"
          [ runTest "Blob" def
          ]
        , runTest "AndEnable" def
#ifdef CLASH_MULTIPLE_HIDDEN
        ,
          -- TODO: Vivado is disabled because it gives different results, see
          -- https://github.com/clash-lang/clash-compiler/issues/2267
          runTest "AndSpecificEnable" def{hdlSim=hdlSim def \\ [Vivado]}

#endif
        , runTest "Ram" def
        , clashTestGroup "Ram"
          [ runTest "RMultiTop" def
          , let _opts = def{ buildTargets=BuildSpecific [ "testBench35"
                                                        , "testBench53"]}
            in runTest "RWMultiTop" _opts
          ]
        , runTest "ResetGen" def
        ,
          -- TODO: we do not support memory files in Vivado
          --
          -- see: https://github.com/clash-lang/clash-compiler/issues/2269
          runTest "RomFile" def{hdlSim=hdlSim def \\ [Vivado]}

        , outputTest "BlockRamLazy" def
        , runTest "BlockRamTest" def{hdlSim=[]}
        , runTest "Compression" def
        , runTest "DelayedReset" def
        , let _opts = def{ -- Vivado segfaults
                           hdlLoad=hdlLoad def \\ [Verilator, Vivado]
                         , hdlSim=hdlSim def \\ [Verilator, Vivado]
                         , buildTargets=BuildSpecific [ "testBenchAB"
                                                      , "testBenchBC"]
                         }
          in runTest "DualBlockRam" _opts
        , let _opts = def { buildTargets=BuildSpecific ["example"]
                          , hdlSim=[]
                          }
           in runTest "NoCPR" _opts
        , runTest "DynamicClocks" def
            { hdlLoad = hdlLoad def \\ [Verilator]
            , hdlSim = hdlSim def \\ [Verilator]
            , clashFlags = ["-fclash-timescale-precision", "1fs"]
            }
        , runTest "Oversample" def
        , runTest "RegisterAR" def
        , runTest "RegisterSR" def
        , runTest "RegisterAE" def
        , runTest "RegisterSE" def
        , let _opts = def{ buildTargets=BuildSpecific [ "testBenchAsync"
                                                      , "testBenchSync"]}
          in runTest "ResetSynchronizer" _opts
        , runTest "ResetLow" def
        , runTest "Rom" def
        , runTest "RomNegative" def
        , clashTestGroup "ROM"
          [ runTest "Async" def
          , runTest "AsyncBlob" def
          , runTest "Blob" def
          ,
            -- TODO: When issue #2039 is fixed, it should be possible to drop
            -- compile-ultra.
            -- TODO: Vivado is disabled because it gives different results, see
            -- https://github.com/clash-lang/clash-compiler/issues/2268
            let _opts = def { clashFlags=["-fclash-compile-ultra"]
                            , hdlSim=hdlSim def \\ [Vivado]
                            }
            in runTest "BlobVec" _opts
          ]
        , runTest "SigP" def{hdlSim=[]}
        , outputTest "T1102A" def{hdlTargets=[VHDL]}
        , outputTest "T1102B" def{hdlTargets=[VHDL]}
        , runTest "T2069" def
        , clashTestGroup "BiSignal"
          [ runTest "Counter" def
          , runTest "CounterHalfTuple" def
          , runTest "CounterHalfTupleRev" def
          , outputTest "T2472" def{hdlTargets=[VHDL]}
          ]
        , runTest "T1007" def{hdlSim=[]}
        ]
      , clashTestGroup "SimIO"
        [ let _opts = def { hdlTargets=[Verilog]
                          , vvpStdoutNonEmptyFail=False
                          , buildTargets=BuildSpecific ["topEntity"]
                          , hdlLoad = [IVerilog]
                          , hdlSim = [IVerilog]
                          }
           in runTest "Test00" _opts
        ]
      , clashTestGroup "SynthesisAttributes"
        [ outputTest "Simple" def
        , outputTest "Product" def
        , outputTest "InstDeclAnnotations" def
        , runTest "Product" def
        , outputTest "T1771" def
        ]
      , clashTestGroup "Testbench"
        [ runTest "TB" def{clashFlags=["-fclash-inline-limit=0"]}
        , runTest "SyncTB" def
        ]
      , clashTestGroup "Types"
        [ runTest "TypeFamilyReduction" def{hdlSim=[]}
        , runTest "NatExp" def{hdlSim=[]}
        ]
      , clashTestGroup "TopEntity"
        -- VHDL tests disabled for now: I can't figure out how to generate a static name whilst retaining the ability to actually test..
        [ outputTest "PortGeneration" def
        , outputTest "PortNamesWithSingletonVector" def{hdlTargets=[Verilog]}
        , runTest "TopEntHOArg" def{buildTargets=BuildSpecific ["f"], hdlSim=[]}
        , runTest "T701" def {hdlSim=[]}
        , runTest "T1033" def {hdlSim=[],buildTargets=BuildSpecific ["top"]}
        , outputTest "T1033" def
        , outputTest "T1072" def
        , outputTest "T1074" def
        , outputTest "Multiple" def
            { hdlTargets = [SystemVerilog]
            , clashFlags = ["-main-is", "topEntity1"]
            }
        , outputTest "Multiple" def
            { hdlTargets = [VHDL]
            , clashFlags = ["-main-is", "topEntity3"]
            }
        , runTest "T1139" def{hdlSim=[]}
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortNames_testBench"]
                          }
           in runTest "PortNames" _opts
        , outputTest "PortNames" def{hdlTargets=[Verilog]}
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortProducts_testBench"]
                          }
           in runTest "PortProducts" _opts
        , outputTest "PortProducts" def{hdlTargets=[Verilog]}
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortProductsSum_testBench"]
                          }
           in runTest "PortProductsSum" _opts
        , outputTest "PortProductsSum" def{hdlTargets=[Verilog]}
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortNamesWithUnit_testBench"]
                          }
           in runTest "PortNamesWithUnit" _opts
        , outputTest "PortNamesWithUnit" def{hdlTargets=[Verilog]}
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortNamesWithVector_testBench"]
                          }
           in runTest "PortNamesWithVector" _opts
        , outputTest "PortNamesWithVector" def{hdlTargets=[Verilog]}
        , let _opts = def { hdlTargets=[Verilog]
                          , buildTargets=BuildSpecific ["PortNamesWithRTree_testBench"]
                          }
           in runTest "PortNamesWithRTree" _opts
        , outputTest "PortNamesWithRTree" def{hdlTargets=[Verilog]}
        , clashLibTest "T1182A" def
        , clashLibTest "T1182B" def
        ]
      , clashTestGroup "Unit"
        [ runTest "Imap" def
        , runTest "ZipWithUnitVector" def
        , runTest "ZipWithTupleWithUnitLeft" def
        , runTest "ZipWithTupleWithUnitRight" def
        , runTest "ZipWithTripleWithUnitMiddle" def
        , runTest "ZipWithUnitSP" def
        , runTest "ZipWithUnitSP2" def
        ]
      , clashTestGroup "Vector"
        [ runTest "EnumTypes" def{hdlSim=[]}
        , runTest "HOCon" def{hdlSim=[]}
        , runTest "VMapAccum" def{hdlSim=[]}
        , runTest "VScan" def{hdlSim=[]}
        , runTest "VZip" def{hdlSim=[]}
        , runTest "VecConst" def{hdlSim=[]}
        ,
          -- vivado segfaults
          runTest "FirOddSize" def{hdlSim=hdlSim def \\ [Vivado]}

        , runTest "IndexInt" def
        ,
          -- Vivado segfaults
          runTest "IndexInt2" def {hdlSim=hdlSim def \\ [Vivado]}

        , outputTest "IndexInt2" def{hdlTargets=[Verilog]}
        , runTest "Concat" def
        , let _opts = def { hdlLoad = hdlLoad def \\ [Verilator]
                          , hdlSim = hdlSim def \\ [Verilator]
                          }
          in runTest "DFold" _opts
        , runTest "DFold2" def
        , runTest "DTFold" def
        ,
          -- vivado segfaults
          runTest "FindIndex" def{hdlSim=hdlSim def \\ [Vivado]}

        , runTest "Fold" def
        , runTest "FoldlFuns" def{hdlSim=[]}
        , runTest "Foldr" def
        , runTest "FoldrEmpty" def
        , runTest "HOClock" def{hdlSim=[]}
        , runTest "HOPrim" def{hdlSim=[]}
        , runTest "Indices" def
        , runTest "Iterate" def
        , outputTest "IterateCF" def{hdlTargets=[VHDL]}
        , runTest "Minimum" def
        , runTest "MovingAvg" def{hdlSim=[]}
        , runTest "PatHOCon" def{hdlSim=[]}
        , runTest "Scatter" def
        , runTest "Split" def{hdlSim=[]}
        , runTest "ToList" def
        , runTest "Unconcat" def
        , runTest "VACC" def{hdlSim=[]}
        , runTest "VEmpty" def
        , runTest "VIndex" def{hdlSim=[]}
        , runTest "VIndicesI" def
        , runTest "VFold" def{hdlSim=hdlSim def \\ [Vivado]} -- vivado segfaults
        , runTest "VMerge" def
        , runTest "VReplace" def
        , runTest "VReverse" def
        , runTest "VRotate" def
        , runTest "VSelect" def
        , runTest "VecOfSum" def{hdlSim=[]}
        , runTest "T452" def{hdlSim=[]}
        , runTest "T478" def{hdlSim=[]}
        , let _opts = def {hdlSim = [], hdlTargets = [VHDL]}
           in runTest "T895" _opts
        , let _opts = def {hdlSim = [], hdlTargets = [VHDL]}
           in runTest "T1360" _opts
        ] -- end vector
      , clashTestGroup "Verification" [
          runTest "SymbiYosys" def{
            hdlTargets=[Verilog, SystemVerilog]
          , buildTargets=BuildSpecific ["topEntity"]
          , hdlLoad=[]
          , hdlSim=[]
          , verificationTool=Just SymbiYosys
          }
        ]
      , clashTestGroup "Xilinx"
        [ let _opts = def{ hdlTargets=[VHDL, Verilog]
                         , hdlLoad=[Vivado]
                         , hdlSim=[Vivado]
                         }
          in runTest "ClockWizard" _opts
        ]
      , clashTestGroup "XOptimization"
        [ outputTest "Conjunction" def
        , outputTest "Disjunction" def
        , clashLibTest "OneDefinedDataPat" def
        , clashLibTest "OneDefinedLitPat" def
        , clashLibTest "OneDefinedDefaultPat" def
        , clashLibTest "ManyDefined" def
        ]
--    , clashTestGroup "PartialEvaluation"
--      [ clashLibTest "EtaExpansion" def
--      , clashLibTest "KnownCase" def
--      , clashLibTest "CaseOfCase" def
--      , clashLibTest "LazyEvaluation" def
--      , clashLibTest "MutualRecursion" def
--      ]
      ] -- end shouldwork
    ] -- end tests
  ] -- end .

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" (show numCapabilities)
  setClashEnvs compiledWith
  runClashTest
