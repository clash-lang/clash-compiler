{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Program ( testProgram
                          , testFailingProgram
                          , PrintOutput ( PrintStdErr
                                        , PrintNeither
                                        , PrintStdOut )
                          )

import           Data.Char        (toLower)
import qualified Data.List        as List
import qualified Data.Text        as Text
import qualified System.Directory as Directory
import           System.Process   (callProcess)
import           System.FilePath  ((</>),(<.>))
import qualified System.IO.Unsafe as Unsafe
import           System.IO.Temp   (createTempDirectory)
import Control.Concurrent.Lock    ( Lock, newAcquired )


data BuildTarget
  = VHDL | Verilog | SystemVerilog | Both | All
  deriving (Show,Eq)


defBuild :: BuildTarget
#ifdef TRAVISBUILD
defBuild = Both
#else
defBuild = All
#endif

temporaryDirectory :: String
temporaryDirectory = ".clash-test-tmp"

main :: IO ()
main = do
  callProcess "cabal" ["new-build", "all"]

  defaultMain $ testGroup "tests"
    [ testGroup "examples"
      [runTest "examples"             defBuild [] "ALU"          ([""],"ALU_topEntity",False)
      ,runTest "examples"             VHDL     [] "Blinker"      (["blinker"],"blinker",False)
      ,runTest "examples"             defBuild [] "BlockRamTest" ([""],"BlockRamTest_topEntity",False)
      ,runTest "examples"             defBuild [] "Calculator"   (["","Calculator_testBench"],"Calculator_testBench",True )
      ,runTest "examples"             defBuild [] "CochleaPlus"  ([""],"CochleaPlus_topEntity",False)
      ,runTest "examples"             defBuild ["-fclash-component-prefix","test"] "FIR" (["","test_FIR_testBench"],"test_FIR_testBench",True )
      ,runTest "examples"             defBuild [] "Fifo"         ([""],"Fifo_topEntity",False)
      ,runTest "examples"             defBuild [] "MAC"          (["","MAC_testBench"],"MAC_testBench",True)
      ,runTest "examples"             defBuild [] "MatrixVect"   (["","MatrixVect_testBench"],"MatrixVect_testBench",True)
      ,runTest "examples"             defBuild [] "Queens"       ([""],"Queens_topEntity",False)
      ,runTest "examples"             defBuild [] "Reducer"      ([""],"Reducer_topEntity",False)
      ,runTest "examples"             defBuild [] "Sprockell"    ([""],"Sprockell_topEntity",False)
      ,runTest "examples"             defBuild [] "Windows"      ([""],"Windows_topEntity",False)
      ,runTest ("examples" </> "crc32") defBuild [] "CRC32"      (["","CRC32_testBench"],"CRC32_testBench",True)
      ,runTest ("examples" </> "i2c") defBuild ["-O2","-fclash-component-prefix","test"] "I2C" (["test_i2c","test_bitmaster","test_bytemaster"],"test_i2c",False)
      ]
    , testGroup "unit-tests"
        [ testGroup "Basic"
            [ runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "BangData"            ([""],"BangData_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Trace"               ([""],"Trace_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "ByteSwap32"          (["","ByteSwap32_testBench"],"ByteSwap32_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "CharTest"            (["","CharTest_testBench"],"CharTest_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "ClassOps"            (["","ClassOps_testBench"],"ClassOps_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "CountTrailingZeros"  (["","CountTrailingZeros_testBench"],"CountTrailingZeros_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "DivMod"              ([""],"DivMod_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "IrrefError"          ([""],"IrrefError_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "LambdaDrop"          ([""],"LambdaDrop_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "LotOfStates"         (["","LotOfStates_testBench"],"LotOfStates_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NestedPrimitives"    ([""],"NestedPrimitives_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NestedPrimitives2"   ([""],"NestedPrimitives2_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NORX"                (["","NORX_testBench"],"NORX_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "PatError"            ([""],"PatError_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "PopCount"            (["","PopCount_testBench"],"PopCount_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "RecordSumOfProducts" ([""],"RecordSumOfProducts_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Replace"             (["", "Replace_testBench"],"Replace_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Shift"               ([""],"Shift_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "SimpleConstructor"   ([""],"SimpleConstructor_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TagToEnum"           ([""],"TagToEnum_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TestIndex"           ([""],"TestIndex_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TwoFunctions"        ([""],"TwoFunctions_topEntity",False)

            -- Fails:
            , testGroup "Failing" [
                runFailingTest ("tests" </> "shouldfail") defBuild [] "RecursiveBoxed" (Just "Callgraph after normalisation contains following recursive components")
               , runFailingTest ("tests" </> "shouldfail") defBuild [] "RecursiveDatatype" (Just "Not in normal form: no Letrec")
--                  Disabled, due to it eating gigabytes of memory:
--                , runFailingTest ("tests" </> "shouldfail") defBuild [] "RecursivePoly" (Just "??")
              ]
            ]
        , testGroup "BitVector"
            [ runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "Box"     (["","Box_testBench"],"Box_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "BoxGrow" (["","BoxGrow_testBench"],"BoxGrow_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "RePack"  ([""],"RePack_topEntity",False)
            ]
        , testGroup "BlackBox"
            [ outputTest ("tests" </> "shouldwork" </> "BlackBox") VHDL [] "TemplateFunction" ([""],"TemplateFunction_topEntity",False) "main"
            , outputTest ("tests" </> "shouldwork" </> "BlackBox") VHDL [] "BlackBoxFunction" ([""],"BlackBoxFunction_topEntity",False) "main"
            ]
        , testGroup "BoxedFunctions"
            [ runTest ("tests" </> "shouldwork" </> "BoxedFunctions") defBuild [] "DeadRecursiveBoxed" ([""],"DeadRecursiveBoxed_topEntity",False)
            ]
        , testGroup "CSignal"
            [ runTest ("tests" </> "shouldwork" </> "CSignal") defBuild [] "CBlockRamTest" ([""],"CBlockRamTest_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "CSignal") defBuild [] "MAC"           ([""],"MAC_topEntity",False)
            ]
#ifdef COSIM
        , testGroup "CoSim"
            [ runTest ("tests" </> "shouldwork" </> "CoSim") Verilog ["-i../../../clash-cosim/src/prims/verilog"] "Multiply" (["","Multiply_testBench"],"Multiply_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CoSim") Verilog ["-i../../../clash-cosim/src/prims/verilog"] "Register" (["","Register_testBench"],"Register_testBench",True)
            ]
#endif
        , testGroup "CustomReprs"
            [ runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC") defBuild [] "RotateC" (["", "RotateC_testBench"],"RotateC_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC") defBuild [] "ReprCompact" (["", "ReprCompact_testBench"],"ReprCompact_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC") defBuild [] "ReprCompactScrambled" (["", "ReprCompactScrambled_testBench"],"ReprCompactScrambled_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC") defBuild [] "ReprLastBitConstructor" (["", "ReprLastBitConstructor_testBench"],"ReprLastBitConstructor_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC") VHDL     [] "ReprStrangeMasks" (["", "ReprStrangeMasks_testBench"],"ReprStrangeMasks_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC") defBuild [] "ReprWide" (["", "ReprWide_testBench"],"ReprWide_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateC") defBuild [] "RotateCScrambled" (["", "RotateCScrambled_testBench"],"RotateCScrambled_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "RotateCNested") defBuild [] "RotateCNested" (["", "RotateCNested_testBench"],"RotateCNested_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "Rotate") defBuild [] "Rotate" (["", "Rotate_testBench"],"Rotate_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "Deriving") defBuild [] "BitPackDerivation" (["", "BitPackDerivation_testBench"],"BitPackDerivation_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "CustomReprs" </> "Indexed") defBuild [] "Indexed" (["", "Indexed_testBench"],"Indexed_testBench",True)
            ]
        , testGroup "Feedback"
            [ runTest ("tests" </> "shouldwork" </> "Feedback") defBuild [] "Fib" (["","Fib_testBench"],"Fib_testBench",True)
            ]
        , testGroup "Fixed"
            [ runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "Mixer"      (["","Mixer_testBench"],"Mixer_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "SFixedTest" (["","SFixedTest_testBench"],"SFixedTest_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "SatWrap"    ([""],"SatWrap_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Fixed") defBuild [] "ZeroInt"    (["","ZeroInt_testBench"],"ZeroInt_testBench",True)
            ]
        , testGroup "Floating"
            [ runTest ("tests" </> "shouldwork" </> "Floating") defBuild ["-fclash-float-support"] "FloatPack" ([""],"FloatPack_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Floating") defBuild ["-fclash-float-support"] "FloatConstFolding" (["","FloatConstFolding_testBench"],"FloatConstFolding_testBench",True)
            ]
        , testGroup "HOPrim"
            [ runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "HOImap"    (["","HOImap_testBench"],"HOImap_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "TestMap"   (["","TestMap_testBench"],"TestMap_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "Transpose" (["","Transpose_testBench"],"Transpose_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "HOPrim") defBuild [] "VecFun"    (["","VecFun_testBench"],"VecFun_testBench",True)
            ]
        , testGroup "Numbers"
            [ runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Bounds"  (["","Bounds_testBench"],"Bounds_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize"  (["","Resize_testBench"],"Resize_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize2" (["","Resize2_testBench"],"Resize2_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "SatMult" ([""],"SatMult_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Strict"  (["","Strict_testBench"],"Strict_testBench",True)
            ]
        , testGroup "Polymorphism"
            [ runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "ExistentialBoxed"  ([""],"ExistentialBoxed_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "FunctionInstances" (["","FunctionInstances_testBench"],"FunctionInstances_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "GADTExistential"   ([""],"GADTExistential_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "LocalPoly"         ([""],"LocalPoly_topEntity",False)
            ]
        , testGroup "RTree"
            [ runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TFold" ([""],"TFold_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TZip"  ([""],"TZip_topEntity",False)
            ]
        , testGroup "Signal"
            [ runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "AlwaysHigh"      ([""],"AlwaysHigh_topEntity",False)
            , outputTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamLazy"    ([""],"BlockRamLazy_topEntity",False) "main"
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamFile"    (["","BlockRamFile_testBench"],"BlockRamFile_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamTest"    ([""],"BlockRamTest_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "MAC"             ([""],"MAC_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "NoCPR"           (["example"],"example",False)
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Ram"             (["","Ram_testBench"],"Ram_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Rom"             (["","Rom_testBench"],"Rom_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RomFile"         (["","RomFile_testBench"],"RomFile_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "SigP"            ([""],"SigP_topEntity",False)

            , runTest ("tests" </> "shouldwork" </> "Signal" </> "BiSignal") defBuild [] "Counter" (["","Counter_testBench"],"Counter_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Signal" </> "BiSignal") defBuild [] "CounterHalfTuple" (["","CounterHalfTuple_testBench"],"CounterHalfTuple_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Signal" </> "BiSignal") defBuild [] "CounterHalfTupleRev" (["","CounterHalfTupleRev_testBench"],"CounterHalfTupleRev_testBench",True)
            ]
        , testGroup "SynthesisAttributes"
            [ outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") defBuild [] "Simple"  ([""],"Simple_topEntity",False) "main"
            , outputTest ("tests" </> "shouldwork" </> "SynthesisAttributes") defBuild [] "Product" ([""],"Product_topEntity",False) "main"
            , runTest    ("tests" </> "shouldwork" </> "SynthesisAttributes") defBuild [] "Product" (["", "Product_testBench"],"Product_testBench",True)
            , testGroup "Failing" [
                runFailingTest ("tests" </> "shouldfail" </> "SynthesisAttributes") defBuild [] "ProductInArgs"   (Just "Attempted to split Product into a number of HDL ports.")
              , runFailingTest ("tests" </> "shouldfail" </> "SynthesisAttributes") defBuild [] "ProductInResult" (Just "Attempted to split Product into a number of HDL ports.")
              ]
            ]
        , testGroup "Testbench"
            [ runTest ("tests" </> "shouldwork" </> "Testbench") defBuild ["-fclash-inline-limit=0"] "TB" (["","TB_testBench"],"TB_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Testbench") defBuild [] "SyncTB"                    (["","SyncTB_testBench"],"SyncTB_testBench",True)
            ]
        , testGroup "Types"
            [ runTest ("tests" </> "shouldwork" </> "Types") defBuild [] "TypeFamilyReduction" ([""],"TypeFamilyReduction_topEntity",False)
            ]
        , testGroup "TopEntity"
            -- VHDL tests disabled for now: I can't figure out how to generate a static name whilst retaining the ability to actually test..
            [ runTest ("tests" </> "shouldwork" </> "TopEntity") Verilog [] "PortNames" (["","PortNames_topEntity","PortNames_testBench"],"PortNames_testBench",True)
            , outputTest ("tests" </> "shouldwork" </> "TopEntity") Verilog [] "PortNames" (["","PortNames_topEntity","PortNames_testBench"],"PortNames_testBench",False) "main"
            , runTest ("tests" </> "shouldwork" </> "TopEntity") Verilog [] "PortProducts" (["","PortProducts_topEntity","PortProducts_testBench"],"PortProducts_testBench",True)
            , outputTest ("tests" </> "shouldwork" </> "TopEntity") Verilog [] "PortProducts" (["","PortProducts_topEntity","PortProducts_testBench"],"PortProducts_testBench",False) "main"
            ]
        , testGroup "Vector"
            [ runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Concat"    (["","Concat_testBench"],"Concat_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DFold"     (["","DFold_testBench"],"DFold_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DFold2"    (["","DFold2_testBench"],"DFold2_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DTFold"    (["","DTFold_testBench"],"DTFold_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "EnumTypes" ([""],"EnumTypes_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FindIndex" (["","FindIndex_testBench"],"FindIndex_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FirOddSize" (["","FirOddSize_testBench"],"FirOddSize_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Fold"      (["","Fold_testBench"],"Fold_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FoldlFuns" ([""],"FoldlFuns_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Foldr"     (["","Foldr_testBench"],"Foldr_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FoldrEmpty" (["","FoldrEmpty_testBench"],"FoldrEmpty_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOClock"   ([""],"HOClock_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOCon"     ([""],"HOCon_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOPrim"    ([""],"HOPrim_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Indices"   (["","Indices_testBench"],"Indices_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Minimum"   (["","Minimum_testBench"],"Minimum_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "MovingAvg" ([""],"MovingAvg_topEntity", False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "PatHOCon"  ([""],"PatHOCon_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Scatter"   (["","Scatter_testBench"],"Scatter_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Split"     ([""],"Split_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "ToList"    (["","ToList_testBench"],"ToList_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Unconcat"  (["","Unconcat_testBench"],"Unconcat_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VACC"      ([""],"VACC_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VEmpty"    (["", "VEmpty_testBench"],"VEmpty_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VIndex"    ([""],"VIndex_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VFold"     (["","VFold_testBench"],"VFold_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VMapAccum" ([""],"VMapAccum_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VMerge"    (["","VMerge_testBench"],"VMerge_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VReplace"  (["","VReplace_testBench"],"VReplace_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VReverse"  (["","VReverse_testBench"],"VReverse_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VRotate"   (["","VRotate_testBench"],"VRotate_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VScan"     ([""],"VScan_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VSelect"   (["","VSelect_testBench"],"VSelect_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VZip"      ([""],"VZip_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VecConst"  ([""],"VecConst_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VecOfSum"  ([""],"VecOfSum_topEntity",False)
            ]
        ]
    ]

type SeqTestTree = IO FilePath -> IO (Maybe Lock, Maybe Lock) -> TestTree


hdlFiles :: String -> IO FilePath -> FilePath -> FilePath -> [FilePath]
hdlFiles ext cwd env subdir = Unsafe.unsafePerformIO $ do
  vhdlDir  <- (</> env </> subdir) <$> cwd
  allFiles <- Directory.getDirectoryContents vhdlDir
  return $ (subdir </>) <$> filter (List.isSuffixOf ext) allFiles

tastyAcquire
  :: [t]
  -> [FilePath]
  -> IO (FilePath, [Maybe Lock])
tastyAcquire seqTests dirs = do
  tests <- newLocks seqTests
  Directory.createDirectoryIfMissing True temporaryDirectory
  tmpDir <- createTempDirectory temporaryDirectory "clash-test-"
  _ <- mapM (Directory.createDirectoryIfMissing True) $ map (tmpDir </>) dirs
  return (tmpDir, tests)

tastyRelease
  :: (FilePath, b)
  -> IO ()
tastyRelease (tmpDir, _) = Directory.removeDirectoryRecursive tmpDir

newLocks
  :: [t]
  -> IO [Maybe Lock]
newLocks tests = sequence [Just <$> newAcquired | _ <- tests]

clashCmd
  :: BuildTarget
  -- ^ Build target
  -> FilePath
  -- ^ Work directory
  -> [String]
  -- ^ Extra arguments
  -> String
  -- ^ Module name
  -> (String, [String])
clashCmd target sourceDir extraArgs modName = ("cabal", ["new-run", "clash", "--"] ++ args)
  where
    args = concat [
        [target']
      , extraArgs
      , [sourceDir </> modName <.> "hs"]
      , ["-i" ++ sourceDir]
      ]

    target' = case target of
      VHDL -> "--vhdl"
      Verilog -> "--verilog"
      SystemVerilog -> "--systemverilog"
      _ -> error $ "Unexpected target in clashCmd: " ++ show target

clashHDL
  :: BuildTarget
  -- ^ Build target
  -> FilePath
  -- ^ Work directory
  -> [String]
  -- ^ Extra arguments
  -> String
  -- ^ Module name
  -> SeqTestTree
clashHDL t sourceDir extraArgs modName =
  let (cmd, args) = clashCmd t sourceDir extraArgs modName in
  testProgram (List.intercalate " " $ "clash" : extraArgs) cmd args PrintStdErr False

ghdlLibrary
  :: FilePath
  -- ^ Directory with modules in it, relative to temporary directory
  -> String
  -- ^ Module name
  -> FilePath
  -- ^ Directory with the VHDL files
  -> SeqTestTree
ghdlLibrary env modName lib cwd =
  testProgram "GHDL (library)" "ghdl" args PrintStdErr False $ (</> env) <$> cwd
      where
        args = ("-i":("--work="++workName):("--workdir="++workdir):"--std=93":hdlFiles "vhdl" cwd env lib')

        lib' = map toLower lib
        workName = case lib' of {[] -> case modName of {"FIR" -> "test_fir_topentity"; _ -> map toLower modName ++ "_topentity"}; k -> k}
        workdir  = case lib' of {[] -> "."; k -> k}

ghdlImport
  :: FilePath
  -- ^ Work directory
  -> [FilePath]
  -- ^ Directories with the VHDL files
  -> SeqTestTree
ghdlImport env subdirs cwd =
  testProgram "GHDL (import)" "ghdl" args PrintStdErr False $ (</> env) <$> cwd
    where
      args = "-i":"--workdir=work":"--std=93":concatMap (hdlFiles "vhdl" cwd env) ((map.map) toLower subdirs)

ghdlMake
  :: FilePath
  -- ^ Work directory
  -> [FilePath]
  -- ^ Directories with the VHDL files
  -> [FilePath]
  -- ^ Library directories
  -> String
  -- ^ Name of the components we want to build
  -> SeqTestTree
ghdlMake env subdirs libs entName cwd =
  testProgram "GHDL (make)" "ghdl"
    (concat
      [["-m"]
      ,["--workdir=work"]
      ,map (\l -> "-P" ++ emptyToDot (map toLower l)) libs
      ,["-o",map toLower (noConflict entName subdirs)]
      ,[entName]
      ])
    PrintStdErr False $ (</> env) <$> cwd
  where
    emptyToDot [] = "."
    emptyToDot k  = k

ghdlSim
  :: FilePath
  -- ^ Directory with the compiled simulation
  -> String
  -- ^ Name of the testbench executable
  -> SeqTestTree
ghdlSim env tbName cwd =
 let args = ["-r","--workdir=work",tbName,"--assert-level=error"] in
 testProgram "GHDL (sim)" "ghdl" args PrintStdErr False $ (</> env) <$> cwd

iverilog
  :: FilePath
  -- ^ Work directory
  -> [FilePath]
  -- ^ Directories with the Verilog files
  -> String
  -- ^ Name of the component we want to build
  -> SeqTestTree
iverilog env subdirs entName cwd =
  testProgram "iverilog" "iverilog" args PrintStdErr False $ (</> env) <$> cwd
    where
      args = ("-g2":"-s":entName:"-o":noConflict entName subdirs:concatMap (hdlFiles "v" cwd env) subdirs)

noConflict :: String -> [String] -> String
noConflict nm seen
  | nm `elem` seen = go (0 :: Int)
  | otherwise      = nm
  where
    go n
      | (nm ++ show n) `elem` seen = go (n+1)
      | otherwise                  = (nm ++ show n)

vvp
  :: FilePath
  -- ^ Directory with the compiled simulation
  -> String
  -- ^ Name of the testbench object
  -> SeqTestTree
vvp env entName cwd =
  testProgram "vvp" "vvp" [entName] PrintStdOut True $ (</> env) <$> cwd

vlog
  :: FilePath
  -- ^ Work directory
  -> [FilePath]
  -- ^ Directory with the SystemVerilog files
  -> [SeqTestTree]
vlog env subdirs =
  [ \cwd -> testProgram "vlib" "vlib" ["work"] PrintStdErr False $ (</> env) <$> cwd
  , \cwd -> testProgram "vlog" "vlog" ("-sv":"-work":"work":typFiles ++ allFiles) PrintStdErr False $ (</> env) <$> cwd
  ]
  where
    typFiles = map (\d -> d </> "*_types.sv") subdirs
    allFiles = map (\d -> d </> "*.sv") subdirs

vsim :: FilePath -> String -> SeqTestTree
vsim env entName cwd =
  testProgram "vsim" "vsim" args PrintStdErr False $ (</> env) <$> cwd
  where
    args = ["-batch", "-do", doScript, entName]

    doScript = List.intercalate ";"
      [ "run -all"
      , unwords
         ["if {[string equal ready [runStatus]]}"
         ,"then {quit -f}"
         ,"else {quit -code 1 -f}"
         ]
      , "quit -code 2 -f"
      ]

createTestTrees
  :: TestName
  -> [SeqTestTree]
  -> IO (FilePath, [Maybe Lock])
  -> TestTree
createTestTrees modName tests tmpDirAndLocks =
  testGroup modName $ [t tmpDir ((!!i) <$> prevNexts) | (i, t) <- zip [0..] tests]
    where
      tmpDir    = fst <$> tmpDirAndLocks
      locks     = snd <$> tmpDirAndLocks
      prevs     = (([Nothing] ++) <$>) locks
      nexts     = ((++ [Nothing]) <$>) locks
      prevNexts = zip <$> prevs <*> nexts

type ExtraTestFunc
  = FilePath -- Env dir
 -> BuildTarget
 -> FilePath -- HDL dir
 -> FilePath -- Mod dir
 -> String   -- Mod name
 -> String   -- (Haskell) ent name
 -> SeqTestTree

runTest'
  :: FilePath
  -> BuildTarget
  -> [String]
  -> String
  -> ([String],String,Bool)
  -> [ExtraTestFunc]
  -> [TestTree]
runTest' env VHDL extraArgs modName (subdirs, entName, doSim) extraTests =
  [withResource acquire tastyRelease (createTestTrees "VHDL" seqTests)]
    where
      cwDir   = Unsafe.unsafePerformIO $ Directory.getCurrentDirectory
      vhdlDir = "vhdl"
      modDir  = vhdlDir </> modName
      workDir = modDir </> "work"
      acquire = tastyAcquire seqTests [vhdlDir, modDir, workDir]

      libs
        | length subdirs == 1 = []
        | otherwise          = subdirs List.\\ [entName]

      seqTests = concat $ [
           [clashHDL VHDL (cwDir </> env) extraArgs modName]
          , map (ghdlLibrary modDir modName) libs
          , [ghdlImport modDir (subdirs List.\\ libs)]
          , [ghdlMake modDir subdirs libs entName]
        ] ++ [if doSim then [ghdlSim modDir (noConflict entName subdirs)] else []]
          ++ [map (\f -> f (cwDir </> env) VHDL vhdlDir modDir modName entName) extraTests]

runTest' env Verilog extraArgs modName (subdirs, entName, doSim) extraTests =
  [withResource acquire tastyRelease (createTestTrees "Verilog" seqTests)]
    where
      cwDir      = Unsafe.unsafePerformIO $ Directory.getCurrentDirectory
      verilogDir = "verilog"
      modDir     = verilogDir </> modName
      acquire    = tastyAcquire seqTests [verilogDir, modDir]

      seqTests =
        [ clashHDL Verilog (cwDir </> env) extraArgs modName
        , iverilog modDir subdirs entName
        ] ++ if doSim then [vvp modDir (noConflict entName subdirs)] else []
          ++ map (\f -> f (cwDir </> env) Verilog verilogDir modDir modName entName) extraTests

runTest' env SystemVerilog extraArgs modName (subdirs,entName,doSim) extraTests =
  [withResource acquire tastyRelease (createTestTrees "SystemVerilog" seqTests)]
    where
      cwDir   = Unsafe.unsafePerformIO $ Directory.getCurrentDirectory
      svDir   = "systemverilog"
      modDir  = svDir </> modName
      acquire = tastyAcquire seqTests [svDir, modDir]

      seqTests =
        concat $
          [ [ clashHDL SystemVerilog (cwDir </> env) extraArgs modName ]
          , vlog modDir subdirs
          ] ++ [if doSim then [vsim modDir entName] else []]
            ++ [map (\f -> f (cwDir </> env) SystemVerilog svDir modDir modName entName) extraTests]

runTest' env Both extraArgs modName entNameM extraTests = concat
  [ runTest' env VHDL extraArgs modName entNameM extraTests
  , runTest' env Verilog extraArgs modName entNameM extraTests
  ]

runTest' env All extraArgs modName entNameM extraTests = concat
  [ runTest' env VHDL extraArgs modName entNameM extraTests
  , runTest' env Verilog extraArgs modName entNameM extraTests
  , runTest' env SystemVerilog extraArgs modName entNameM extraTests
  ]

runTest
  :: FilePath
  -> BuildTarget
  -> [String]
  -> String
  -> ([String],String,Bool)
  -> TestTree
runTest env target extraArgs modName (subdirs,entName,doSim) =
  testGroup modName (runTest' env target extraArgs modName (subdirs,entName,doSim) [])

runFailingTest'
  :: FilePath
  -- ^ Work directory
  -> BuildTarget
  -- ^ Build target
  -> [String]
  -- ^ Extra arguments
  -> String
  -- ^ Module name
  -> Maybe Text.Text
  -- ^ Expected stderr
  -> TestTree
runFailingTest' _ All  _ _ _ = error "Unexpected test target: All"
runFailingTest' _ Both _ _ _ = error "Unexpected test target: Both"
runFailingTest' env target extraArgs modName expectedStderr =
  let cwDir       = Unsafe.unsafePerformIO $ Directory.getCurrentDirectory in
  let (cmd, args) = clashCmd target (cwDir </> env) ("-fclash-nocache" : extraArgs) modName in
  let testName    = "clash (" ++ show target ++ ")" in
  testFailingProgram
    testName
    cmd
    args
    PrintNeither
    False
    Nothing
    expectedStderr
    (Directory.getCurrentDirectory)
    (return (Nothing, Nothing))

runFailingTest
  :: FilePath
  -- ^ Work directory
  -> BuildTarget
  -- ^ Build target
  -> [String]
  -- ^ Extra arguments
  -> String
  -- ^ Module name
  -> Maybe Text.Text
  -- ^ Expected stderr
  -> TestTree
runFailingTest env Both extraArgs modName expectedStderr =
  testGroup modName
    [ runFailingTest' env VHDL extraArgs modName expectedStderr
    , runFailingTest' env Verilog extraArgs modName expectedStderr
    ]
runFailingTest env All extraArgs modName expectedStderr =
  testGroup modName
    [ runFailingTest' env VHDL extraArgs modName expectedStderr
    , runFailingTest' env Verilog extraArgs modName expectedStderr
    , runFailingTest' env SystemVerilog extraArgs modName expectedStderr
    ]
runFailingTest env target extraArgs modName expectedStderr =
  testGroup modName [ runFailingTest' env target extraArgs modName expectedStderr ]

outputTest'
  :: String
  -> ExtraTestFunc
outputTest' funcName env target _hdlDir modDir modName entityName =
  testProgram "Output file test" "cabal" args PrintStdErr False
    where
      -- TODO: Clean up flags
      args = [ "new-exec"
             , "--"
             , "runghc"
             , "-XBinaryLiterals"
             , "-XConstraintKinds"
             , "-XDataKinds"
             , "-XDeriveAnyClass"
             , "-XDeriveGeneric"
             , "-XDeriveLift"
             , "-XExplicitForAll"
             , "-XExplicitNamespaces"
             , "-XFlexibleContexts"
             , "-XKindSignatures"
             , "-XMagicHash"
             , "-XMonoLocalBinds"
             , "-XScopedTypeVariables"
             , "-XTemplateHaskell"
             , "-XTemplateHaskellQuotes"
             ,  "-XTypeApplications"
             , "-XTypeFamilies"
             , "-XTypeOperators"
             , "-XNoImplicitPrelude"
             , "-XNoMonomorphismRestriction"
             , "-XNoStrict"
             , "-XNoStrictData"
             , "-main-is"
             , "--ghc-arg=" ++ modName ++ "." ++ funcName ++ show target
             , env </> modName <.> "hs"
             , modDir
             , filename
             ]
      filename =
        case target of
          VHDL          -> map toLower $ entityName ++ ".vhdl"
          Verilog       -> entityName ++ ".v"
          SystemVerilog -> entityName ++ ".sv"
          _             -> error $ "Unexpected target: " ++ show target

outputTest
  :: FilePath
  -> BuildTarget
  -> [String]
  -> String
  -> ([String],String,Bool)
  -> String
  -> TestTree
outputTest env target extraArgs modName entNameM funcName =
  let testName = modName ++ " [output test]" in
  testGroup testName (runTest' env target extraArgs modName entNameM [outputTest' funcName])
