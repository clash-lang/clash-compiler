{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Program

import           Data.Char        (toLower)
import qualified Data.List        as List
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
      [runTest "examples"             defBuild [] "ALU"          ([""],"ALU_topEntity",False)
      ,runTest "examples"             VHDL     [] "Blinker"      (["blinker"],"blinker",False)
      ,runTest "examples"             defBuild [] "BlockRamTest" ([""],"BlockRamTest_topEntity",False)
      ,runTest "examples"             defBuild [] "Calculator"   (["","Calculator_testBench"],"Calculator_testBench",True )
      ,runTest "examples"             defBuild [] "CochleaPlus"  ([""],"CochleaPlus_topEntity",False) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest "examples"             defBuild [] "FIR"          (["","FIR_testBench"],"FIR_testBench",True ) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest "examples"             defBuild [] "Fifo"         ([""],"Fifo_topEntity",False) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest "examples"             defBuild [] "MAC"          (["","MAC_testBench"],"MAC_testBench",True)
      ,runTest "examples"             defBuild [] "MatrixVect"   (["","MatrixVect_testBench"],"MatrixVect_testBench",True)
      ,runTest "examples"             defBuild [] "Queens"       ([""],"Queens_topEntity",False) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest "examples"             defBuild [] "Reducer"      ([""],"Reducer_topEntity",False)
      ,runTest "examples"             defBuild [] "Sprockell"    ([""],"Sprockell_topEntity",False)
      ,runTest "examples"             defBuild [] "Windows"      ([""],"Windows_topEntity",False) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest ("examples" </> "crc32") defBuild [] "CRC32"      (["","CRC32_testBench"],"CRC32_testBench",True)  -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
      ,runTest ("examples" </> "i2c") defBuild ["-O2"] "I2C"     (["i2c","bitmaster","bytemaster"],"i2c",False)
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
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "LotOfStates"         (["","LotOfStates_testBench"],"LotOfStates_testBench",True) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NestedPrimitives"    ([""],"NestedPrimitives_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NestedPrimitives2"   ([""],"NestedPrimitives2_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "NORX"                (["","NORX_testBench"],"NORX_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "PatError"            ([""],"PatError_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "PopCount"            (["","PopCount_testBench"],"PopCount_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "RecordSumOfProducts" ([""],"RecordSumOfProducts_topEntity",False) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "Shift"               ([""],"Shift_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "SimpleConstructor"   ([""],"SimpleConstructor_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TagToEnum"           ([""],"TagToEnum_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TestIndex"           ([""],"TestIndex_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Basic") defBuild [] "TwoFunctions"        ([""],"TwoFunctions_topEntity",False)
            ]
        , testGroup "BitVector"
            [ runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "Box"     (["","Box_testBench"],"Box_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "BoxGrow" (["","BoxGrow_testBench"],"BoxGrow_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "BitVector") defBuild [] "RePack"  ([""],"RePack_topEntity",False)
            ]
        , testGroup "BoxedFunctions"
            [ runTest ("tests" </> "shouldwork" </> "BoxedFunctions") defBuild [] "DeadRecursiveBoxed" ([""],"DeadRecursiveBoxed_topEntity",False)
            ]
        , testGroup "CSignal"
            [ runTest ("tests" </> "shouldwork" </> "CSignal") defBuild [] "CBlockRamTest" ([""],"CBlockRamTest_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "CSignal") defBuild [] "MAC"           ([""],"MAC_topEntity",False)
            ]
        , testGroup "Feedback" -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
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
            [ runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Bounds"  (["","Bounds_testBench"],"Bounds_testBench",True) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize"  (["","Resize_testBench"],"Resize_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Resize2" (["","Resize2_testBench"],"Resize2_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "SatMult" ([""],"SatMult_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Numbers") defBuild [] "Strict"  (["","Strict_testBench"],"Strict_testBench",True)
            ]
        , testGroup "Polymorphism"
            [ runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "ExistentialBoxed" ([""],"ExistentialBoxed_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Polymorphism") defBuild [] "LocalPoly"        ([""],"LocalPoly_topEntity",False)
            ]
        , testGroup "RTree"
            [ runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TFold" ([""],"TFold_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "RTree") defBuild [] "TZip"  ([""],"TZip_topEntity",False)
            ]
        , testGroup "Signal"
            [ runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "AlwaysHigh"   ([""],"AlwaysHigh_topEntity",False) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamFile" (["","BlockRamFile_testBench"],"BlockRamFile_testBench",True) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "BlockRamTest" ([""],"BlockRamTest_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "MAC"          ([""],"MAC_topEntity",False) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "NoCPR"        (["example"],"example",False)
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Ram"          (["","Ram_testBench"],"Ram_testBench",True) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "Rom"          (["","Rom_testBench"],"Rom_testBench",True) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "RomFile"      (["","RomFile_testBench"],"RomFile_testBench",True) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            , runTest ("tests" </> "shouldwork" </> "Signal") defBuild [] "SigP"         ([""],"SigP_topEntity",False) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/11525
            ]
        , testGroup "Testbench" -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/1152
            [ runTest ("tests" </> "shouldwork" </> "Testbench") defBuild ["-fclash-inline-limit=0"] "TB" (["","TB_testBench"],"TB_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Testbench") defBuild [] "SyncTB"                    (["","SyncTB_testBench"],"SyncTB_testBench",True)
            ]
        , testGroup "Vector"
            [ runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Concat"    (["","Concat_testBench"],"Concat_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DFold"     (["","DFold_testBench"],"DFold_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DFold2"    (["","DFold2_testBench"],"DFold2_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "DTFold"    (["","DTFold_testBench"],"DTFold_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "EnumTypes" ([""],"EnumTypes_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "FindIndex" (["","FindIndex_testBench"],"FindIndex_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Fold"      (["","Fold_testBench"],"Fold_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Foldr"     (["","Foldr_testBench"],"Foldr_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOClock"   ([""],"HOClock_topEntity",False) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/115
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOCon"     ([""],"HOCon_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "HOPrim"    ([""],"HOPrim_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Minimum"   (["","Minimum_testBench"],"Minimum_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "MovingAvg" ([""],"MovingAvg_topEntity", False) -- Broken on GHC 8.0 due to: https://ghc.haskell.org/trac/ghc/ticket/115
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "PatHOCon"  ([""],"PatHOCon_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Scatter"   (["","Scatter_testBench"],"Scatter_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Split"     ([""],"Split_topEntity",False)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "ToList"    (["","ToList_testBench"],"ToList_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "Unconcat"  (["","Unconcat_testBench"],"Unconcat_testBench",True)
            , runTest ("tests" </> "shouldwork" </> "Vector") defBuild [] "VACC"      ([""],"VACC_topEntity",False)
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

clashHDL :: BuildTarget -> FilePath -> [String] -> String -> TestTree
clashHDL t env extraArgs modName =
  testProgram ("Clash(" ++ show t ++ ")")
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

ghdlLibrary
  :: FilePath
  -- ^ Working directory
  -> String
  -- ^ Module name
  -> FilePath
  -- ^ Directory with the VHDL files
  -> TestTree
ghdlLibrary env modName lib = withResource (return env) (const (return ()))
    (\d -> testProgram "GHDL (library)" "ghdl"
      ("-i":("--work="++workName):("--workdir="++workdir):"--std=93":vhdlFiles d lib') (Just env) False False)
  where
    lib' = map toLower lib
    workName = case lib' of {[] -> map toLower modName ++ "_topentity"; k -> k}
    workdir  = case lib' of {[] -> "."; k -> k}

    vhdlFiles :: IO FilePath -> FilePath -> [FilePath]
    vhdlFiles d subdir =  map (subdir </>)
                       .  Unsafe.unsafePerformIO
                       $  filter (List.isSuffixOf "vhdl")
                      <$> (Directory.getDirectoryContents . (</> subdir) =<< d)

ghdlImport
  :: FilePath
  -- ^ Working directory
  -> [FilePath]
  -- ^ Directories with the VHDL files
  -> TestTree
ghdlImport env subdirs = withResource (return env) (const (return ()))
    (\d -> testProgram "GHDL (import)" "ghdl"
      ("-i":"--workdir=work":"--std=93":concatMap (vhdlFiles d) ((map.map) toLower subdirs)) (Just env) False False)
  where
    vhdlFiles :: IO FilePath -> FilePath -> [FilePath]
    vhdlFiles d subdir =  map (subdir </>)
                       .  Unsafe.unsafePerformIO
                       $  filter (List.isSuffixOf "vhdl")
                      <$> (Directory.getDirectoryContents . (</> subdir) =<< d)

ghdlMake
  :: FilePath
  -- ^ Working directory
  -> [FilePath]
  -- ^ Directories with the VHDL files
  -> [FilePath]
  -- ^ Library directories
  -> String
  -- ^ Name of the components we want to build
  -> TestTree
ghdlMake env subdirs libs entName =
  testProgram "GHDL (make)" "ghdl"
    (concat
      [["-m"]
      ,["--workdir=work"]
      ,map (\l -> "-P" ++ emptyToDot (map toLower l)) libs
      ,["-o",map toLower (noConflict entName subdirs)]
      ,[entName]
      ])
    (Just env) False False
  where
    emptyToDot [] = "."
    emptyToDot k  = k

ghdlSim
  :: FilePath
  -- ^ Directory with the compiled simulation
  -> String
  -- ^ Name of the testbench executable
  -> TestTree
ghdlSim env tbName = testProgram "GHDL (sim)" "ghdl"
  ["-r","--workdir=work",tbName,"--assert-level=error"] (Just env) False False

iverilog
  :: FilePath
  -- ^ Working directory
  -> [FilePath]
  -- ^ Directories with the Verilog files
  -> String
  -- ^ Name of the component we want to build
  -> TestTree
iverilog env subdirs entName = withResource (return env) (const (return ()))
    (\d -> testProgram "iverilog" "iverilog"
              ("-g2":"-s":entName:"-o":noConflict entName subdirs:
               concatMap (verilogFiles d) subdirs) (Just env) False False)
  where
    verilogFiles :: IO FilePath -> FilePath -> [FilePath]
    verilogFiles d subdir =  map (subdir </>)
                          .  Unsafe.unsafePerformIO
                          $  filter (List.isSuffixOf "v")
                         <$> (Directory.getDirectoryContents . (</> subdir) =<< d)

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
  -> TestTree
vvp env entName = testProgram "vvp" "vvp" [entName] (Just env) False True

vlog
  :: FilePath
  -- ^ Working directory
  -> [FilePath]
  -- ^ Directory with the SystemVerilog files
  -> TestTree
vlog env subdirs = testGroup "vlog"
  [ testProgram "vlib" "vlib" ["work"] (Just env) False False
  , testProgram "vlog" "vlog" ("-sv":"-work":"work":typFiles ++ allFiles) (Just env) False False
  ]
  where
    typFiles = map (\d -> d </> "*_types.sv") subdirs
    allFiles = map (\d -> d </> "*.sv") subdirs

vsim :: FilePath -> String -> TestTree
vsim env entName =
  testProgram "vsim" "vsim"
    ["-batch","-do",doScript,entName] (Just env) False False
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
        -> ([String],String,Bool)
        -> TestTree
runTest env VHDL extraArgs modName (subdirs,entName,doSim) = withResource aquire release (const grp)
  where
    vhdlDir   = env </> "vhdl"
    modDir    = vhdlDir </> modName
    workdir   = modDir </> "work"
    aquire    = Directory.createDirectoryIfMissing True workdir
    release _ = Directory.removeDirectoryRecursive vhdlDir

    libs
      | length subdirs == 1 = []
      | otherwise          = subdirs List.\\ [entName]

    grp       = testGroup modName $ concat
                  [ [clashHDL VHDL env extraArgs modName]
                  , map (ghdlLibrary modDir modName) libs
                  , [ghdlImport modDir (subdirs List.\\ libs)]
                  , [ghdlMake modDir subdirs libs entName]
                  ] ++ if doSim then [ghdlSim modDir (noConflict entName subdirs)] else []

runTest env Verilog extraArgs modName (subdirs,entName,doSim) =
    withResource (return ()) release (const grp)
  where
    verilogDir = env </> "verilog"
    modDir     = verilogDir </> modName
    release _  = Directory.removeDirectoryRecursive verilogDir

    grp        = testGroup modName $
                   [ clashHDL Verilog env extraArgs modName
                   , iverilog modDir subdirs entName
                   ] ++ if doSim then [vvp modDir (noConflict entName subdirs)] else []

runTest env SystemVerilog extraArgs modName (subdirs,entName,doSim) =
    withResource (return ()) release (const grp)
  where
    svDir     = env </> "systemverilog"
    modDir    = svDir </> modName
    release _ = Directory.removeDirectoryRecursive svDir

    grp       = testGroup modName $
                  [ clashHDL SystemVerilog env extraArgs modName
                  , vlog modDir subdirs
                  ] ++ if doSim then [vsim modDir entName] else []

runTest env Both extraArgs modName entNameM = testGroup "VHDL & Verilog"
  [ runTest env VHDL extraArgs modName entNameM
  , runTest env Verilog extraArgs modName entNameM
  ]

runTest env All extraArgs modName entNameM = testGroup "VHDL & Verilog & SystemVerilog"
  [ runTest env VHDL extraArgs modName entNameM
  , runTest env Verilog extraArgs modName entNameM
  , runTest env SystemVerilog extraArgs modName entNameM
  ]
