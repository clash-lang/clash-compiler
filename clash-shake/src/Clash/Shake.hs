{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Shake
    ( ClashProject(..)
    , HDL(..)
    , clashShake
    , ClashKit(..)
    , clashRules
    , XilinxTarget(..), papilioPro, papilioOne, nexysA750T
    , xilinxISE
    , xilinxVivado
    , hexImage
    ) where

import           Development.Shake
  (CmdOption(Cwd), (<//>), (%>), shakeFiles, alwaysRerun, need, phony,
   putNormal, removeFilesAfter, cmd_, readFile', getDirectoryFiles,
   writeFileChanged, copyFileChanged, shakeArgs, shakeOptions, want)
import qualified Development.Shake as Shake
import           Development.Shake.Config
  (getConfig, usingConfigFile, readConfigFile, usingConfig)
import           Development.Shake.FilePath
  ((</>), (<.>), joinPath, splitPath, takeBaseName, makeRelative)


import           Control.Monad.Trans

import           Text.Mustache ((~>), object)
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Types as Mustache

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.Char (toLower)
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified System.Directory as Dir

import           Clash.Driver.Types
import           Clash.Prelude (pack)

data HDL
    = VHDL
    | Verilog
    | SystemVerilog
    deriving (Eq, Enum, Bounded, Show, Read)

hdlDir :: HDL -> FilePath
hdlDir VHDL = "vhdl"
hdlDir Verilog = "verilog"
hdlDir SystemVerilog = "systemverilog"

hdlExt :: HDL -> FilePath
hdlExt VHDL = "vhdl"
hdlExt Verilog = "v"
hdlExt SystemVerilog = "sv"

data XilinxTarget = XilinxTarget
    { targetFamily :: String
    , targetDevice :: String
    , targetPackage :: String
    , targetSpeed :: String
    }

targetMustache :: XilinxTarget -> [Mustache.Pair]
targetMustache XilinxTarget{..} =
    [ "targetFamily" ~> T.pack targetFamily
    , "targetDevice" ~> T.pack targetDevice
    , "targetPackage" ~> T.pack targetPackage
    , "targetSpeed" ~> T.pack targetSpeed
    , "part" ~> T.pack (targetDevice <> targetPackage <> targetSpeed)
    ]

papilioPro :: XilinxTarget
papilioPro = XilinxTarget "Spartan6" "xc6slx9" "tqg144" "-2"

papilioOne :: XilinxTarget
papilioOne = XilinxTarget "Spartan3E" "xc3s500e" "vq100" "-5"

nexysA750T :: XilinxTarget
nexysA750T = XilinxTarget "Artrix7" "xc7a50t" "icsg324" "-1L"

data ClashProject = ClashProject
    { projectName :: String
    , clashModule :: String
    , clashTopName :: String
    , topName :: String
    , clashFlags :: [String]
    , shakeDir :: FilePath
    , buildDir :: FilePath
    , clashDir :: FilePath
    }

type ClashRules = ReaderT ClashProject Shake.Rules

data ClashKit = ClashKit
    { clash :: String -> [String] -> Shake.Action ()
    , manifestSrcs :: Shake.Action [FilePath]
    }

clashRules :: HDL -> FilePath -> Shake.Action () -> ClashRules ClashKit
clashRules hdl srcDir extraGenerated = do
    ClashProject{..} <- ask
    let synDir = buildDir </> clashDir
        rootDir = joinPath . map (const "..") . splitPath $ buildDir
        srcDir' = rootDir </> srcDir

    let clash cmd args = do
            clashExe <- fromMaybe ("stack exec --") <$> getConfig "CLASH"
            Shake.cmd_ (Cwd buildDir) clashExe
              ([cmd, "-i" <> srcDir', "-outputdir", clashDir] <> clashFlags <> args)

    let manifest = synDir </> hdlDir hdl </> clashModule </> clashTopName </> clashTopName <.> "manifest"
        manifestSrcs = do
            Shake.need [manifest]
            Manifest{..} <- read <$> readFile' manifest
            let clashSrcs = map T.unpack componentNames <>
                            [ map toLower clashTopName <> "_types" | hdl == VHDL ]
            return [ synDir </> hdlDir hdl </> clashModule </> clashTopName </> c <.> hdlExt hdl | c <- clashSrcs ]

    lift $ do
      synDir </> hdlDir hdl <//> "*.manifest" %> \_out -> do
          let src = srcDir </> clashModule <.> "hs" -- TODO
          alwaysRerun
          need [ src ]
          extraGenerated
          clash "clash" [case hdl of { VHDL -> "--vhdl"; Verilog -> "--verilog"; SystemVerilog -> "--systemverilog" }, rootDir </> src]

      phony "clashi" $ do
          let src = srcDir </> clashModule <.> "hs" -- TODO
          clash "clashi" [rootDir </> src]

      phony "clash" $ do
          need [manifest]

      phony "clean-clash" $ do
          putNormal $ "Cleaning files in " ++ synDir
          removeFilesAfter synDir [ "//*" ]

    let kit = ClashKit{..}
    return kit


xilinxISE :: ClashKit -> XilinxTarget -> FilePath -> FilePath -> ClashRules ()
xilinxISE ClashKit{..} fpga srcDir targetDir = do
    ClashProject{..} <- ask
    let outDir = buildDir </> targetDir
        rootDir = joinPath . map (const "..") . splitPath $ outDir

    let ise tool args = do
            root <- getConfig "ISE_ROOT"
            wrap <- getConfig "ISE"
            let exe = case (wrap, root) of
                    (Just wrap1, _) -> [wrap1, tool]
                    (Nothing, Just root1) -> [root1 </> "ISE/bin/lin64" </> tool]
                    (Nothing, Nothing) -> error "ISE_ROOT or ISE must be set in build.mk"
            cmd_ (Cwd outDir) exe args

    let getFiles dir pats = getDirectoryFiles srcDir [ dir </> pat | pat <- pats ]
        hdlSrcs = getFiles "src-hdl" ["*.vhdl", "*.v", "*.ucf" ]
        ipCores = getFiles "ipcore_dir" ["*.xco", "*.xaw"]

    lift $ do
        outDir <//> "*.tcl" %> \out -> do
            let src0 = shakeDir </> "xilinx-ise/project.tcl.mustache"
            s <- T.pack <$> readFile' src0
            alwaysRerun

            srcs1 <- manifestSrcs
            srcs2 <- hdlSrcs
            cores <- ipCores

            template <- case Mustache.compileTemplate src0 s of
                Left err -> fail (show err)
                Right template -> return template
            let values = object . mconcat $
                         [ [ "project" ~> T.pack projectName ]
                         , [ "top" ~> T.pack topName ]
                         , targetMustache fpga
                         , [ "srcs" ~> mconcat
                             [ [ object [ "fileName" ~> (rootDir </> src1) ] | src1 <- srcs1 ]
                             , [ object [ "fileName" ~> (rootDir </> srcDir </> src1) ] | src1 <- srcs2 ]
                             , [ object [ "fileName" ~> core ] | core <- cores ]
                             ]
                           ]
                         , [ "ipcores" ~> [ object [ "name" ~> takeBaseName core ] | core <- cores ] ]
                         ]
            writeFileChanged out . T.unpack $ Mustache.substitute template values

        outDir </> "ipcore_dir" <//> "*" %> \out -> do
            let src = srcDir </> makeRelative outDir out
            copyFileChanged src out

        phony (takeBaseName targetDir </> "ise") $ do
            need [outDir </> projectName <.> "tcl"]
            ise "ise" [outDir </> projectName <.> "tcl"]

        phony (takeBaseName targetDir </> "bitfile") $ do
            need [outDir </> topName <.> "bit"]

        outDir </> topName <.> "bit" %> \_out -> do
            srcs1 <- manifestSrcs
            srcs2 <- hdlSrcs
            cores <- ipCores
            need $ mconcat
              [ [ outDir </> projectName <.> "tcl" ]
              , [ src | src <- srcs1 ]
              , [ srcDir </> src | src <- srcs2 ]
              , [ outDir </> core | core <- cores ]
              ]
            ise "xtclsh" [projectName <.> "tcl", "rebuild_project"]

xilinxVivado :: ClashKit -> XilinxTarget -> FilePath -> FilePath -> ClashRules ()
xilinxVivado ClashKit{..} fpga srcDir targetDir = do
    ClashProject{..} <- ask
    let outDir = buildDir </> targetDir
        projectDir = outDir </> projectName
        xpr = projectDir </> projectName <.> "xpr"

    let vivado tool args = do
            root0 <- getConfig "VIVADO_ROOT"
            wrap0 <- getConfig "VIVADO"
            let exe = case (wrap0, root0) of
                    (Just wrap1, _) -> [wrap1, tool]
                    (Nothing, Just root1) -> [root1 </> "bin" </> tool]
                    (Nothing, Nothing) -> error "VIVADO_ROOT or VIVADO must be set in build.mk"
            cmd_ (Cwd outDir) exe args
        vivadoBatch tcl = do
            need [outDir </> tcl]
            vivado "vivado"
              [ "-mode", "batch"
              , "-nojournal"
              , "-nolog"
              , "-source", tcl
              ]

    let getFiles dir pats = getDirectoryFiles srcDir [ dir </> pat | pat <- pats ]
        hdlSrcs = getFiles "src-hdl" ["*.vhdl", "*.v" ]
        constrSrcs = getFiles "src-hdl" ["*.xdc" ]
        ipCores = getFiles "ip" ["*.xci"]

    lift $ do
        xpr %> \_out -> vivadoBatch "project.tcl"

        outDir </> "project.tcl" %> \out -> do
            let src = shakeDir </> "xilinx-vivado/project.tcl.mustache"
            s <- T.pack <$> readFile' src
            alwaysRerun

            srcs1 <- manifestSrcs
            srcs2 <- hdlSrcs
            cores <- ipCores
            constrs <- constrSrcs

            template <- case Mustache.compileTemplate src s of
                Left err -> fail (show err)
                Right template -> return template
            let values = object . mconcat $
                         [ [ "project" ~> T.pack projectName ]
                         , [ "top" ~> T.pack topName ]
                         , targetMustache fpga
                         , [ "board" ~> T.pack "digilentinc.com:nexys-a7-50t:part0:1.0" ] -- TODO
                         , [ "srcs" ~> mconcat
                             [ [ object [ "fileName" ~> src1 ] | src1 <- srcs1 ]
                             , [ object [ "fileName" ~> (srcDir </> src1) ] | src1 <- srcs2 ]
                             ]
                           ]
                         , [ "coreSrcs" ~> object
                             [ "nonempty" ~> not (null cores)
                             , "items" ~> [ object [ "fileName" ~> (srcDir </> core) ] | core <- cores ]
                             ]
                           ]
                         , [ "ipcores" ~> [ object [ "name" ~> takeBaseName core ] | core <- cores ] ]
                         , [ "constraintSrcs" ~> [ object [ "fileName" ~> (srcDir </> src1) ] | src1 <- constrs ] ]
                         ]
            writeFileChanged out . T.unpack $ Mustache.substitute template values

        outDir </> "build.tcl" %> \out -> do
            let src = shakeDir </> "xilinx-vivado/project-build.tcl.mustache"
            s <- T.pack <$> readFile' src
            alwaysRerun

            template <- case Mustache.compileTemplate src s of
                Left err -> fail (show err)
                Right template -> return template
            let values = object . mconcat $
                         [ [ "project" ~> T.pack projectName ]
                         , [ "top" ~> T.pack topName ]
                         ]
            writeFileChanged out . T.unpack $ Mustache.substitute template values

        outDir </> "upload.tcl" %> \out -> do
            let src = shakeDir </> "xilinx-vivado/upload.tcl.mustache"
            s <- T.pack <$> readFile' src
            alwaysRerun

            template <- case Mustache.compileTemplate src s of
                Left err -> fail (show err)
                Right template -> return template
            let values = object . mconcat $
                         [ [ "project" ~> T.pack projectName ]
                         , [ "top" ~> T.pack topName ]
                         , targetMustache fpga
                         ]
            writeFileChanged out . T.unpack $ Mustache.substitute template values

        phony (takeBaseName targetDir </> "vivado") $ do
            need [xpr]
            vivado "vivado" [xpr]

        phony (takeBaseName targetDir </> "bitfile") $ do
            need [projectDir </> projectName <.> "runs" </> "impl_1" </> topName <.> "bit"]

        projectDir </> projectName <.> "runs" </> "impl_1" </> topName <.> "bit" %> \_out -> do
            need [xpr]
            vivadoBatch "build.tcl"

        phony (takeBaseName targetDir </> "upload") $ do
            need [projectDir </> projectName <.> "runs" </> "impl_1" </> topName <.> "bit"]
            vivadoBatch "upload.tcl"

clashShake :: ClashProject -> ClashRules () -> IO ()
clashShake proj@ClashProject{..} rules =
  shakeArgs shakeOptions{ shakeFiles = buildDir } $ do
    cfg <- do
        haveConfig <- liftIO $ Dir.doesFileExist "build.mk"
        if haveConfig then do
            usingConfigFile "build.mk"
            liftIO $ readConfigFile "build.mk"
          else do
            usingConfig mempty
            return mempty
    runReaderT rules proj

    phony "clean" $ do
        putNormal $ "Cleaning files in " ++ buildDir
        removeFilesAfter buildDir [ "//*" ]

    want $ case HM.lookup "TARGET" cfg of
        Nothing -> ["clash"]
        Just target -> [target </> "bitfile"]

hexImage :: Maybe Int -> FilePath -> FilePath -> Shake.Action ()
hexImage size src out = do
    bs <- liftIO $ maybe id ensureSize size . BS.unpack <$> BS.readFile src
    let bvs = map (filter (/= '_') . show . pack) bs
    writeFileChanged out (unlines bvs)
  where
    ensureSize size_ bs = take size_ $ bs <> repeat 0
