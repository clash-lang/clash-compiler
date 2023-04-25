module Main where

import Control.Monad
import Data.Maybe
import Distribution.PackageDescription.Utils
import Distribution.Simple
import Distribution.Simple.Build
import Distribution.Simple.BuildPaths
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibType
import Distribution.Types.GenericPackageDescription
import Distribution.Types.HookedBuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.PackageDescription
import Distribution.Types.UnqualComponentName
import Distribution.Verbosity
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks simpleUserHooks
    { postBuild = ffiPostBuild }

ffiPostBuild
  :: Args
  -> BuildFlags
  -> PackageDescription
  -> LocalBuildInfo
  -> IO ()
ffiPostBuild args flags desc info = do
  -- Create lib/ in the project directory
  let outPath = takeDirectory (fromJust $ pkgDescrFile info) </> "lib"
  createDirectoryIfMissing True outPath

  -- Copy each foreign library to lib/
  forM_ (foreignLibs desc) $ \flib ->
    let name = unUnqualComponentName (foreignLibName flib)
        dLib = buildDir info </> name </> flibTargetName info flib
     in copySoAsVpl outPath dLib

  -- Do the normal post-build hook action
  postBuild simpleUserHooks args flags desc info

-- | Get the name of the library that will be written to disk when building
-- the library. Lifted from `Distribution.Simple.GHC`.
--
flibTargetName :: LocalBuildInfo -> ForeignLib -> String
flibTargetName lbi flib =
    case (os, foreignLibType flib) of
      (Windows, ForeignLibNativeShared) -> nm <.> "dll"
      (Windows, ForeignLibNativeStatic) -> nm <.> "lib"
      (Linux,   ForeignLibNativeShared) -> "lib" ++ nm <.> versionedExt
      (_other,  ForeignLibNativeShared) ->
        "lib" ++ nm <.> dllExtension (hostPlatform lbi)
      (_other,  ForeignLibNativeStatic) ->
        "lib" ++ nm <.> staticLibExtension (hostPlatform lbi)
      (_any,    ForeignLibTypeUnknown)  -> cabalBug "unknown foreign lib type"
  where
    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

    os :: OS
    os = let (Platform _ os') = hostPlatform lbi
         in os'

    -- If a foreign lib foo has lib-version-info 5:1:2 or
    -- lib-version-linux 3.2.1, it should be built as libfoo.so.3.2.1
    -- Libtool's version-info data is translated into library versions in a
    -- nontrivial way: so refer to libtool documentation.
    versionedExt :: String
    versionedExt =
      let nums = foreignLibVersion flib os
      in foldl (<.>) "so" (map show nums)

-- | Copy a file to the same directory, but change the extension to .vpl. This
-- is needed for iverilog, as it will not load VPI modules which do not have
-- either a .vpi or .vpl extension, unlike other simulators which will load
-- the .so file that cabal normally produces.
--
copySoAsVpl :: FilePath -> FilePath -> IO ()
copySoAsVpl outDir so =
  -- We use installMaybeExecutable file because it preserves the permissions
  -- of the original file. On my machine, just using installExecutableFile
  -- meant the permissions were *slightly* different.
  let outPath = replaceDirectory (replaceExtensions so "vpl") outDir
   in installMaybeExecutableFile verbose so outPath

