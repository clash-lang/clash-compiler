import Distribution.Simple ( Args
                           , postClean
                           , preBuild
                           , simpleUserHooks
                           , defaultMainWithHooks
                           , UserHooks
                           )

import Distribution.Simple.Setup ( BuildFlags
                                 , CleanFlags
                                 )

import Distribution.PackageDescription ( PackageDescription
                                       , HookedBuildInfo
                                       )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo )

import System.Process (callProcess)


main = defaultMainWithHooks
          simpleUserHooks { preBuild = cosimBuild
                          , postClean = cosimClean
                          }


-- | Runs Makefile
cosimBuild
    :: Args
    -> BuildFlags
    -> IO HookedBuildInfo
cosimBuild args flags = do
    putStrLn "Compiling clash-cosim.cbits.."
    callProcess "/usr/bin/make" ["-C", "src/cbits", "-s"]
    preBuild simpleUserHooks args flags

-- | Cleans binaries made by cosimPostBuild
cosimClean
    :: Args
    -> CleanFlags
    -> PackageDescription
    -> ()
    -> IO ()
cosimClean args flags pkgDescription stub = do
    callProcess "make" ["-C", "src/cbits", "clean", "-s"]
    postClean simpleUserHooks args flags pkgDescription stub
