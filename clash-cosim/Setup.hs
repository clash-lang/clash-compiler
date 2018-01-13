{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Exception (IOException, SomeException, try)
import Control.Monad     (unless)
import Data.Char         (isSpace)
import Data.List         (isPrefixOf, isInfixOf, dropWhileEnd, intercalate)
import Data.Maybe        (fromJust)
import NeatInterpolation (text)
import System.IO         (stderr, hPutStrLn)
import System.Process    (callProcess, readProcessWithExitCode)
import Text.Printf       (printf)

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import Distribution.Simple ( Args
                           , postClean
                           , postBuild
                           , simpleUserHooks
                           , defaultMainWithHooks
                           , UserHooks
                           )

import Distribution.Simple.Setup          (BuildFlags)
import Distribution.Simple.Setup          (CleanFlags)
import Distribution.PackageDescription    (PackageDescription, ccOptions)
import Distribution.PackageDescription    (HookedBuildInfo, setupBuildInfo)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, configFlags)
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription


main = defaultMainWithHooks
          simpleUserHooks { postBuild = cosimBuild
                          , postClean = cosimClean
                          }


supportsMinusN :: String -> Bool
supportsMinusN s = any (isPrefixOf "-N ") (map trim $ lines s)
    where
        trim :: String -> String
        trim = dropWhileEnd isSpace . dropWhile isSpace

tryIO :: IO a ->  IO (Either IOException a)
tryIO = try

trySome :: IO a -> IO (Either SomeException a)
trySome = try


-- | Runs Makefile
cosimBuild
    :: Args
    -> BuildFlags
    -> PackageDescription
    -> LocalBuildInfo
    -> IO ()
cosimBuild args flags pkgDescription localBuildInfo = do
    postBuild simpleUserHooks args flags pkgDescription localBuildInfo

    -- Check if correct vvp version is installed
    vvpHelp <- tryIO $ readProcessWithExitCode "vvp" ["-h"] []
    let vvpHelp' = case vvpHelp of
          Left (Text.pack . show -> err) ->
              error $ Text.unpack $ [text|
                  Failed to execute vvp. System reported:

                      ${err}

                  Vvp is part of the iverilog package. On Ubuntu systems, run:

                      sudo apt install iverilog
                  |]
          Right (_exitCode, _stdout, stderr) -> stderr

    -- HACK: Fetch Ubuntu 18.04 version of iverilog. Although this is not
    -- guaranteed to install the latest patched version, it is a pretty safe
    -- bet as this package has been stable for almost two years now. We use
    -- a Danish mirror (one.com) as the *.archive.ubuntu.com do not support
    -- HTTPS (this is usually no problem due to APT handling auth logic).
    unless (supportsMinusN vvpHelp') $ error $ Text.unpack [text|
        Installed version of vvp does not support the flag '-N'. Upgrade
        your version to >= 11.0. On Ubuntu <= 16.04, run:

            cd /tmp
            wget -q https://mirror.one.com/ubuntu/pool/main/r/readline/libreadline7_7.0-0ubuntu2_amd64.deb
            wget -q https://mirror.one.com/ubuntu/pool/universe/i/iverilog/iverilog_10.1-0.1build1_amd64.deb
            sha256sum libreadline7_7.0-0ubuntu2_amd64.deb iverilog_10.1-0.1build1_amd64.deb
            sudo dpkg -i libreadline7_7.0-0ubuntu2_amd64.deb iverilog_10.1-0.1build1_amd64.deb
            rm libreadline7_7.0-0ubuntu2_amd64.deb iverilog_10.1-0.1build1_amd64.deb
            cd -

        Make sure the checksums correspond with:

            647f958429e17496bc96f188befd8229d30b2c1719255a5e8d15b5cd7be8593b  libreadline7_7.0-0ubuntu2_amd64.deb
            5aab60f8f7cbae29205c47684c5fce41a60e6d8e1b8fea31013747407e95bf0b  iverilog_10.1-0.1build1_amd64.deb
        |]

    -- Pass compile options mentioned in clash-cosim.cabal to 'make'
    let ccOpts = ccOptions
                   $ libBuildInfo
                   $ fromJust
                   $ library
                   $ localPkgDescr localBuildInfo

    let ldOpts = ldOptions
                   $ libBuildInfo
                   $ fromJust
                   $ library
                   $ localPkgDescr localBuildInfo


    makeResult <- trySome $ callProcess
                                "/usr/bin/make"
                                [ "-C"
                                , "src/cbits"
                                , "-s"
                                , printf "CFLAGS=%s" (intercalate " " ccOpts)
                                , printf "LFLAGS=%s" (intercalate " " ldOpts)
                                ]
    case makeResult of
        Left (Text.pack . show -> err) ->
            error $ Text.unpack $ [text|
                Running 'make' on 'src/cbits/Makefile' failed. System reported:

                    ${err}

                This might occur when (a component of) gcc is missing. On Ubuntu systems, run:

                    sudo apt install build-essential g++-multilib
                |]
        Right _ ->
            return ()

-- | Cleans binaries made by cosimPostBuild
cosimClean
    :: Args
    -> CleanFlags
    -> PackageDescription
    -> ()
    -> IO ()
cosimClean args flags pkgDescription stub = do
    postClean simpleUserHooks args flags pkgDescription stub
    callProcess "make" ["-C", "src/cbits", "clean", "-s"]
