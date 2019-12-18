{-# LANGUAGE CPP #-}

module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest (docTestOpts ++ ["-isrc","src/Clash/Prelude.hs"
                               ,"src/Clash/Tutorial.hs"
                               ,"src/Clash/Examples.hs"])

docTestOpts :: [String]
docTestOpts =
#if __GLASGOW_HASKELL__ >= 806
  "-XNoStarIsType" :
#endif
#if __GLASGOW_HASKELL__ <= 804
  "-XTypeInType" :
#endif
  -- TODO: Figure out a way to auto-sync this with clash-prelude.cabal
  [ "-XBangPatterns"
  , "-XBinaryLiterals"
  , "-XDataKinds"
  , "-XDefaultSignatures"
  , "-XDeriveDataTypeable"
  , "-XDeriveFoldable"
  , "-XDeriveFunctor"
  , "-XDeriveGeneric"
  , "-XDeriveLift"
  , "-XDeriveTraversable"
  , "-XDerivingStrategies"
  , "-XInstanceSigs"
  , "-XKindSignatures"
  , "-XMagicHash"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XTupleSections"
  , "-XTypeApplications"
  , "-XTypeOperators"
  , "-XViewPatterns"
  ]
