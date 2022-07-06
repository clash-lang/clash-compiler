$GHC_VERSION=(ghc --numeric-version)
$GHC_MAJOR_VERSION=Write-Output $GHC_VERSION | wsl tr -d '.' | wsl head -c2

if ($GHC_MAJOR_VERSION -ne 84) {
  $XNoStarIsType="-XNoStarIsType"
}

ghci `
  -fobject-code `
  -iclash-ghc/src-bin-common/ `
  -iclash-lib/src `
  -iclash-ghc/src-ghc `
  -Wall -Wcompat -DTOOL_VERSION_ghc="`"`"`"$GHC_VERSION`"`"`"" `
  -XBangPatterns -XBinaryLiterals -XDataKinds -XDefaultSignatures `
  -XDeriveDataTypeable -XDeriveFoldable -XDeriveFunctor -XDeriveGeneric `
  -XDeriveLift -XDeriveTraversable -XDerivingStrategies -XInstanceSigs `
  -XKindSignatures $XNoStarIsType -XScopedTypeVariables -XStandaloneDeriving `
  -XTupleSections -XTypeApplications -XTypeOperators -XViewPatterns `
  -DDEBUG `
  Clash.hs
