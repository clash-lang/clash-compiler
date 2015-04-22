Download sources:
```
git clone git@github.com:clash-lang/clash-prelude.git
git clone git@github.com:christiaanb/ghc-typelits-natnormalise.git
```

Go to clash-prelude dir:
```
cd clash-prelude
```

Run:
```
cabal sandbox init
cabal sandbox add-source ../ghc-typelits-natnormalise
cabal install --dependencies-only --enable-tests
```

Configure the package with testing enabled:
```
cabal configure --enable-tests
```

Once you've finished hacking, build and test:
```
cabal build
cabal test
```
