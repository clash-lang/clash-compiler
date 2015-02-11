Download sources:
```
git clone git@github.com:christiaanb/clash2.git
git clone git@github.com:christiaanb/clash-prelude.git
```

Go to clash2 dir:
```
cd clash2
```

Run:
```
cabal sandbox init
cabal sandbox add-source clash-lib
cabal sandbox add-soruce clash-vhdl
cabal sandbox add-source clash-verilog
cabal sandbox add-source clash-ghc
cabal sandbox add-source ../clash-prelude
```
