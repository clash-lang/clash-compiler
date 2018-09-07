You can use this to profile just the normalization process.

Add the following lines to your cabal.project.local:
```
packages: ./benchmark/profiling/*/*.cabal

package clash-profiling
  profiling: True
```

And run using:
```
$ cabal new-run clash-profile-normalization-prepare -- [INPUT_FILES]
$ cabal new-run clash-profile-normalization-run -- [INPUT_FILES] +RTS -p
```
