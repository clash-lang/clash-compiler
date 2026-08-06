---
issues: [3213]
prs: [3301]
---

# CHANGED
`flake.nix` now advertises the [clash-lang Cachix binary cache](https://clash-lang.cachix.org) via `nixConfig.extra-substituters`. Running `nix develop` will prompt you to trust the cache, avoiding having to build Clash from source.
