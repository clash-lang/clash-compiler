# Clash's release process

Clash actively deploys to two platforms:

 * [Hackage](http://hackage.haskell.org/package/clash-prelude), a source
   distribution platform for Haskell applications.
 * [The Snap Store](https://snapcraft.io/clash), a binary distribution platform
   for most Linux distributions.

Development happens on the `master` branch. Released versions live in `1.0`,
`1.2`, etc. Changes made to these branches automatically get published every
night. This is achieved through [GitLab Pipeline Schedules](https://gitlab.com/clash-lang/clash-compiler/pipeline_schedules).
(You need admin access to see that page.) At the time of writing it gets
triggered on `master` and `1.2`. For Hackage, a release candidate gets pushed out
with the version numbers in the various cabal files. Candidates can be previewed
on [a special Hackage page](http://hackage.haskell.org/package/clash-prelude/candidates/).
For the Snap Store, an _edge_ release gets made when triggered on `master`. When
triggered on any other branch a _beta_ release gets made.

## Branches overview
* `master`: development branch
  * Only members of the `devops` team can merge to it

* `[0-9].[0-9]`: release branches
  * Only members of the `devops` team can merge to it
  * Warning: matches `1.0`, `1.2`, but NOT `1.22` (GitHub limitation).

## Releasing a new version major version (1.x)
1. Change version numbers in:
  * `clash-prelude/clash-prelude.cabal`
  * `clash-prelude-hedgehog/clash-prelude-hedgehog.cabal`
  * `clash-lib/clash-lib.cabal`
  * `clash-lib-hedgehog/clash-lib-hedgehog.cabal`
  * `clash-ghc/clash-ghc.cabal`
  * `clash-ghc/clash-cores.cabal`
  * `.ci/bindist/linux/snap/snap/snapcraft.yaml`
  * `docs/conf.py`
2. Change the defaults of cabal flags in `clash-prelude/clash-prelude.cabal`
   where necessary. At the time of writing this applies only to
   `-fmultiple-hidden`.
3. Update the CHANGELOG (see changelog/README.md).
4. Create a release branch named after the major version of Clash you're trying to
   release. For example, if you're planning on releasing Clash 1.6.0, create a
   branch called `1.6`.
5. Ask someone with admin permissions on GitLab to create a new nightly schedule
   and trigger a test release build on [GitLab CI](https://gitlab.com/clash-lang/clash-compiler/pipeline_schedules).
   The new schedule should be the same as the old one, but targeting the new
   release branch. Verify that both Snap and Hackage release fine, and preview
   their releases.
     * [Preview on Hackage](http://hackage.haskell.org/package/clash-prelude/candidates/)
     * [Preview on Snap](https://snapcraft.io/clash)
6. Create a release on [GitHub's new release page](https://github.com/clash-lang/clash-compiler/releases/new)
7. Update these docs if anything is missing :-)
8. Enjoy!

## Releasing a new version minor version (1.x.x)
1. Change version numbers in:
  * `clash-prelude/clash-prelude.cabal`
  * `clash-lib/clash-lib.cabal`
  * `clash-ghc/clash-ghc.cabal`
  * `clash-ghc/clash-cores.cabal`
  * `.ci/bindist/linux/snap/snap/snapcraft.yaml`
  * `docs/conf.py`
2. Update the CHANGELOG (see changelog/README.md).
3. Ask someone with admin permissions on GitLab to trigger a nightly scheduele,
   or simply wait a day. Verify that both Snap and Hackage release fine, and
   preview heir releases.
     * [Preview on Hackage](http://hackage.haskell.org/package/clash-prelude/candidates/)
     * [Preview on Snap](https://snapcraft.io/clash)

4. Create a release on [GitHub's new release page](https://github.com/clash-lang/clash-compiler/releases/new)
5. Cherry-pick commit made in (2) to `master`
6. Update these docs if anything is missing :-)
7. Enjoy!
