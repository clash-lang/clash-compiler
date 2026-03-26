# Clash's release process

Clash actively deploys to one platform: [Hackage](http://hackage.haskell.org/package/clash-prelude),
a source distribution platform for Haskell applications.

Development happens on the `master` branch. Released versions live in `1.0`,
`1.2`, etc. Changes made to these branches automatically get published every
night. This is achieved through [GitLab Pipeline Schedules](https://gitlab.com/clash-lang/clash-compiler/pipeline_schedules).
(You need admin access to see that page.) At the time of writing it gets
triggered on `master` and `1.8`. A release candidate gets pushed out
with the version numbers in the various cabal files. Candidates can be previewed
on [a special Hackage page](http://hackage.haskell.org/package/clash-prelude/candidates/).

## Branches overview
* `master`: development branch
  * Only members of the `devops` team can merge to it

* `[0-9].[0-9]`: release branches
  * Only members of the `devops` team can merge to it
  * Warning: matches `1.0`, `1.2`, but NOT `1.22` (GitHub limitation).

## Releasing a new major version (1.x)
1. Change version numbers in:
  * `clash-prelude/clash-prelude.cabal`
  * `clash-prelude-hedgehog/clash-prelude-hedgehog.cabal`
  * `clash-lib/clash-lib.cabal`
  * `clash-lib-hedgehog/clash-lib-hedgehog.cabal`
  * `clash-ghc/clash-ghc.cabal`
  * `docs/conf.py`
2. Update the CHANGELOG (see `changelog/README.md`). Each entry should normally
   end with a link to a PR or issue. The script `changelog/blame-pr.py` will
   tell you for a single file which PR('s) introduced it.
3. Add a new `pull_request_rule` for backports to `.mergify.yml` for the branch
   you will create.
4. Create a release branch named after the major version of Clash you're trying to
   release. For example, if you're planning on releasing Clash 1.6.0, create a
   branch called `1.6`.
5. Repeat step (1) in the new release branch.
6. Ask someone with admin permissions on GitLab to create a new nightly schedule
   and trigger a test release build on [GitLab CI](https://gitlab.com/clash-lang/clash-compiler/pipeline_schedules).
   The new schedule should be the same as the old one, but targeting the new
   release branch. Verify that the Hackage release deploys well, and preview
   the release. [Preview on Hackage](http://hackage.haskell.org/package/clash-prelude/candidates/)
7. Write release notes for: Twitter, LinkedIn, and clash-lang.org.
8. Create a release on [GitHub's new release page](https://github.com/clash-lang/clash-compiler/releases/new)
9. After the release is on Hackage: run `changelog/comment-gh.py` and execute
   the commands it lists. This will inform users subscribed to specific issues
   that a fix for their issue is now in a released version.
10. Update the [starter projects](https://github.com/clash-lang/stack-templates/)
11. Update these docs if anything is missing :-)
12. Enjoy!

## Releasing a new minor version (1.x.x)
1. Create a release branch (i.e. `v1.8.5_release`) from
   the branch of the version you are releasing.
2. Change version numbers in:
  * `clash-prelude/clash-prelude.cabal`
  * `clash-prelude-hedgehog/clash-prelude-hedgehog.cabal`
  * `clash-lib/clash-lib.cabal`
  * `clash-lib-hedgehog/clash-lib-hedgehog.cabal`
  * `clash-ghc/clash-ghc.cabal`
  * `docs/conf.py`

   This both includes the version number at the top of many files, and the
   listed Clash dependecies.

   The script `changelog/update-version.py` can do this for you (execute inside
   `changelog/`) but also search for any occurances of the old version.
3. In a **single separate commit**, update the CHANGELOG (see `changelog/README.md`).
   `aggregate-entries.py` can help to generate this text, though it still needs
   to be checked afterwards. Each entry should normally end with a link to a PR
   or issue. The script `changelog/blame-pr.py` will tell you for a single file
   which PR('s) introduced it.
4. Merge the release branch into the version branch.
5. Ask someone with admin permissions on GitLab to trigger a nightly schedule,
   or simply wait a day. Verify that the Hackage release deploys well, and
   preview the release. [Preview on Hackage](http://hackage.haskell.org/package/clash-prelude/candidates/)
   (be aware that Hackage may update slowly and partially, and as such documentation
   changes made to a version that already had documentation may not
   show up properly)
6. Create a release on [GitHub's new release page](https://github.com/clash-lang/clash-compiler/releases/new)
7. Update the [starter projects](https://github.com/clash-lang/stack-templates/)
   This requires obtaining the hashes and lengths of some `.cabal` files.
   Use `echo "sha256:$(sha256sum *cabal | cut -f1 -d' '),$(wc -c *.cabal | cut -f1 -d' ')"`
   in the containing folder (i.e. in `clash-prelude` etc.).
8. Cherry-pick commit made in (3) (changelog updates) to `master`
9. After the release is on Hackage: run `changelog/comment-gh.py` and execute
   the commands it lists. This will inform users subscribed to specific issues
   that a fix for their issue is now in a released version.
10. Update these docs if anything is missing :-)
11. Enjoy!
