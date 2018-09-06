#Hacking

Download sources:
```
git clone https://github.com/clash-lang/clash-prelude.git
git clone https://github.com/clash-lang/ghc-tcplugins-extra.git
git clone https://github.com/clash-lang/ghc-typelits-natnormalise.git
```

Go to clash-prelude dir:
```
cd clash-prelude
```

Run:
```
cabal sandbox init
cabal sandbox add-source ../ghc-tcplugins-extra
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

# Supplying patches
Patches are only accepted through GitHub pull requests.

1. Following the [Hacking](#hacking) section above.
2. Go to the [clash-prelude](https://github.com/clash-lang/clash-prelude/) GitHub repository page, and click on _Fork_.
   <img src=https://help.github.com/assets/images/help/repository/fork_button.jpg height=50 />
   
   This will create a new _remote_ `git` repository.
3. Add your forked repository as a new remote:

   ```
   $ git remote add <GITHUB_USERNAME> git@github.com:<GITHUB_USERNAME>/clash-prelude.git
   ```
4. Create a new branch in which you will be working on your patch:

   ```
   $ git checkout -b <BRANCH_NAME>
   ```
5. Start hacking.
6. Commit changes sensibly!

   ```
   $ git add -p
   ... add small changes ...
   $ git commit
   ... sensible commit message for small change ...
   $ git add -p
   ... small changes ...
   $ git commit
   
   etc.
   ```
7. Push to _your_ remote GitHub repository:
   
   ```
   $ git push -u <GITHUB_USERNAME> <BRANCH_NAME>
   ```
9. Hack some more
10. Commit and push some more:

   ```
   $ git add -p
   $ git commit
   $ git add -p
   $ git commit
   $ git push
   ```
11. Sync with central repository:
   
   ```
   $ git pull --rebase origin master
   ```
   
   Fix any conflicts that might arise.

   Then push to _your_ remote repository.

   ```
   $ git push
   ```

   If the above complains do:

   ```
   $ git push --force
   ```
12. Create a pull request:
   
   * Go to `https://github.com/<GITHUB_USERNAME>/clash-prelude`
   * Select the \<BRANCH_NAME\> <img src=https://help.github.com/assets/images/help/branch/pick-your-branch.png height=150 />
   * Click the __Compare & review__ button <img src=https://help.github.com/assets/images/help/pull_requests/pull-request-start-review-button.png height=100 />
   * Click the __Create pull request__ button.

     <img src=https://help.github.com/assets/images/help/pull_requests/pull-request-review-page.png width=600 />
13. Updating the pull request:

   * Once you've created a pull request, any changes to your branch will automatically be added to the pull request.
   * To push your updated changes to your branch, always do:
   
   ```
   $ git pull --rebase origin master
   ... fix any conflicts ...
   $ git push --force
   ```
