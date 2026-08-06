The changelog is managed using [qlog](https://github.com/marijn-qbaylogic/qlog/).
It is part of the Nix development shell, so `nix develop` provides it without
any installation. See the `qlog` README for how to install it otherwise.

Changelog entries are stored in files, and then aggregated into `CHANGELOG.md`.
Simple entries can be created using `qlog entry`, but more is possible by editing
the files manually. See the `qlog` documentation for more information.

Keep in mind that the entries are written in Markdown.
For example, when including Haskell code, use
````
```hs
<code>
```
````

Issue links are added to the end of entries automatically.
