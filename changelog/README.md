Inspired by [Solving Gitlab's CHANGELOG crisis](https://about.gitlab.com/blog/2018/07/03/solving-gitlabs-changelog-conflict-crisis/)
we now use a single file per change. Of course, if you feel adding an entry to
the CHANGELOG would only pollute it, feel free to not make a file. To keep it
somewhat manageable:

* To add a changelog entry, create a changelog file.

  Either `touch changelog/$(date --iso-8601=seconds | tr : _)_my_change_message` and
  edit the file manually, or run `mk-entry.py`.
* Messages should be valid Markdown, and should start with the type of change
  (`FIXED`,`ADDED`,`CHANGED`,`REMOVED`,`DEPRICATED`).

  For example:
  `CHANGED: Added this example to the README file.`

* All changelog entries should also have the link to a PR/issue. These can be
  found using `blame-pr.py`.
* We collect these files for each release and put their messages in
  `CHANGELOG.md`. `aggregate-entries.py` helps to combine these entries into
  a changelog section, but the output still needs to be checked.

  After adding them to `CHANGELOG.md`, the entry files are deleted.


I've added an example to the commit introducing this change.
