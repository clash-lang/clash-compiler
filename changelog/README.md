Inspired by [Solving Gitlab's CHANGELOG crisis](https://about.gitlab.com/blog/2018/07/03/solving-gitlabs-changelog-conflict-crisis/)
we now use a single file per change. Of course, if you feel adding an entry to
the CHANGELOG would only pollute it, feel free to not make a file. To keep it
somewhat manageable:

* Create a changelog file: `touch $(date --iso-8601=seconds | tr : _)_my_change_message`
* We collect these files for each release and put their messages in
  `CHANGELOG.md`. The files are subsequently deleted.

A changelog file can contain multiple entries. Each entry starts with a tag
word from the list below, followed by one or more lines of text in Markdown
format. An entry should mention the GitHub PR that introduces the change or the
GitHub issue that the change fixes.

Changelog entries should be understandable as a stand-alone text; it should give
the reader an overview of the change without needing to consult more
documentation, but it can refer to other documentation for further information.
A Changelog entry is allowed to be verbose, but it need not duplicate
documentation; a link to further documentation is often enough.

Example:

```
ADDED: Commodo eros. Suspendisse tincidunt mi vel metus. [#1234](https://github.com/clash-lang/clash-compiler/pull/1234)
FIXED: Facilisis neque. Nulla mattis odio vitae tortor. [#1234](https://github.com/clash-lang/clash-compiler/issues/1234)
```

The tags we use are:
 * ADDED:
 * FIXED:
 * CHANGED:
 * DEPRECATED:
 * REMOVED:
 * INTERNAL NEW:
 * INTERNAL FIX:
 * INTERNAL CHANGE:
 * DOCUMENTATION:
   (meaning something that only affects documentation, not code)

Usually, the name of the section in the CHANGELOG is equal to the tag;
the exceptions are:
 * INTERNAL NEW: Listed in a CHANGELOG section named _New internal features:_
 * INTERNAL FIX: Listed in a CHANGELOG section named _Internal fixes:_
 * INTERNAL CHANGE: Listed in a CHANGELOG section named _Internal changes:_
