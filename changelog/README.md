Inspired by [Solving Gitlab's CHANGELOG crisis](https://about.gitlab.com/blog/2018/07/03/solving-gitlabs-changelog-conflict-crisis/)
we now use a single file per change. Of course, if you feel adding an entry to
the CHANGELOG would only pollute it, feel free to not make a file. To keep it
somewhat manageable:

* Create a changelog file: `touch $(date --iso-8601=seconds | tr : _)_my_change_message`
* We collect these files for each release and put their messages in
  `CHANGELOG.md`. The files are subsequently deleted.
* Messages should be valid Markdown

I've added an example to the commit introducing this change.
