#!/usr/bin/env python3
"""
Print a list of GitHub CLI commands that posts a comment under each fixed issue,
indicating that the fix for an issue is now part of a released version.

Usage:

   ./comment-gh.py 1.6.3

"""
import re
import sys

ISSUE_RE = re.compile("https://github.com/clash-lang/clash-compiler/issues/[0-9]+")

def get_issues(search_for_version):
  changelog = open("../CHANGELOG.md").read()
  sections = changelog.split("##")

  for section in sections[1:]:
    (version_line, *lines) = section.split("\n")
    (version_str, _date_str) = version_line.strip().split(maxsplit=1)

    if version_str == search_for_version:
      for line in lines:
        match = ISSUE_RE.search(line)
        if match is not None:
          yield match.group(0)

if __name__ == '__main__':
  version = sys.argv[1]
  for issue_url in set(get_issues(version)):
    print(f"gh issue comment {issue_url} -b \"We've released [v{version}](https://github.com/clash-lang/clash-compiler/releases/tag/v{version}), which includes a fix for this issue.\"")
