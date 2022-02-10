#!/usr/bin/env python3
import lxml.html
import subprocess
import requests
import functools

ORG = "clash-lang"
REPO = "clash-compiler"

def commit_url(digest):
    return f"https://github.com/{ORG}/{REPO}/branch_commits/{digest}"

def pr_url(pr):
    return f"https://github.com/{ORG}/{REPO}/pull/{pr}"

def blame_cmd(path):
    return ["git", "blame", "-l", path]

def blame(path):
    result = subprocess.run(blame_cmd(path), check=True, capture_output=True)

    content_lines = open(path).readlines()
    blame_lines = result.stdout.splitlines()

    for content_line, blame_line in zip(content_lines, blame_lines):
        digest = blame_line.split()[0].decode()
        yield (digest, content_line)

@functools.lru_cache(maxsize=None)
def get_pr(digest):
    html = requests.get(commit_url(digest)).content
    doc = lxml.html.fromstring(html)
    prs = doc.cssselect(".pull-request > a")
    prs = tuple(int(pr.text.strip()[1:]) for pr in prs)
    if len(prs) == 1:
        return prs[0]
    elif len(prs) > 1:
        raise ValueError(f"Multiple PRs for {digest}")
    else:
        raise ValueError(f"No PR for {digest}")

if __name__ == '__main__':
    import sys

    prs = set()
    for digest, line in blame(sys.argv[1]):
        pr = get_pr(digest)
        prs.add(pr)
        print(f"{pr}\t{line}")

    for pr in prs:
        print(f"[#{pr}]({pr_url(pr)})")
