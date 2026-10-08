#!/usr/bin/env python3
"""
Keeps one comment on a pull request with links to the benchmark site: one to
the branch, one to the newest commit, and a list of the earlier pushes. The
comment is edited on every push instead of adding a new one.
"""

import datetime
import json
import os
import re
import urllib.parse
import urllib.request

SITE = "https://clash-lang.github.io/clash-benchmarks/"
MARKER = "<!-- clash-benchmark-links "
MAX_PUSHES = 100

def api(method, path, body=None):
  """Calls the GitHub API. Returns the decoded JSON and the next page, if any."""
  url = path if path.startswith("https://") else os.environ["GITHUB_API_URL"] + path
  req = urllib.request.Request(
    url,
    method=method,
    data=None if body is None else json.dumps(body).encode(),
    headers={
      "Accept": "application/vnd.github+json",
      "Authorization": f"Bearer {os.environ['GITHUB_TOKEN']}",
      "X-GitHub-Api-Version": "2022-11-28",
    },
  )
  with urllib.request.urlopen(req) as resp:
    next_page = re.search(r'<([^>]+)>;\s*rel="next"', resp.headers.get("Link") or "")
    return json.load(resp), next_page and next_page.group(1)

def list_comments(repo, number):
  path = f"/repos/{repo}/issues/{number}/comments?per_page=100"
  while path:
    page, path = api("GET", path)
    yield from page

def link(name, value):
  # The site accepts "owner/repo@branch" as a branch key. It reads better
  # with "/" and "@" left as they are.
  return f"{SITE}?{name}={urllib.parse.quote(value, safe='/@')}"

def commit_link(sha):
  return f"[`{sha[:7]}`]({link('commit', sha)})"

def read_pushes(body):
  """The pushes so far, newest first, as kept in the comment itself."""
  try:
    return json.loads(body[len(MARKER):].split(" -->")[0])["pushes"]
  except (ValueError, KeyError) as e:
    print(f"::error::Could not read the state of the comment: {e}")
    raise

def add_pushes(pushes, before, head, now):
  # A push whose run was replaced in the concurrency queue is the "before"
  # commit of the next push.
  if before and before.strip("0") and all(p["sha"] != before for p in pushes):
    pushes = [{"sha": before, "seen": None}] + pushes
  known = next((p for p in pushes if p["sha"] == head), {"sha": head, "seen": now})
  return ([known] + [p for p in pushes if p["sha"] != head])[:MAX_PUSHES]

def render(pushes, branch_key):
  def seen(push):
    return f" ({push['seen'][:16].replace('T', ' ')} UTC)" if push["seen"] else ""

  previous = pushes[1:]
  lines = [
    f"{MARKER}{json.dumps({'pushes': pushes})} -->",
    "### Benchmarks",
    "",
    "This pull request has the `performance` label, so the benchmark runner "
    "measures its commits. Results show up on the site once a commit has been "
    "measured.",
    "",
    f"- **Branch:** [`{branch_key}`]({link('branch', branch_key)}), "
    "which follows the branch as it advances",
    f"- **Latest push:** {commit_link(pushes[0]['sha'])}",
  ]
  if previous:
    lines += [
      "",
      f"<details><summary>Previous pushes ({len(previous)})</summary>",
      "",
      *(f"- {commit_link(p['sha'])}{seen(p)}" for p in previous),
      "",
      "</details>",
    ]
  return "\n".join(lines)

def main():
  with open(os.environ["GITHUB_EVENT_PATH"]) as fp:
    event = json.load(fp)
  repo = os.environ["GITHUB_REPOSITORY"]
  pr = event["pull_request"]
  head_repo = pr["head"]["repo"]
  branch_key = (
    f"{head_repo['full_name']}@{pr['head']['ref']}" if head_repo else pr["head"]["ref"]
  )

  existing = next(
    (c for c in list_comments(repo, pr["number"])
     if c["user"]["type"] == "Bot" and (c["body"] or "").startswith(MARKER)),
    None,
  )
  pushes = read_pushes(existing["body"]) if existing else []
  now = datetime.datetime.now(datetime.timezone.utc).isoformat(timespec="seconds")
  pushes = add_pushes(pushes, event.get("before"), pr["head"]["sha"], now)
  body = render(pushes, branch_key)

  if existing is None:
    api("POST", f"/repos/{repo}/issues/{pr['number']}/comments", {"body": body})
  elif existing["body"] != body:
    api("PATCH", f"/repos/{repo}/issues/comments/{existing['id']}", {"body": body})


if __name__ == '__main__':
  main()
