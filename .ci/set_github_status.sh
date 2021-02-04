#!/bin/sh
commit_status=${1}
username="clash-lang"
repo="clash-compiler"

curl --url "https://api.github.com/repos/${username}/${repo}/statuses/${CI_COMMIT_SHA}" \
     --header 'Content-Type: application/json' \
     --header "authorization: Bearer ${GITHUB_STATUS_TOKEN}" \
     --data "{ \"state\": \"${commit_status}\", \"target_url\": \"${CI_PIPELINE_URL}\", \"description\": \"All Gitlab pipelines\", \"context\": \"ci/gitlab/gitlab.com\" }"
