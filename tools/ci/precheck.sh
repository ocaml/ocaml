#!/bin/bash

set -x

PRECHECK_URL="https://ci.inria.fr/ocaml/job/precheck"

# A filename used to pipe data between commands
OUTPUT="out.txt"

# Bail out if comment is not trigger

if test "${COMMENT_BODY}" != "/precheck"; then exit 0; fi

# Bail out if not in a PR

if test -z "${PULL_REQUEST_URL}"; then exit 0; fi

# Check that the user belongs to caml-devel

## Retrieve team URL

curl -s \
     -H "Authorization: Bearer ${CAML_DEVEL_TOKEN}" \
     -H "Content-type: application/json" \
     https://api.github.com/orgs/ocaml/teams/caml-devel | jq -r '.url' | tee ${OUTPUT}

read CAML_DEVEL_URL < ${OUTPUT}

echo "CAML_DEVEL_URL=${CAML_DEVEL_URL}"
echo "COMMENT_USER_LOGIN=${COMMENT_USER_LOGIN}"

## Retrieve the list of members of caml-devel

curl -s \
     -H "Authorization: Bearer ${CAML_DEVEL_TOKEN}" \
     -H "Content-type: application/json" \
     ${CAML_DEVEL_URL}/members | jq -r '.[].login' > ${OUTPUT}

## Check whether comment author belongs to caml-devel

if ! grep -Fxq "${COMMENT_USER_LOGIN}" ${OUTPUT}
then
    MESSAGE="Sorry, only members of the developer team may trigger precheck."
    echo "$MESSAGE" | jq -R -s '{body: .}' | \
        curl -s -H "Authorization: Bearer ${GITHUB_TOKEN}" \
             -H "Content-type: application/json" \
             ${ISSUE_URL}/comments -d @-
    exit 0
fi

# Retrieve PR repo and branch

curl -s ${PULL_REQUEST_URL} | jq -r '"\(.head.ref) \(.head.repo.html_url)"' | tee ${OUTPUT}

read BRANCH REPO_URL < ${OUTPUT}

echo "BRANCH=${BRANCH}"
echo "REPO_URL=${REPO_URL}"

# Trigger Jenkins build

echo "Triggering Jenkins build"

curl -s -i -u "${JENKINS_USER}:${JENKINS_TOKEN}" \
     -F "REPO_URL=${REPO_URL}" -F "BRANCH=${BRANCH}" \
     ${PRECHECK_URL}/buildWithParameters | tee ${OUTPUT}

# Get Jenkins queue URL

read JENKINS_QUEUE_URL < <(grep -i '^Location:' ${OUTPUT} | cut -d' ' -f 2 | tr -d '[:space:]')

echo "JENKINS_QUEUE_URL=${JENKINS_QUEUE_URL}"

# Get Jenkins job URL by polling queue URL

echo "Getting Jenkins job URL"

JENKINS_JOB_URL=

for i in {1..10}
do
    echo "Polling for 10s (${i})..."

    sleep 10

    curl -s -u "${JENKINS_USER}:${JENKINS_TOKEN}" \
         ${JENKINS_QUEUE_URL}api/json | jq -r '.executable.url' | tee ${OUTPUT}

    read JENKINS_JOB_URL < ${OUTPUT}

    if test -n "${JENKINS_JOB_URL}"
    then
        break
    fi
done

echo "JENKINS_JOB_URL=${JENKINS_JOB_URL}"

# Post message saying that the build has started

if test -z "${JENKINS_JOB_URL}"
then
    MESSAGE="Build requested, but could not retrieve Jenkins job URL :("
else
    MESSAGE="Precheck started: ${JENKINS_JOB_URL}"
fi

echo "$MESSAGE" | jq -R -s '{body: .}' | \
    curl -s -H "Authorization: Bearer ${GITHUB_TOKEN}" \
         -H "Content-type: application/json" \
         ${ISSUE_URL}/comments -d @-
