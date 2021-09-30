#!/usr/bin/env bash
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 David Allsopp, OCaml Labs, Cambridge.                  *
#*                                                                        *
#*   Copyright 2021 David Allsopp Ltd.                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# The aim of this script is to ensure that all the commits for a PR or branch
# push are fetched. Particularly for long-lived PRs, the relevant commits for
# the merge-base (i.e. the commit on trunk) will not be present by default.
# For force pushes, the same can be true for branches (e.g. a rebase)
# After running this script, 5 variables are available:
#   - FETCH_HEAD - the merge commit for a PR or the tip of the branch of a push
#   - UPSTREAM_BRANCH - the branch a PR is against or the full ref of the push
#   - UPSTREAM_SHA - the tip of UPSTREAM_BRANCH (prior to push, if applicable)
#   - PR_BRANCH - the PR's branch name; equal to $UPSTREAM_BRANCH for a push
#   - PR_HEAD - the tip of PR_BRANCH (so, for a push, the new tip after pushing)

# GitHub Actions doesn't support the ternary operator, so the dance is done here
# Each script has:
#   $1 - ref to fetch when deepening
#   $2 - event type ('pull_request' or 'push')
#   $3 - upstream branch name
#   $4 - upstream branch SHA
#   $5 - PR branch name
#   $6 - PR SHA
#   $7 - full ref being pushed
#   $8 - upstream SHA prior to push
#   $9 - repeats $7
#  $10 - upstream SHA after the push
FETCH_REF="${1}"
if [[ $2 = 'pull_request' ]]; then
  shift 2
else
  shift 6
fi

# Record FETCH_HEAD (if it hasn't been by a previous step)
git branch fetch_head FETCH_HEAD &> /dev/null || true

FETCH_HEAD=$(git rev-parse fetch_head)
UPSTREAM_BRANCH="$1"
UPSTREAM_HEAD="$2"
PR_BRANCH="$3"
PR_HEAD="$4"

# Ensure that enough has been fetched to have all the commits between the
# the two branches.

NEW=0
# Special case: new tags and new branches will have UPSTREAM_HEAD=0\{40}
if [[ -z ${UPSTREAM_HEAD//0/} ]]; then
  echo "$UPSTREAM_BRANCH is new: only testing HEAD"
  UPSTREAM_HEAD="$PR_HEAD~1"
  NEW=1
elif ! git log -1 "$UPSTREAM_HEAD" &> /dev/null ; then
  echo "$UPSTREAM_BRANCH has been force-pushed"
  git fetch origin "$UPSTREAM_HEAD" &> /dev/null
fi

if ! git merge-base "$UPSTREAM_HEAD" "$PR_HEAD" &> /dev/null; then
  echo "Determining merge-base of $UPSTREAM_HEAD..$PR_HEAD for $PR_BRANCH"

  DEEPEN=50
  MSG='Deepening'

  while ! git merge-base "$UPSTREAM_HEAD" "$PR_HEAD" &> /dev/null
  do
    echo " - $MSG by $DEEPEN commits from $FETCH_REF"
    git fetch origin --deepen=$DEEPEN "$FETCH_REF" &> /dev/null
    MSG='Further deepening'
    ((DEEPEN*=2))
  done
fi

MERGE_BASE=$(git merge-base "$UPSTREAM_HEAD" "$PR_HEAD")

if [[ $UPSTREAM_BRANCH != $PR_BRANCH ]]; then
  echo "$PR_BRANCH branched from $UPSTREAM_BRANCH at: $MERGE_BASE"
elif ((!NEW)); then
  echo "$UPSTREAM_BRANCH branched at: $MERGE_BASE"
fi
