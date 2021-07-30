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

# Hygiene Checks: ensure that configure.ac generates configure
# This tests both branches and PRs. Any commit which updates either files which
# affect configure (configure.ac, VERSION, aclocal.m4 and build-aux/*) and also
# which alter this script.
# The behaviour is slightly different for pushes vs pull requests: in a PR, all
# commits must be correct; in a push, it must be the case that the configure is
# correct at the tip of the branch. This allows you to push a correcting PR to
# trunk, for example, but rejects a PR which includes bad commits (for increased
# bisect safety).

set -e

if [[ $1 = 'pull_request' ]]; then
  ALL_COMMITS_MUST_PASS=1
else
  ALL_COMMITS_MUST_PASS=0
fi

# We need all the commits in the PR to be available
. tools/ci/actions/deepen-fetch.sh

# Display failing commits in red for PRs and yellow for branches (error/warning)
if ((ALL_COMMITS_MUST_PASS)); then
  COLOR='31'
else
  COLOR='33'
fi

CI_SCRIPT='tools/ci/actions/check-configure.sh'
PATHS=\
'configure\|configure\.ac\|VERSION\|aclocal\.m4\|build-aux/.*'\
'\|tools/autogen\|tools/git-dev-options\.sh'

# $1 - commit to checkout files from
# $2 - range of commits to diff
# When testing a single commit, $1 and $2 will be the same; when validating the
# tip of a branch, $1 will be HEAD and $2 will be the range of commits in the
# branch.
CheckTree () {
  RET=0
  COMMIT="$1"
  COMMITS_TO_SEARCH="$2"
  if git diff-tree --diff-filter=d --no-commit-id --name-only -r \
       "$COMMITS_TO_SEARCH" | grep -qx "$PATHS\|$CI_SCRIPT"; then
    git checkout -qB return
    git checkout -q "$COMMIT"
    mv configure configure.ref
    make -s configure
    if diff -q configure configure.ref >/dev/null ; then
      echo -e "$COMMIT: \e[32mconfigure.ac generates configure\e[0m"
    else
      RET=1
      echo -e \
        "$COMMIT: \e[${COLOR}mconfigure.ac doesn't generate configure\e[0m"
    fi
    mv configure.ref configure
    git checkout -q return
  fi
  return $RET
}

# $RESULT is 1 for success and 0 for error
RESULT=1
# We traverse the commits in commit order; if $ALL_COMMITS_MUST_PASS=0, the
# success of the most recent commit of the branch (traversed last) will
# override any previous failure.
for commit in $(git rev-list "$MERGE_BASE..$PR_HEAD" --reverse); do
  if CheckTree "$commit" "$commit"; then
    if ((!ALL_COMMITS_MUST_PASS)); then
      # Commit passed, so reset any previous failure
      RESULT=1
    fi
  else
    RESULT=0
  fi
done

if ((!RESULT)); then
  echo 'configure.ac no longer generates configure'
  if ((ALL_COMMITS_MUST_PASS)); then
    echo 'Please rebase the PR, editing the commits identified above and run:'
  else
    echo 'Please fix the branch by committing changes after running:'
  fi
  echo 'make -B configure'
  exit 1
fi
