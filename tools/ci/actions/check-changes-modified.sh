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

set -e

# Hygiene Checks: check that Changes has been updated in PRs
# One of the following must be true:
#   - A commit in the PR alters the Changes file
#   - A commit in the PR contains a line like 'No change needed' ($REGEX below)
#   - The no-change-entry-needed label is applied to the PR (handled in YAML)

# We need all the commits in the PR to be available
. tools/ci/actions/deepen-fetch.sh

MSG='Check Changes has been updated'
COMMIT_RANGE="$MERGE_BASE..$PR_HEAD"

# Check if Changes has been updated in the PR
if git diff "$COMMIT_RANGE" --name-only --exit-code Changes > /dev/null; then
  # Check if any commit messages include something like No Changes entry needed
  REGEX='[Nn]o [Cc]hange.* needed'
  if [[ -n $(git log --grep="$REGEX" --max-count=1 "$COMMIT_RANGE") ]]; then
    echo -e "$MSG: \e[33mSKIPPED\e[0m (owing to commit message)"
  else
    echo -e "$MSG: \e[31mNO\e[0m"
    cat <<"EOF"
------------------------------------------------------------------------
Most contributions should come with a message in the Changes file, as
described in our contributor documentation:

  https://github.com/ocaml/ocaml/blob/trunk/CONTRIBUTING.md#changelog

Some very minor changes (typo fixes for example) may not need
a Changes entry. In this case, you may explicitly disable this test by
adding the code word "No change entry needed" (on a single line) to
a commit message of the PR, or using the "no-change-entry-needed" label
on the github pull request.
------------------------------------------------------------------------
EOF
    exit 1
  fi
else
  echo -e "$MSG: \e[32mYES\e[0m"
fi
