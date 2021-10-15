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
#   - The no-change-entry-needed label is applied to the PR (handled in YAML)

# We need all the commits in the PR to be available
. tools/ci/actions/deepen-fetch.sh

MSG='Check Changes has been updated'
COMMIT_RANGE="$MERGE_BASE..$PR_HEAD"

# Check if Changes has been updated in the PR
if git diff "$COMMIT_RANGE" --name-only --exit-code Changes > /dev/null; then
  echo -e "$MSG: \e[31mNO\e[0m"
  cat <<"EOF"
------------------------------------------------------------------------
Most contributions should come with a message in the Changes file, as
described in our contributor documentation:

  https://github.com/ocaml/ocaml/blob/trunk/CONTRIBUTING.md#changelog

Some very minor changes (typo fixes for example) may not need
a Changes entry. In this case, you may explicitly disable this test by
using the "no-change-entry-needed" label on the github pull request.
------------------------------------------------------------------------
EOF
  exit 1
else
  echo -e "$MSG: \e[32mYES\e[0m"
fi
