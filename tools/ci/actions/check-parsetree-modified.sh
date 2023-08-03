#!/usr/bin/env bash
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 Paul-Elliot Anglès d'Auriac, Tarides                   *
#*                                                                        *
#*   Copyright 2023 Tarides                                               *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

set -e

# Hygiene Checks: check that whenever the parsetree.mli file has been modified,
# the parsetree-change label has been applied to the PR

# Exactly of the following must be true:
#   - No commit in the PR alters the parsetree.mli file
#   - The parsetree-change label is applied to the PR

API_URL="$1"
shift 1

AUTH="authorization: Bearer $GITHUB_TOKEN"

# We need all the commits in the PR to be available
. tools/ci/actions/deepen-fetch.sh

COMMIT_RANGE="$MERGE_BASE..$PR_HEAD"

LABEL='parsetree-change'

if ! git diff "$COMMIT_RANGE" --name-only --exit-code parsing/parsetree.mli \
   > /dev/null; then
  echo -e "The parsetree has been modified."
  if curl --silent --header "$AUTH" "$API_URL/labels" | grep -q "$LABEL"; then
    echo -e "Label $LABEL is assigned to the PR."
  else
    echo -e "Please assign the label $LABEL to the PR"
    exit 1
  fi
else
  echo -e "The parsetree has not been modified."
  if curl --silent --header "$AUTH" "$API_URL/labels" | grep -q "$LABEL"; then
    echo -e "Please remove the label $LABEL to the PR"
    exit 1
  else
    echo -e "Label $LABEL is not assigned to the PR"
  fi
fi
