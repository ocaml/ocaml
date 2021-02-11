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

# Hygiene Checks: ensure that check-typo passes for all files
# This tests both branches and PRs. It is capable of requiring that every commit
# in a PR satisfies check-typo, but at present it only requires that the HEAD
# of the branch satisfies it.

set -e

# Set to 1 to require all commits individually to pass check-typo
CHECK_ALL_COMMITS=0

# We need all the commits in the PR to be available
. tools/ci/actions/deepen-fetch.sh

# Test to see if any part of the directory name has been marked prune
not_pruned () {
  DIR=$(dirname "$1")
  if [[ $DIR = '.' ]] ; then
    return 0
  else
    case ",$(git check-attr typo.prune "$DIR" | sed -e 's/.*: //')," in
      ,set,)
      return 1
      ;;
      *)

      not_pruned "$DIR"
      return $?
    esac
  fi
}

# $1 - commit to checkout files from
# $2 - range of commits to diff
CheckTypoTree () {
  COMMIT="$1"
  COMMITS_TO_SEARCH="$2"
  export OCAML_CT_HEAD="$COMMIT"
  export OCAML_CT_LS_FILES="git diff-tree --no-commit-id --name-only -r \
$COMMITS_TO_SEARCH --"
  export OCAML_CT_CAT='git cat-file --textconv'
  export OCAML_CT_PREFIX="$COMMIT:"
  GIT_INDEX_FILE=tmp-index git read-tree --reset -i "$COMMIT"
  git diff-tree --diff-filter=d --no-commit-id --name-only -r \
    "$COMMITS_TO_SEARCH" | (while IFS= read -r path
  do
    if not_pruned "$path" ; then
      echo "Checking $COMMIT: $path"
      if ! tools/check-typo "./$path" ; then
        touch failed
      fi
    else
      echo "NOT checking $COMMIT: $path (typo.prune)"
    fi
  done)
  rm -f tmp-index
}

# tmp-index is used to ensure that correct version of .gitattributes is used by
# check-typo
export OCAML_CT_GIT_INDEX='tmp-index'
export OCAML_CT_CA_FLAG='--cached'
rm -f failed

COMMIT_RANGE="$MERGE_BASE..$PR_HEAD"
if ((CHECK_ALL_COMMITS)); then
  # Check each commit in turn
  for commit in $(git rev-list "$COMMIT_RANGE" --reverse); do
    CheckTypoTree "$commit" "$commit"
  done
else
  # Use the range of commits just to get the list of files to check; only HEAD
  # is scanned.
  CheckTypoTree "$FETCH_HEAD" "$COMMIT_RANGE"
fi

if [[ -e failed ]]; then
  exit 1
fi
