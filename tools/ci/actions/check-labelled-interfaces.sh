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

# Hygiene Checks: Ensure that *Labels module docs are in sync with the
# unlabelled version.

MSG='CheckSyncStdlibDocs is a no-op'

tools/sync_stdlib_docs
if git diff --quiet --exit-code; then
  echo -e "$MSG: \e[32mYES\e[0m"
else
  echo -e "$MSG: \e[31mNO\e[0m"
  echo "CheckSyncStdlibDocs: failure with the following differences:"
  git --no-pager diff
  cat<<EOF
------------------------------------------------------------------------
This should be fixable by just running tools/sync_stdlib_docs and
eviewing the changes it makes.
------------------------------------------------------------------------
EOF
  git checkout .
  exit 1
fi
