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

# Hygiene Checks: Ensure that all the .depend files are up-to-date.

MSG='make alldepend is a no-op'

make alldepend

# note: we cannot use $? as (set -e) may be set globally,
# and disabling it locally is not worth the hassle.
# note: we ignore the whitespace in case different C dependency
# detectors use different indentation styles.
if git diff --ignore-all-space --quiet --exit-code **.depend; then
  echo -e "$MSG: \e[32mYES\e[0m"
else
  echo -e "$MSG: \e[31mNO\e[0m"
  echo "CheckDepend: failure with the following differences:"
  git --no-pager diff --ignore-all-space **.depend
  cat<<EOF
------------------------------------------------------------------------
This should be fixable by just running make alldepend after building the
compiler.
------------------------------------------------------------------------
EOF
  exit 1
fi
