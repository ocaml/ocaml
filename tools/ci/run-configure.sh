#!/bin/sh
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                             Antonin Decimo                             *
#*                                                                        *
#*   Copyright 2026 Antonin Decimo                                        *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Run ./configure with the given arguments. If it fails, display the content
# of config.log before exiting with configure's exit status.
# Used by both the GitHub Actions workflows and the Inria Jenkins CI.

failed=0
./configure "$@" || failed=$?

if [ "$failed" -ne 0 ]; then
  # Disable tracing (if any) so that it does not pollute the output
  set +x
  # Output can be a little unpredictable in CI logs: ensure that the fold is
  # definitely on a new line
  echo
  if [ ! -f config.log ]; then
    echo "configure failed with exit status $failed (no config.log)"
    exit "$failed"
  fi
  lines=$(($(wc -l < config.log)))
  if [ "${GITHUB_ACTIONS:-}" = 'true' ]; then
    echo "::group::config.log content ($lines lines)"
    cat config.log
    echo '::endgroup::'
  else
    echo "======== config.log content ($lines lines) ========"
    cat config.log
    echo '======== end of config.log ========'
  fi
  exit "$failed"
fi
