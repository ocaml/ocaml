#!/bin/sh

set -e

# Filter the output to ignore some unavoidable errors
output=$("$@" -Wpedantic -Werror -fdiagnostics-format=json 2>&1 | jq -f filter.jq)
if [ -z "${output}" ]; then
  # re-execute as non-avoidable errors have been filtered
  "$@"
  exit "${TEST_PASS}"
else
  # re-execute to get human-readable output
  exec "$@" -Wpedantic -Werror
fi
