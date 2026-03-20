#!/usr/bin/env bash

LDVER=$(ld -v 2>&1 | egrep -o "PROJECT:ld(64)?-[0-9]*" | sed -E "s/PROJECT:ld(64)?-//")

# Extract the first 3 parts of an LD version number
version () {
    echo "$@" | awk -F. '{ printf("%d%03d%03d\n", $1,$2,$3); }'
}

if [ $(version "$LDVER") -lt $(version "224.0.0.0") ]; then
  echo "ld version is $LDVER, only 224 or above are supported.";
  test_result=${TEST_SKIP};
else
  test_result=${TEST_PASS};
fi

exit ${test_result}
