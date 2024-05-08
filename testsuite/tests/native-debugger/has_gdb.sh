#!/bin/sh

version () {
    echo "$@" | awk -F. '{ printf("%d%03d%03d\n", $1,$2,$3); }'
}

if ! which gdb > /dev/null 2>&1; then
    echo "gdb not available" > "${ocamltest_response}"
    exit ${TEST_SKIP}
else
    # Linux check for GDB version
    GDB_VERSION=$(gdb --version |head -n 1 | awk -F' ' '{print $5}')
    if [ $(version "$GDB_VERSION") -ge $(version "12.1") ]; then
        exit ${TEST_PASS}
    else
        exit ${TEST_SKIP}
    fi
fi
