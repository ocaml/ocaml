#!/bin/bash

OCAML_OS="$1"

# Extract the first 4 parts of an LLDB version number
version () {
    echo "$@" | awk -F. '{ printf("%d%03d%03d%03d\n", $1,$2,$3,$4); }'
}

if ! which lldb > /dev/null 2>&1; then
    echo "lldb not available" > "${ocamltest_response}"
    exit ${TEST_SKIP}
else
    if [ "$OCAML_OS" = "macos" ]; then
        # macOS version
        LLDB_VERSION=$(lldb --version |head -n 1 | awk -F'-' '{print $2}')
        # We need XCode 15.3 or greater
        # lldb-1500.0.404.7
        # Apple Swift version 5.10 (swiftlang-5.10.0.13 clang-1500.3.9.4)
        if [ $(version "$LLDB_VERSION") -ge $(version "1500.0.404.7") ]; then
            exit ${TEST_PASS}
        else
            exit ${TEST_SKIP}
        fi
    elif [ "$OCAML_OS" = "linux" ]; then
        # Linux version
        LLDB_VERSION=$(lldb --version |awk -F' ' '{print $3}')
        if [ $(version "$LLDB_VERSION") -ge $(version "14.0.0") ]; then
            exit ${TEST_PASS}
        else
            exit ${TEST_SKIP}
        fi
    else
        exit ${TEST_SKIP}
    fi


fi
