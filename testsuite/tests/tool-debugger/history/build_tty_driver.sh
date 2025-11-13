#!/bin/sh
# Build tty_driver with platform-specific linker flags
# Usage: build_tty_driver.sh <ocamlc_path>

OCAMLC="$1"

# On Linux, openpty is in libutil and requires -lutil
# On macOS/BSD, openpty is in libc and doesn't need extra flags
if [ "$(uname -s)" = "Linux" ]; then
    UTIL_FLAG="-cclib -lutil"
else
    UTIL_FLAG=""
fi

"$OCAMLC" -custom -o tty_driver.exe unix.cma tty_driver.ml tty_stubs.c $UTIL_FLAG
