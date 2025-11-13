#!/bin/sh
# Build tty_driver with platform-specific linker flags

# On Linux, openpty is in libutil and requires -lutil
# On macOS/BSD, openpty is in libc and doesn't need extra flags

if [ "$(uname -s)" = "Linux" ]; then
    UTIL_FLAG="-cclib -lutil"
else
    UTIL_FLAG=""
fi

${ocamlc} -custom -o tty_driver.exe unix.cma tty_driver.ml tty_stubs.c ${UTIL_FLAG}
