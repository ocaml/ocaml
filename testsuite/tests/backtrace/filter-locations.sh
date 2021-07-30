#!/bin/sh
# This location filter is erasing information from the backtrace
# to be robust to different inlining choices made by different compiler settings.
# It checks that the expected locations occur (in the expected order).
sed -e "s/^.*in file/File/" -e 's/ (inlined)//' | grep ^File
