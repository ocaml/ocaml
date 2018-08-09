#!/bin/bash

(
  for prim in \
      alloc array compare extern floats gc_ctrl hash intern interp ints io \
      lexing md5 meta obj parsing signals str sys callback weak finalise \
      stacks dynlink backtrace_byt backtrace spacetime_byt afl bigarray
  do
      sed -n -e "s/CAMLprim value \([a-z0-9_][a-z0-9_]*\).*/\1/p" "$prim.c"
  done
) | LC_ALL=C sort | uniq
