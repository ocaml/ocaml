#!/bin/sh

#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# duplicated from $(ROOTDIR)/runtime/Makefile

# #8985: the meaning of character range a-z depends on the locale, so force C
#        locale throughout.
export LC_ALL=C
(
  for prim in \
      alloc array compare extern floats gc_ctrl hash intern interp ints io \
      lexing md5 meta memprof obj parsing signals str sys callback weak \
      finalise stacks dynlink backtrace_byt backtrace spacetime_byt afl \
      bigarray eventlog
  do
      sed -n -e 's/^CAMLprim value \([a-z0-9_][a-z0-9_]*\).*/\1/p' "$prim.c"
  done
  sed -n -e 's/^CAMLprim_int64_[0-9](\([a-z0-9_][a-z0-9_]*\)).*/caml_int64_\1\
caml_int64_\1_native/p' ints.c
) | sort | uniq
