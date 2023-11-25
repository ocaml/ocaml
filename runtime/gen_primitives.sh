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

# If primitives contain duplicated lines (e.g. because the code is defined
# like
# #ifdef X
# CAMLprim value caml_foo() ...
# #else
# CAMLprim value caml_foo() ...
# #endif), horrible things will happen: duplicated entries in Runtimedef ->
# double registration in Symtable -> empty entry in the PRIM table ->
# the bytecode interpreter is confused.
# We sort the primitive file and remove duplicates to avoid this problem.

# Warning: we use "sort | uniq" instead of "sort -u" because in the MSVC
# port, the "sort" program in the path is Microsoft's and not cygwin's

# Warning: POSIX sort is locale dependent, that's why we set LC_ALL explicitly.
# Sort is unstable for "is_directory" and "isatty"
# see http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sort.html:
# "using sort to process pathnames, it is recommended that LC_ALL .. set to C"

# #8985: in sed, the meaning of character range a-z depends on the locale,
# so force C locale throughout.

export LC_ALL=C

case $# in
  0) echo "Usage: gen_primitives.sh <primitives file> <.c files>" 1>&2
     exit 2;;
  *) primitives="$1"; shift;;
esac

tmp_primitives="$primitives.tmp$$"

sed -n -e 's/^CAMLprim value \([a-z][a-z0-9_]*\).*$/\1/p' "$@" | \
sort | uniq > "$tmp_primitives"

# To speed up builds, we avoid changing "primitives" when files
# containing primitives change but the primitives table does not

if test -f "$primitives" && cmp -s "$tmp_primitives" "$primitives"
then rm "$tmp_primitives"
else mv "$tmp_primitives" "$primitives"
fi
