#!/bin/sh

#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, Collège de France and Inria                   *
#*                                                                        *
#*   Copyright 2023 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Build the runtime/prims.c file, with proper C declarations of the primitives

export LC_ALL=C

case $# in
  0) echo "Usage: gen_primsc.sh <primitives file> <.c files>" 1>&2
     exit 2;;
  *) primitives="$1"; shift;;
esac

cat <<'EOF'
/* Generated file, do not edit */

#define CAML_INTERNALS
#include "caml/mlvalues.h"
#include "caml/prims.h"
#include "caml/instruct.h"
#include "caml/startup.h"
#include "build_config.h"

EOF

# Extract the beginning of primitive definitions:
# from 'CAMLprim' at beginning of line to the first closing parenthesis.
# The first pattern below matches single-line definitions such as
#    CAMLprim value foo(value x) {
# The second pattern matches multi-line definitions such as
#    CAMLprim value foo(value x,
#                       value y)
sed -n \
  -e '/^CAMLprim value .*)/p' \
  -e '/^CAMLprim value [^)]*$/,/)/p' \
  "$@" |
# Transform these definitions into "CAMLextern" declarations
sed \
  -e 's/^CAMLprim /CAMLextern /' \
  -e 's/).*$/);/'

# Generate the table of primitives
echo
echo 'const c_primitive caml_builtin_cprim[] = {'
sed -e 's/.*/  (c_primitive) &,/' "$primitives"
echo '  0 };'

# Generate the table of primitive names
echo
echo 'const char * const caml_names_of_builtin_cprim[] = {'
sed -e 's/.*/  "&",/' "$primitives"
echo '  0 };'

# ocamlrun values for symbols which are provided by the bytecode linker
# - ocamlrun is able to use any of the mechanisms to load the bytecode
# - caml_runtime_standard_library_default for bytecode images on this runtime
# - caml_start_code, caml_code_size, caml_marshalled_global_data,
#   caml_marshalled_global_data_size, caml_section and caml_section_table_size
#   are generated for -output-obj and so forth; stub values are included here so
#   that the caml_startup functions can all still be linked in ocamlrun (even
#   though they cannot be called)
cat <<'EOF'

const enum caml_byte_program_mode caml_byte_program_mode = STANDARD;
const char_os *caml_runtime_standard_library_default = OCAML_STDLIB_DIR;
opcode_t caml_start_code[] = {STOP};
asize_t caml_code_size = sizeof(caml_start_code);
char * caml_marshalled_global_data = NULL;
asize_t caml_marshalled_global_data_size = 0;
char * caml_section_table = NULL;
asize_t caml_section_table_size = 0;
EOF
