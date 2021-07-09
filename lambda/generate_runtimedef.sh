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

# #10332: the meaning of character range a-z depends on the locale, so force C
#         locale throughout.
export LC_ALL=C
echo 'let builtin_exceptions = [|'
tr -d '\r' < "$1" | sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$|  \1;|p'
echo '|]'

echo 'let builtin_primitives = [|'
sed -e 's/.*/  "&";/' "$2"
echo '|]'
