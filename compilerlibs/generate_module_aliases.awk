#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                        Nicolas Ojeda Bar, LexiFi                       *
#*                                                                        *
#*   Copyright 2020 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Generate "map" files for each prefixed library.
#
# For example, for ocamlcommon, the map file will be of the form
#
# module                           Annot = Ocamlcommon__annot
# module                      Arg_helper = Ocamlcommon__arg_helper
# module                      Ast_helper = Ocamlcommon__ast_helper
# module                  Ast_invariants = Ocamlcommon__ast_invariants
#       etc.
#
# Read on stdin the list of basenames of the concerned units, without exntension
# or directory-part. They will appear in the map file in the same order in which
# they were read (but note that the order does not matter).

function capitalize(s)
{
    return toupper(substr(s,1,1)) substr(s,2)
}

# pad(n, s)
#
# Pads a string s to n characters by prefixing with spaces.

function pad(n, s)
{
    m = n - length(s)
    for (j = 0; j < m; j++) s = " " s
    return s
}

{
    maxlen = 0;
    for (i = 1; i <= NF; i++) {
        long[i] = $i
        short[i] = $i
        sub(/^[a-zA-Z]+__/,"", short[i])
        len = length(short[i])
        maxlen = (maxlen < len) ? len : maxlen
    }
    for (i = 1; i <= NF; i++) {
        print "module", pad(maxlen, capitalize(short[i])), "=", capitalize(long[i])
    }
}
