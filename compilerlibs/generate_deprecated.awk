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

# For each unit name, print the contents of the compatibility shim on stdout.
#
# Arguments:
#
# - PREFIX: the prefix being used for this unit
# - NAME: the name of the unit (including the extension .cmo or .cmi)

function capitalize(s)
{
    return toupper(substr(s,1,1)) substr(s,2)
}

BEGIN {
    # Extract basename and extension
    n = split(NAME, a, "/")
    split(a[n], b, ".")
    base = b[1]
    ext = b[2]

    # Form Prefix.Basename
    s = capitalize(PREFIX) "." capitalize(base)

    # Print the contents of the compatibility shim
    print "[@@@ocaml.deprecated \"Use", s, "instead.\"]"
    if (ext == "cmo")
        print "include", s
    else # .cmi case
        print "include module type of struct include", s, "end"
}
