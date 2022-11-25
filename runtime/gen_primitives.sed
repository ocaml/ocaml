#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                     Antonin Decimo, Tarides, Paris                     *
#*                                                                        *
#*   Copyright 2022 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

/^CAMLprim value/ {
  :loop
  /)/ ! {
    N; b loop
  }

  s/\n  */ /g

  s/^CAMLprim value \([a-z0-9_][a-z0-9_]*\) *(\([^)]*\)).*$/\1(\2)/p;
}
