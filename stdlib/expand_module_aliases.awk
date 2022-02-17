#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 Jeremie Dimino, Jane Street Europe                     *
#*                                                                        *
#*   Copyright 2017 Jane Street Group LLC                                 *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# This script adds the Stdlib__ prefixes to the module aliases in
# stdlib.ml and stdlib.mli
BEGIN { state=0 }
NR == 1 { printf ("# 1 \"%s\"\n", FILENAME) }
/\(\*MODULE_ALIASES\*\)\r?/ { state=1 }
{ if (state==0)
    { if (FILENAME ~ /Labels/ &&
          sub(/@since [^(]* \(/, "@since ")) sub(/ in [^)]*\)/, ""); print; }
  else if (state==1)
    state=2;
  else if ($1 == "module")
  { if (ocamldoc!="true") printf("\n(** @canonical Stdlib.%s *)", $2);
    printf("\nmodule %s = Stdlib__%s\n", $2, $4);
  }
  else
    print
}
