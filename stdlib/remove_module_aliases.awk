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

# This script remove the module aliases from stdlib.ml and stdlib.mli
# so that ocamldep doesn't register dependencies from stdlib to all
# other modules
BEGIN { in_aliases=0 }
NR == 1 { printf ("# 1 \"%s\"\n", FILENAME) }
/^\(\*MODULE_ALIASES\*\)\r?$/ { in_aliases=1 }
!in_aliases { print }
