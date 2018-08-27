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

# This script extract the Pervasives submodule from stdlib.mli into
# pervasives.mli, for ocamldoc
BEGIN { state=0 }
/^\(\*MODULE_ALIASES\*\)\r?$/ && state == 0 { state=1 }
{
    if (state == 0) print
}
