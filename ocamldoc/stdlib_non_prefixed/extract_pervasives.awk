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
/^module Pervasives : sig\r?$/ && state == 0 { state=1 }
/^end\r?$/                     && state == 2 { state=3 }
{
    if (state == 1) state=2;
    else if (state == 2) print
}
