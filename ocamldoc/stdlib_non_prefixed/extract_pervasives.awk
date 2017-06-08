# This script extract the Pervasives submodule from stdlib.mli into
# pervasives.mli, for ocamldoc
BEGIN { state=0 }
/^module Pervasives : sig\r?$/ && state == 0 { state=1 }
/^end\r?$/                     && state == 2 { state=3 }
{
    if (state == 1) state=2;
    else if (state == 2) print
}
