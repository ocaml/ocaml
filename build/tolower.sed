#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#       Nicolas Pouillard, projet Gallium, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# tolower.sed expands one ...<:lower<FOO>>... to ...foo... per line
h
s/.*<:lower<\(.*\)>>.*/\1/
t cont
b end
:cont
y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/
s/$/|/
G
s/\(.*\)|\n\(.*\)<:lower<\(.*\)>>/\2\1/
:end
