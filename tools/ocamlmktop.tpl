#!/bin/sh
#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Damien Doligez, projet Para, INRIA Rocquencourt            #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$


# Dual shell script.  Works under both Bourne Shell and MPW Shell.

if : == x
then # Bourne Shell
     exec %%BINDIR%%/ocamlc -linkall toplevellib.cma "$@" topmain.cmo
else # MPW Shell
     ocamlc -linkall toplevellib.cma {"parameters"} topmain.cmo
     exit {status}
end
fi
