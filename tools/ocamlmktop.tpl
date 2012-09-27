#!/bin/sh
#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Damien Doligez, projet Para, INRIA Rocquencourt            #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id: ocamlmktop.tpl 12959 2012-09-27 13:12:51Z maranget $

exec %%BINDIR%%/jocamlc -I +compiler-libs -linkall ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma "$@" topstart.cmo
