#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$

# ocaml must be installed...

test: Tests Exemples

Tests:
	TERM=dumb ../ocaml < Tests.ml | tail +3 > Tests.proc
	- diff Tests.exp Tests.proc

Exemples:
	TERM=dumb ../ocaml < Exemples.ml | tail +3 > Exemples.proc
	- diff Exemples.exp Exemples.proc
