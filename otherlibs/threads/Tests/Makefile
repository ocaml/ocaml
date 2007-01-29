#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../../LICENSE.#
#                                                                       #
#########################################################################

# $Id$

PROGS=test1.byt test2.byt test3.byt test4.byt test5.byt test6.byt \
  test7.byt test8.byt test9.byt testA.byt sieve.byt \
  testio.byt testsocket.byt testwait.byt testsignal.byt testsignal2.byt \
  testsieve.byt token1.byt token2.byt

CAMLC=../../../boot/ocamlrun ../../../ocamlc -I .. -I ../../../stdlib -I ../../unix

include ../../../config/Makefile

all: $(PROGS)

clean:
	rm -f *.cm* *.byt

sorts.byt: sorts.ml
	$(CAMLC) -o sorts.byt -I ../../graph threads.cma graphics.cma sorts.ml $(LIBS) $(X11_LINK)

.SUFFIXES: .ml .byt

.ml.byt:
	$(CAMLC) -custom -o $*.byt unix.cma threads.cma $*.ml $(LIBS)

$(PROGS): ../threads.cma ../libvmthreads.a
