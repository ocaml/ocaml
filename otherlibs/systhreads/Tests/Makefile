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
  testio.byt testsocket.byt testsignal.byt testsignal2.byt \
  torture.byt

include ../../../config/Makefile

CAMLC=../../../boot/ocamlrun ../../../ocamlc -I .. -I ../../unix -I ../../../stdlib 

CAMLOPT=../../../boot/ocamlrun ../../../ocamlopt -I .. -I ../../unix -I ../../../stdlib 

all: $(PROGS)

allopt: $(PROGS:.byt=.out)

clean:
	rm -f *.cm* *.byt *.out
	rm -f $(PROGS:.byt=.ml)

%.byt: ../../threads/Tests/%.ml
	cp ../../threads/Tests/$*.ml $*.ml
	$(CAMLC) -custom -o $*.byt unix.cma threads.cma $*.ml ../libthreads.a ../../unix/libunix.a -cclib -lpthread

%.out: ../../threads/Tests/%.ml
	cp ../../threads/Tests/$*.ml $*.ml
	$(CAMLOPT) -o $*.out unix.cmxa threads.cmxa $*.ml ../libthreadsnat.a ../../unix/libunix.a -cclib -lpthread

$(PROGS): ../threads.cma ../libthreads.a
$(PROGS:.byt=.out): ../threads.cmxa ../libthreadsnat.a
