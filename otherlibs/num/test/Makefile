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

include ../../../config/Makefile

CAMLC=../../../boot/ocamlrun ../../../ocamlc -I ../../../stdlib
CAMLOPT=../../../boot/ocamlrun ../../../ocamlopt -I ../../../stdlib
CC=$(BYTECC)
CFLAGS=-I.. $(BYTECCCOMPOPTS)

test: test.byt test.opt
	if $(SUPPORTS_SHARED_LIBRARIES); then ../../../byterun/ocamlrun -I .. ./test.byt; else ./test.byt; fi
	./test.opt

TESTFILES=test.cmo \
  test_nats.cmo test_big_ints.cmo test_ratios.cmo test_nums.cmo \
  test_io.cmo end_test.cmo

TESTOPTFILES=$(TESTFILES:.cmo=.cmx)

test.byt: $(TESTFILES) ../nums.cma ../libnums.a
	$(CAMLC) -ccopt -L.. -o test.byt ../nums.cma $(TESTFILES)

test.opt: $(TESTOPTFILES) ../nums.cmxa ../libnums.a
	$(CAMLOPT) -ccopt -L.. -o test.opt ../nums.cmxa $(TESTOPTFILES)

test_bng: test_bng.o
	$(CC) $(CFLAGS) -o test_bng ../bng.o test_bng.o -lbignum

$(TESTOPTFILES): ../../../ocamlopt

.SUFFIXES: .ml .cmo .cmx

.ml.cmo:
	$(CAMLC) -I .. -c $<

.ml.cmx:
	$(CAMLOPT) -I .. -c $<

ocamlnum:
	ocamlmktop -o ocamlnum -custom ../nums.cma ../libnums.a

clean:
	rm -f test.byt test.opt test_bng *.o *.cm? ocamlnum

depend:
	ocamldep *.ml > .depend

include .depend
