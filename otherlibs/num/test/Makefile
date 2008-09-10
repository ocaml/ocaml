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

CAMLC=../../../boot/ocamlrun ../../../ocamlc -I ../../../stdlib -w A -warn-error A
CAMLOPT=../../../boot/ocamlrun ../../../ocamlopt -I ../../../stdlib
CC=$(BYTECC)
CFLAGS=-I.. -I../../../byterun $(BYTECCCOMPOPTS)

test: test.byt test.opt test_pi
	if $(SUPPORTS_SHARED_LIBRARIES); \
	  then ../../../byterun/ocamlrun -I .. ./test.byt; else ./test.byt; fi
	./test.opt

TESTFILES=test.cmo \
  test_nats.cmo test_big_ints.cmo test_ratios.cmo test_nums.cmo \
  test_io.cmo end_test.cmo

TESTOPTFILES=$(TESTFILES:.cmo=.cmx)

test.byt: $(TESTFILES) ../nums.cma ../libnums.a
	$(CAMLC) -ccopt -L.. -I .. -o test.byt -g ../nums.cma $(TESTFILES)

test.opt: $(TESTOPTFILES) ../nums.cmxa ../libnums.a
	$(CAMLOPT) -ccopt -L.. -I .. -o test.opt ../nums.cmxa $(TESTOPTFILES)

test_bng: test_bng.o
	$(CC) $(CFLAGS) -o test_bng ../bng.o test_bng.o -lbignum

$(TESTOPTFILES): ../../../ocamlopt

test_pi: test_pi.byt test_pi.bin

test_pi.byt: pi_big_int.cmo pi_num.cmo
	$(CAMLC) -ccopt -L.. -I .. -o pi_big_int.byt -g ../nums.cma pi_big_int.cmo
	$(CAMLC) -ccopt -L.. -I .. -o pi_num.byt -g ../nums.cma pi_num.cmo
	./pi_big_int.byt 1000
	./pi_num.byt 1000

test_pi.bin: pi_big_int.cmx pi_num.cmx
	$(CAMLOPT) -ccopt -L.. -I .. -o pi_big_int.bin -g ../nums.cmxa pi_big_int.cmx
	$(CAMLOPT) -ccopt -L.. -I .. -o pi_num.bin -g ../nums.cmxa pi_num.cmx
	./pi_big_int.bin 1000
	./pi_num.bin 1000

.SUFFIXES: .ml .cmo .cmx

.ml.cmo:
	$(CAMLC) -I .. -c -g $<

.ml.cmx:
	$(CAMLOPT) -I .. -c $<

ocamlnum:
	ocamlmktop -o ocamlnum -custom ../nums.cma ../libnums.a

clean:
	rm -f *.byt *.opt *.bin test_bng *.o *.cm? ocamlnum *~

depend:
	ocamldep *.ml > .depend

include .depend
