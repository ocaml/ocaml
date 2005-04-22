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

include ../config/Makefile

CAMLC=../boot/ocamlrun ../ocamlc
CAMLOPT=../boot/ocamlrun ../ocamlopt
COMPFLAGS=-nostdlib -I ../stdlib -I ../gcamllib
OPTFLAGS=-S
CAMLYACC=../yacc/ocamlyacc
YACCFLAGS=-v
CAMLLEX=../boot/ocamlrun ../lex/ocamllex
CAMLDEP=../boot/ocamlrun ../tools/ocamldep
CAMLRUN=../byterun/ocamlrun
CODERUNPARAMS=OCAMLRUNPARAM='o=100'

BYTE_EXE=plus.byt double.byt print.byt gprt.byt

CODE_EXE=$(BYTE_EXE:.byt=.out)

# default: all codetest bytetest
default: all bytetest codetest

all: $(BYTE_EXE) $(CODE_EXE)

# Common rules

.SUFFIXES:
.SUFFIXES: .mli .ml .cmi .cmo .cmx .byt .fast.byt .out .fast.out

.ml.byt:
	$(CAMLC) $(COMPFLAGS) -o $*.byt gcamllib.cma $<

.ml.fast.byt:
	cp $*.ml $*_fast.ml
	$(CAMLC) $(COMPFLAGS) -unsafe -o $*.fast.byt $*_fast.ml
	rm -f $*_fast.ml

.ml.out:
	$(CAMLOPT) $(COMPFLAGS) $(OPTFLAGS) -o $*.out gcamllib.cmxa $<

.ml.fast.out:
	cp $*.ml $*_fast.ml
	$(CAMLOPT) $(COMPFLAGS) $(OPTFLAGS) -unsafe -o $*.fast.out $*_fast.ml
	rm -f $*_fast.ml

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) $(OPTFLAGS) -c $<

$(BYTE_EXE) $(BYTE_KB) $(BYTE_GENLEX): ../ocamlc
$(BYTE_EXE): ../stdlib/stdlib.cma
$(CODE_EXE) $(CODE_KB) $(CODE_GENLEX): ../ocamlopt
$(CODE_EXE): ../stdlib/stdlib.cmxa ../stdlib/libasmrun.a

clean::
	rm -f *.byt *.out
	rm -f *.cm[iox] *.[os]
	rm -f *~

# Regression test

test: codetest

bytetest:
	set -e; \
        for prog in $(BYTE_EXE:.byt=); do \
          echo $$prog; \
          if test -f Results/$$prog.runtest; then \
            sh Results/$$prog.runtest test $(CAMLRUN) $$prog.byt; \
          else \
            $(CAMLRUN) $$prog.byt | cmp - Results/$$prog.out; \
          fi; \
        done

codetest:
	set -e; \
        for prog in $(CODE_EXE:.out=); do \
          echo $$prog; \
          if test -f Results/$$prog.runtest; then \
            sh Results/$$prog.runtest test ./$$prog.out; \
          else \
            ./$$prog.out | cmp - Results/$$prog.out; \
          fi; \
        done

clean::
	rm -f Lex/testscanner.ml

# Benchmark

bench: codebench

bytebench:
	set -e; \
        for prog in $(BYTE_EXE:.byt=); do \
          echo "$$prog                " | cut -c 1-16 | tr -d '\012'; \
          if test -f Results/$$prog.runtest; then \
            sh Results/$$prog.runtest bench $(CAMLRUN) $$prog.byt; \
          else \
            xtime -o /dev/null -e /dev/null $(CAMLRUN) $$prog.byt; \
          fi; \
        done

codebench:
	set -e; \
        for prog in $(CODE_EXE:.out=); do \
          echo "$$prog                " | cut -c 1-16 | tr -d '\012'; \
          if test -f Results/$$prog.runtest; then \
            $(CODERUNPARAMS) sh Results/$$prog.runtest bench ./$$prog.out; \
          else \
            $(CODERUNPARAMS) xtime -repeat 3 -o /dev/null -e /dev/null ./$$prog.out; \
          fi; \
        done

# Dependencies

depend: beforedepend
	$(CAMLDEP) -I KB -I Lex *.mli *.ml KB/*.mli KB/*.ml Lex/*.mli Lex/*.ml > .depend

include .depend

