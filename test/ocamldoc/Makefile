#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#         Maxence Guesdon, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2004 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$ 

ROOT=../..
include $(ROOT)/config/Makefile

CAMLC=$(ROOT)/boot/ocamlrun $(ROOT)/ocamlc
CAMLOPT=$(ROOT)/boot/ocamlrun $(ROOT)/ocamlopt
COMPFLAGS=-nostdlib -I $(ROOT)/stdlib -I KB -I Lex
OPTFLAGS=-S
CAMLYACC=$(ROOT)/yacc/ocamlyacc
YACCFLAGS=-v
CAMLLEX=$(ROOT)/boot/ocamlrun $(ROOT)/lex/ocamllex
CAMLDEP=$(ROOT)/boot/ocamlrun $(ROOT)/tools/ocamldep
CAMLRUN=$(ROOT)/byterun/ocamlrun
OCAMLDOC=$(ROOT)/boot/ocamlrun $(ROOT)/ocamldoc/ocamldoc
OCAMLDOC_TEST=$(OCAMLDOC) -g $(ROOT)/ocamldoc/odoc_test.cmo -warn-error


all:
	for i in *.ml; do $(MAKE) TARGET=`basename $$i .ml` test_one; done

test_one: results/$(TARGET).txt

results/$(TARGET).txt : $(TARGET).ml
	$(OCAMLDOC_TEST) -o $@ $< 2> results/$(TARGET).stderr

clean:
	rm -f results/*.txt results/*.stderr