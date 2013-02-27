#######################################################################
#                                                                     #
#                 MLTk, Tcl/Tk interface of OCaml                     #
#                                                                     #
#    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    #
#               projet Cristal, INRIA Rocquencourt                    #
#            Jacques Garrigue, Kyoto University RIMS                  #
#                                                                     #
#  Copyright 2002 Institut National de Recherche en Informatique et   #
#  en Automatique and Kyoto University.  All rights reserved.         #
#  This file is distributed under the terms of the GNU Library        #
#  General Public License, with the special exception on linking      #
#  described in file LICENSE found in the OCaml source tree.          #
#                                                                     #
#######################################################################

include ../support/Makefile.common

# We are using the non-installed library !
BYT_COMPFLAGS=-I ../lib -I ../camltk -I ../support -I $(OTHERS)/unix -w s -dllpath ../support
BIN_COMPFLAGS=-I ../lib -I ../camltk -I ../support -I $(OTHERS)/unix -w s

WITH_BYT_CAMLTK=labltk.cma camltk.cmo
WITH_BIN_CAMLTK=labltk.cmxa camltk.cmx

BYT_EXECS =\
  addition.byt helloworld.byt winskel.byt fileinput.byt\
  eyes.byt tetris.byt mytext.byt fileopen.byt\

BIN_EXECS=$(BYT_EXECS:.byt=.bin)

EXECS=$(BYT_EXECS:.byt=$(EXE))

all: byt bin

byt: $(BYT_EXECS)

#opt: hello.opt demo.opt eyes.opt calc.opt clock.opt tetris.opt

bin: opt

opt: $(BIN_EXECS)

addition.bin: addition.cmx
	echo $(BIN_EXECS);
	$(CAMLOPT) $(BIN_COMPFLAGS) -o $@ $(WITH_BIN_CAMLTK) addition.cmx

helloworld.bin: helloworld.cmx
	$(CAMLOPT) $(BIN_COMPFLAGS) -o $@ $(WITH_BIN_CAMLTK) helloworld.cmx

winskel.bin: winskel.cmx
	$(CAMLOPT) $(BIN_COMPFLAGS) -o $@ $(WITH_BIN_CAMLTK) winskel.cmx

fileinput.bin: fileinput.cmx
	$(CAMLOPT) $(BIN_COMPFLAGS) -o $@ $(WITH_BIN_CAMLTK) unix.cmxa fileinput.cmx

socketinput.bin: socketinput.cmx
	$(CAMLOPT) $(BIN_COMPFLAGS) -o $@ unix.cmxa $(WITH_BIN_CAMLTK) socketinput.cmx

eyes.bin: eyes.cmx
	$(CAMLOPT) $(BIN_COMPFLAGS) -o $@ unix.cmxa $(WITH_BIN_CAMLTK) eyes.cmx

tetris.bin: tetris.cmx
	$(CAMLOPT) $(BIN_COMPFLAGS) -o $@ unix.cmxa $(WITH_BIN_CAMLTK) tetris.cmx

mytext.bin: mytext.cmx
	$(CAMLOPT) $(BIN_COMPFLAGS) -o $@ unix.cmxa $(WITH_BIN_CAMLTK) mytext.cmx

fileopen.bin: fileopen.cmx
	$(CAMLOPT) $(BIN_COMPFLAGS) -o $@ unix.cmxa $(WITH_BIN_CAMLTK) fileopen.cmx


addition.byt: addition.cmo
	$(CAMLC) $(BYT_COMPFLAGS) -o $@ $(LIBNAME).cma addition.cmo

helloworld.byt: helloworld.cmo
	$(CAMLC) $(BYT_COMPFLAGS) -o $@ $(LIBNAME).cma helloworld.cmo

winskel.byt: winskel.cmo
	$(CAMLC) $(BYT_COMPFLAGS) -o $@ $(LIBNAME).cma winskel.cmo

fileinput.byt: fileinput.cmo
	$(CAMLC) $(BYT_COMPFLAGS) -o $@ unix.cma $(LIBNAME).cma fileinput.cmo

socketinput.byt: socketinput.cmo
	$(CAMLC) $(BYT_COMPFLAGS) -o $@ unix.cma $(LIBNAME).cma socketinput.cmo

eyes.byt: eyes.cmo
	$(CAMLC) $(BYT_COMPFLAGS) -o $@ unix.cma $(LIBNAME).cma eyes.cmo

tetris.byt: tetris.cmo
	$(CAMLC) $(BYT_COMPFLAGS) -o $@ unix.cma $(LIBNAME).cma tetris.cmo

mytext.byt: mytext.cmo
	$(CAMLC) $(BYT_COMPFLAGS) -o $@ unix.cma $(LIBNAME).cma mytext.cmo

# graph.byt: graphics.cmo graphics_test.cmo
# 	  $(CAMLC) -o $@ graphics.cmo graphics_test.cmo
#
# graphics_test.cmo: graphics.cmo

fileopen.byt: fileopen.cmo
	$(CAMLC) $(BYT_COMPFLAGS) -o $@ $(LIBNAME).cma fileopen.cmo

clean :
	rm -f *.cm? *.o a.out $(EXECS) $(BYT_EXECS) $(BIN_EXECS)

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx .cma .cmxa

.mli.cmi:
	$(CAMLCOMP) $(BYT_COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLCOMP) $(BYT_COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(BIN_COMPFLAGS) -c $<
