#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../LICENSE.      #
#                                                                       #
#########################################################################

# $Id$

include ../config/Makefile

RUNTIME=../boot/ocamlrun
COMPILER=../ocamlc
CAMLC=$(RUNTIME) $(COMPILER)
COMPFLAGS=-g -warn-error A -nostdlib
OPTCOMPILER=../ocamlopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
OPTCOMPFLAGS=-warn-error A -nostdlib
CAMLDEP=../boot/ocamlrun ../tools/ocamldep

NOBUILTINFLAG=-nobuiltintypes

OBJS=builtintypes.cmo \
  pervasives.cmo $(OTHERS)
OTHERS=array.cmo list.cmo char.cmo string.cmo sys.cmo \
  hashtbl.cmo sort.cmo marshal.cmo obj.cmo \
  int32.cmo int64.cmo nativeint.cmo \
  lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo stream.cmo buffer.cmo \
  printf.cmo format.cmo scanf.cmo \
  arg.cmo printexc.cmo gc.cmo \
  digest.cmo random.cmo callback.cmo camlinternalOO.cmo oo.cmo \
  genlex.cmo weak.cmo \
  lazy.cmo filename.cmo complex.cmo \
  rtype.cmo \
  arrayLabels.cmo listLabels.cmo stringLabels.cmo moreLabels.cmo stdLabels.cmo

# make MODE=migration when you want to port gcaml to the latest ocaml-cvs
ifeq ($(MODE),migration)
ifeq ($(CAMLC),../boot/ocamlrun ../boot/ocamlc)
# migration + coldstart
NOBUILTINFLAG:=
OBJS:=pervasives.cmo $(OTHERS)
endif
endif

all: stdlib.cma std_exit.cmo camlheader camlheader_ur

allopt: stdlib.cmxa std_exit.cmx allopt-$(PROFILING)

allopt-noprof:

allopt-prof: stdlib.p.cmxa std_exit.p.cmx
	rm -f std_exit.p.cmi

install:
	cp stdlib.cma std_exit.cmo *.cmi *.mli *.ml camlheader camlheader_ur \
           $(LIBDIR)

installopt: installopt-default installopt-$(PROFILING)

installopt-default:
	cp stdlib.cmxa stdlib.a std_exit.o *.cmx $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) stdlib.a

installopt-noprof:
	rm -f $(LIBDIR)/stdlib.p.cmxa; ln -s stdlib.cmxa $(LIBDIR)/stdlib.p.cmxa
	rm -f $(LIBDIR)/stdlib.p.a; ln -s stdlib.a $(LIBDIR)/stdlib.p.a
	rm -f $(LIBDIR)/std_exit.p.cmx; \
          ln -s std_exit.cmx $(LIBDIR)/std_exit.p.cmx
	rm -f $(LIBDIR)/std_exit.p.o; ln -s std_exit.o $(LIBDIR)/std_exit.p.o

installopt-prof:
	cp stdlib.p.cmxa stdlib.p.a std_exit.p.cmx std_exit.p.o $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) stdlib.p.a

stdlib.cma: $(OBJS)
	$(CAMLC) -a -o stdlib.cma $(OBJS)

stdlib.cmxa: $(OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o stdlib.cmxa $(OBJS:.cmo=.cmx)

stdlib.p.cmxa: $(OBJS:.cmo=.p.cmx)
	$(CAMLOPT) -a -o stdlib.p.cmxa $(OBJS:.cmo=.p.cmx)

camlheader camlheader_ur: header.c ../config/Makefile
	if $(SHARPBANGSCRIPTS); then \
          echo '#!$(BINDIR)/ocamlrun' > camlheader && \
          echo '#!' | tr -d '\012' > camlheader_ur; \
        else \
	  $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) \
                    -DRUNTIME_NAME='"$(BINDIR)/ocamlrun"' \
                    header.c -o tmpheader$(EXE) && \
          strip tmpheader$(EXE) && \
	  mv tmpheader$(EXE) camlheader && \
          cp camlheader camlheader_ur; \
        fi

clean::
	rm -f camlheader camlheader_ur

pervasives.cmi: pervasives.mli
	$(CAMLC) $(COMPFLAGS) -nopervasives -c pervasives.mli

pervasives.cmo: pervasives.ml
	$(CAMLC) $(COMPFLAGS) -nopervasives -c pervasives.ml

pervasives.cmx: pervasives.ml
	$(CAMLOPT) $(OPTCOMPFLAGS) -nopervasives -c pervasives.ml

pervasives.p.cmx: pervasives.ml
	@if test -f pervasives.cmx; \
           then mv pervasives.cmx pervasives.n.cmx; else :; fi
	@if test -f pervasives.o; \
           then mv pervasives.o pervasives.n.o; else :; fi
	$(CAMLOPT) $(OPTCOMPFLAGS) -p -nopervasives -c pervasives.ml
	mv pervasives.cmx pervasives.p.cmx
	mv pervasives.o pervasives.p.o
	@if test -f pervasives.n.cmx; \
           then mv pervasives.n.cmx pervasives.cmx; else :; fi
	@if test -f pervasives.n.o; \
           then mv pervasives.n.o pervasives.o; else :; fi

builtintypes.cmi: builtintypes.mli
	$(CAMLC) $(COMPFLAGS) $(NOBUILTINFLAG) -c builtintypes.mli

builtintypes.cmo: builtintypes.ml
	$(CAMLC) $(COMPFLAGS) $(NOBUILTINFLAG) -c builtintypes.ml

builtintypes.cmx: builtintypes.ml
	$(CAMLOPT) $(OPTCOMPFLAGS) $(NOBUILTINFLAG) -c builtintypes.ml

builtintypes.p.cmx: builtintypes.ml
	@if test -f builtintypes.cmx; \
           then mv builtintypes.cmx builtintypes.n.cmx; else :; fi
	@if test -f builtintypes.o; \
           then mv builtintypes.o builtintypes.n.o; else :; fi
	$(CAMLOPT) $(OPTCOMPFLAGS) -p $(NOBUILTINFLAG) -c builtintypes.ml
	mv builtintypes.cmx builtintypes.p.cmx
	mv builtintypes.o builtintypes.p.o
	@if test -f builtintypes.n.cmx; \
           then mv builtintypes.n.cmx builtintypes.cmx; else :; fi
	@if test -f builtintypes.n.o; \
           then mv builtintypes.n.o builtintypes.o; else :; fi

rtype.cmi: rtype.mli
	$(CAMLC) $(COMPFLAGS) -rectypes -c rtype.mli

rtype.cmo: rtype.ml
	$(CAMLC) $(COMPFLAGS) -rectypes -c rtype.ml

rtype.cmx: rtype.ml
	$(CAMLOPT) $(OPTCOMPFLAGS) -rectypes -c rtype.ml

rtype.p.cmx: rtype.ml
	@if test -f rtype.cmx; \
           then mv rtype.cmx rtype.n.cmx; else :; fi
	@if test -f rtype.o; \
           then mv rtype.o rtype.n.o; else :; fi
	$(CAMLOPT) $(OPTCOMPFLAGS) -p -rectypes -c rtype.ml
	mv rtype.cmx rtype.p.cmx
	mv rtype.o rtype.p.o
	@if test -f rtype.n.cmx; \
           then mv rtype.n.cmx rtype.cmx; else :; fi
	@if test -f rtype.n.o; \
           then mv rtype.n.o rtype.o; else :; fi

# camlinternalOO.cmi must be compiled with -nopervasives for applets
camlinternalOO.cmi: camlinternalOO.mli
	$(CAMLC) $(COMPFLAGS) -nopervasives -c camlinternalOO.mli

CIOO=camlinternalOO
$(CIOO).cmx $(CIOO).p.cmx: $(CIOO).ml
	$(MAKE) EXTRAFLAGS="-inline 0" CIOO=dummy $@

# labelled modules require the -nolabels flag
labelled.cmo:
	$(MAKE) EXTRAFLAGS=-nolabels RUNTIME=$(RUNTIME) \
                COMPILER=$(COMPILER) $(LABELLED:.ml=.cmo)
	touch $@
labelled.cmx:
	$(MAKE) EXTRAFLAGS=-nolabels $(LABELLED:.ml=.cmx)
	touch $@
labelled.p.cmx:
	$(MAKE) EXTRAFLAGS=-nolabels $(LABELLED:.ml=.p.cmx)
	touch $@

.SUFFIXES: .mli .ml .cmi .cmo .cmx .p.cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) `./Compflags $@` -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) `./Compflags $@` -c $<

.ml.cmx:
	$(CAMLOPT) $(OPTCOMPFLAGS) `./Compflags $@` -c $<

.ml.p.cmx:
	$(CAMLOPT) $(OPTCOMPFLAGS) `./Compflags $@` -p -c -o $*.p.cmx $<

# Dependencies on the compiler
$(OBJS) std_exit.cmo: $(COMPILER)
$(OBJS:.cmo=.cmi) std_exit.cmi: $(COMPILER)
$(OBJS:.cmo=.cmx) std_exit.cmx: $(OPTCOMPILER)
$(OBJS:.cmo=.p.cmx) std_exit.p.cmx: $(OPTCOMPILER)

# Dependencies on Pervasives (not tracked by ocamldep)
$(OBJS) std_exit.cmo: pervasives.cmi
$(OTHERS:.cmo=.cmi) std_exit.cmi: pervasives.cmi
$(OBJS:.cmo=.cmx) std_exit.cmx: pervasives.cmi
$(OBJS:.cmo=.p.cmx) std_exit.p.cmx: pervasives.cmi
$(OTHERS:.cmo=.cmx) std_exit.cmx: pervasives.cmx
$(OTHERS:.cmo=.p.cmx) std_exit.p.cmx: pervasives.cmx

clean::
	rm -f *.cm* *.o *.a
	rm -f *~

include .depend

depend:
	$(CAMLDEP) *.mli *.ml > .depend
