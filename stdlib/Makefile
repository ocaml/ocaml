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
COMPFLAGS=-warn-error A -nostdlib
OPTCOMPILER=../ocamlopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
OPTCOMPFLAGS=-warn-error A -nostdlib
CAMLDEP=../boot/ocamlrun ../tools/ocamldep

BASIC=pervasives.cmo array.cmo list.cmo char.cmo string.cmo sys.cmo \
  hashtbl.cmo sort.cmo marshal.cmo obj.cmo \
  int32.cmo int64.cmo nativeint.cmo \
  lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo stream.cmo buffer.cmo \
  printf.cmo format.cmo scanf.cmo \
  arg.cmo printexc.cmo gc.cmo \
  digest.cmo random.cmo callback.cmo camlinternalOO.cmo oo.cmo \
  genlex.cmo weak.cmo \
  lazy.cmo filename.cmo complex.cmo
LABELLED=arrayLabels.ml listLabels.ml stringLabels.ml moreLabels.ml

OBJS=$(BASIC) labelled.cmo stdLabels.cmo
ALLOBJS=$(BASIC) $(LABELLED:.ml=.cmo) stdLabels.cmo

all: stdlib.cma std_exit.cmo camlheader camlheader_ur

allopt: stdlib.cmxa std_exit.cmx allopt-$(PROFILING)

allopt-noprof:

allopt-prof: stdlib.p.cmxa std_exit.p.cmx

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
	$(CAMLC) -a -o stdlib.cma $(ALLOBJS)

stdlib.cmxa: $(OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o stdlib.cmxa $(ALLOBJS:.cmo=.cmx)

stdlib.p.cmxa: $(OBJS:.cmo=.p.cmx)
	$(CAMLOPT) -a -o stdlib.p.cmxa $(ALLOBJS:.cmo=.p.cmx)

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
	$(CAMLC) $(COMPFLAGS) $(EXTRAFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) $(EXTRAFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(OPTCOMPFLAGS) $(EXTRAFLAGS) -c $<

.ml.p.cmx:
	@if test -f $*.cmx; then mv $*.cmx $*.n.cmx; else :; fi
	@if test -f $*.o; then mv $*.o $*.n.o; else :; fi
	$(CAMLOPT) $(OPTCOMPFLAGS) $(EXTRAFLAGS) -p -c $<
	mv $*.cmx $*.p.cmx
	mv $*.o $*.p.o
	@if test -f $*.n.cmx; then mv $*.n.cmx $*.cmx; else :; fi
	@if test -f $*.n.o; then mv $*.n.o $*.o; else :; fi

$(ALLOBJS) labelled.cmo std_exit.cmo: pervasives.cmi $(COMPILER)
$(ALLOBJS:.cmo=.cmx) labelled.cmx std_exit.cmx: pervasives.cmi $(OPTCOMPILER)
$(ALLOBJS:.cmo=.p.cmx) labelled.p.cmx std_exit.p.cmx: pervasives.cmi $(OPTCOMPILER)
$(ALLOBJS:.cmo=.cmi) std_exit.cmi: $(COMPILER)
labelled.cmo labelled.cmx labelled.p.cmx: $(LABELLED) $(LABELLED:.ml=.mli)

clean::
	rm -f *.cm* *.o *.a
	rm -f *~

include .depend

depend:
	$(CAMLDEP) *.mli *.ml > .depend
