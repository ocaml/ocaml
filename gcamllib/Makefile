#########################################################################
#                                                                       #
#                                G'Caml                                 #
#                                                                       #
#                   Jun Furuse, University of Tokyo                     #
#                                                                       #
#   Copyright 2005 Institut National de Recherche en Informatique et    #
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
COMPFLAGS=-warn-error Agy -nostdlib -I ../stdlib -I ../toplevel -I ../typing
OPTCOMPILER=../ocamlopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
OPTCOMPFLAGS=-warn-error Ag -nostdlib -I ../stdlib -I ../toplevel -I ../typing
CAMLDEP=../boot/ocamlrun ../tools/ocamldep

# OBJS=gcaml.cmo safevio.cmo gprint.cmo plus.cmo rec.cmo cplus.cmo
OBJS=gcaml.cmo garith.cmo safevio.cmo gprint.cmo plus.cmo rec.cmo cplus.cmo
ALLOBJS=$(OBJS)

all: gcamllib.cma

allopt: gcamllib.cmxa allopt-$(PROFILING)

allopt-noprof:

allopt-prof: gcamllib.p.cmxa

install:
	cp gcamllib.cma *.cmi *.mli *.ml \
           $(LIBDIR)

installopt: installopt-default installopt-$(PROFILING)

installopt-default:
	cp gcamllib.cmxa gcamllib.a *.cmx $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) gcamllib.a

installopt-noprof:
	rm -f $(LIBDIR)/gcamllib.p.cmxa; ln -s gcamllib.cmxa $(LIBDIR)/gcamllib.p.cmxa
	rm -f $(LIBDIR)/gcamllib.p.a; ln -s gcamllib.a $(LIBDIR)/gcamllib.p.a

installopt-prof:
	cp gcamllib.p.cmxa gcamllib.p.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) gcamllib.p.a

gcamllib.cma: $(OBJS)
	$(CAMLC) -a -o gcamllib.cma $(ALLOBJS)

gcamllib.cmxa: $(OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o gcamllib.cmxa $(ALLOBJS:.cmo=.cmx)

gcamllib.p.cmxa: $(OBJS:.cmo=.p.cmx)
	$(CAMLOPT) -a -o gcamllib.p.cmxa $(ALLOBJS:.cmo=.p.cmx)

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

$(ALLOBJS): $(COMPILER)
$(ALLOBJS:.cmo=.cmx): $(OPTCOMPILER)
$(ALLOBJS:.cmo=.p.cmx): $(OPTCOMPILER)
$(ALLOBJS:.cmo=.cmi): $(COMPILER)

clean::
	rm -f *.cm* *.o *.a
	rm -f *~

include .depend

depend:
	$(CAMLDEP) *.mli *.ml > .depend
