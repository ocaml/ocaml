#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License.          #
#                                                                       #
#########################################################################

# $Id$

include ../config/Makefile

RUNTIME=../boot/ocamlrun
COMPILER=../ocamlc
CAMLC=$(RUNTIME) $(COMPILER)
COMPFLAGS=-g
OPTCOMPILER=../ocamlopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
OPTCOMPFLAGS=
CAMLDEP=../boot/ocamlrun ../tools/ocamldep

OBJS=pervasives.cmo list.cmo char.cmo string.cmo array.cmo sys.cmo \
  hashtbl.cmo sort.cmo marshal.cmo obj.cmo \
  lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo stream.cmo \
  buffer.cmo printf.cmo format.cmo arg.cmo printexc.cmo gc.cmo \
  digest.cmo random.cmo oo.cmo genlex.cmo callback.cmo weak.cmo \
  lazy.cmo filename.cmo

all: stdlib.cma std_exit.cmo camlheader camlheader_ur

allopt: stdlib.cmxa std_exit.cmx allopt-$(PROFILING)

allopt-noprof:

allopt-prof: stdlib.p.cmxa std_exit.p.cmx

install:
	cp stdlib.cma std_exit.cmo *.cmi *.mli *.ml camlheader camlheader_ur $(LIBDIR)

installopt: installopt-default installopt-$(PROFILING)

installopt-default:
	cp stdlib.cmxa stdlib.a std_exit.o *.cmx $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) stdlib.a

installopt-noprof:
	rm -f $(LIBDIR)/stdlib.p.cmxa; ln -s stdlib.cmxa $(LIBDIR)/stdlib.p.cmxa
	rm -f $(LIBDIR)/stdlib.p.a; ln -s stdlib.a $(LIBDIR)/stdlib.p.a
	rm -f $(LIBDIR)/std_exit.p.cmx; ln -s std_exit.cmx $(LIBDIR)/std_exit.p.cmx
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
	  $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) -DRUNTIME_NAME='"$(BINDIR)/ocamlrun"' header.c -o camlheader && \
          strip camlheader && \
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
	@if test -f pervasives.cmx; then mv pervasives.cmx pervasives.n.cmx; else :; fi
	@if test -f pervasives.o; then mv pervasives.o pervasives.n.o; else :; fi
	$(CAMLOPT) $(OPTCOMPFLAGS) -p -nopervasives -c pervasives.ml
	mv pervasives.cmx pervasives.p.cmx
	mv pervasives.o pervasives.p.o
	@if test -f pervasives.n.cmx; then mv pervasives.n.cmx pervasives.cmx; else :; fi
	@if test -f pervasives.n.o; then mv pervasives.n.o pervasives.o; else :; fi

# oo.cmi must be compiled with -nopervasives for applets
oo.cmi: oo.mli
	$(CAMLC) $(COMPFLAGS) -nopervasives -c oo.mli

# stdlabel.cmo and morelabel.cmo use -labelize
stdlabel.cmo: stdlabel.ml
	$(CAMLC) $(COMPFLAGS) -labelize -c stdlabel.ml

stdlabel.cmx: stdlabel.ml
	$(CAMLOPT) $(OPTCOMPFLAGS) -labelize -c stdlabel.ml

stdlabel.p.cmx: stdlabel.ml
	@if test -f stdlabel.cmx; then mv stdlabel.cmx stdlabel.n.cmx; else :; fi
	@if test -f stdlabel.o; then mv stdlabel.o stdlabel.n.o; else :; fi
	$(CAMLOPT) $(OPTCOMPFLAGS) -p -labelize -c stdlabel.ml
	mv stdlabel.cmx stdlabel.p.cmx
	mv stdlabel.o stdlabel.p.o
	@if test -f stdlabel.n.cmx; then mv stdlabel.n.cmx stdlabel.cmx; else :; fi
	@if test -f stdlabel.n.o; then mv stdlabel.n.o stdlabel.o; else :; fi

morelabel.cmo: morelabel.ml
	$(CAMLC) $(COMPFLAGS) -labelize -c morelabel.ml

morelabel.cmx: morelabel.ml
	$(CAMLOPT) $(OPTCOMPFLAGS) -labelize -c morelabel.ml

morelabel.p.cmx: morelabel.ml
	@if test -f morelabel.cmx; then mv morelabel.cmx morelabel.n.cmx; else :; fi
	@if test -f morelabel.o; then mv morelabel.o morelabel.n.o; else :; fi
	$(CAMLOPT) $(OPTCOMPFLAGS) -p -labelize -c morelabel.ml
	mv morelabel.cmx morelabel.p.cmx
	mv morelabel.o morelabel.p.o
	@if test -f morelabel.n.cmx; then mv morelabel.n.cmx morelabel.cmx; else :; fi
	@if test -f morelabel.n.o; then mv morelabel.n.o morelabel.o; else :; fi

.SUFFIXES: .mli .ml .cmi .cmo .cmx .p.cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(OPTCOMPFLAGS) -c $<

.ml.p.cmx:
	@if test -f $*.cmx; then mv $*.cmx $*.n.cmx; else :; fi
	@if test -f $*.o; then mv $*.o $*.n.o; else :; fi
	$(CAMLOPT) $(OPTCOMPFLAGS) -p -c $<
	mv $*.cmx $*.p.cmx
	mv $*.o $*.p.o
	@if test -f $*.n.cmx; then mv $*.n.cmx $*.cmx; else :; fi
	@if test -f $*.n.o; then mv $*.n.o $*.o; else :; fi

$(OBJS) std_exit.cmo: pervasives.cmi $(COMPILER)
$(OBJS:.cmo=.cmx) std_exit.cmx: pervasives.cmi $(OPTCOMPILER)
$(OBJS:.cmo=.p.cmx) std_exit.p.cmx: pervasives.cmi $(OPTCOMPILER)
$(OBJS:.cmo=.cmi) std_exit.cmi: $(COMPILER)

clean::
	rm -f *.cm* *.o *.a
	rm -f *~

include .depend

depend:
	$(CAMLDEP) *.mli *.ml > .depend
