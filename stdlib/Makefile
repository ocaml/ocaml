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
  hashtbl.cmo sort.cmo filename.cmo marshal.cmo obj.cmo \
  lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo stream.cmo \
  printf.cmo format.cmo arg.cmo printexc.cmo gc.cmo \
  digest.cmo random.cmo oo.cmo genlex.cmo callback.cmo weak.cmo \
  lazy.cmo

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
	ln -s stdlib.cmxa $(LIBDIR)/stdlib.p.cmxa
	ln -s stdlib.a $(LIBDIR)/stdlib.p.a
	ln -s std_exit.cmx $(LIBDIR)/std_exit.p.cmx
	ln -s std_exit.o $(LIBDIR)/std_exit.p.o

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
