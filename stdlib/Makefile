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
  hashtbl.cmo sort.cmo filename.cmo obj.cmo lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo stream.cmo \
  printf.cmo format.cmo arg.cmo printexc.cmo gc.cmo \
  digest.cmo random.cmo oo.cmo genlex.cmo callback.cmo weak.cmo marshal.cmo \
  lazy.cmo

all: stdlib.cma std_exit.cmo camlheader

allopt: stdlib.cmxa std_exit.cmx

install:
	cp stdlib.cma std_exit.cmo *.cmi *.mli *.ml camlheader $(LIBDIR)

installopt:
	cp stdlib.cmxa stdlib.a std_exit.o *.cmx $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) stdlib.a

stdlib.cma: $(OBJS)
	$(CAMLC) -a -o stdlib.cma $(OBJS)

stdlib.cmxa: $(OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o stdlib.cmxa $(OBJS:.cmo=.cmx)

camlheader: header.c ../config/Makefile
	if $(SHARPBANGSCRIPTS); \
	then echo "#!$(BINDIR)/ocamlrun" > camlheader; \
	else $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) '-DRUNTIME_NAME="$(BINDIR)/ocamlrun"' header.c -o camlheader; \
	     strip camlheader; \
        fi

clean::
	rm -f camlheader

pervasives.cmi: pervasives.mli
	$(CAMLC) $(COMPFLAGS) -nopervasives -c pervasives.mli

pervasives.cmo: pervasives.ml
	$(CAMLC) $(COMPFLAGS) -nopervasives -c pervasives.ml

pervasives.cmx: pervasives.ml
	$(CAMLOPT) $(OPTCOMPFLAGS) -nopervasives -c pervasives.ml

# oo.cmi must be compiled with -nopervasives for applets
oo.cmi: oo.mli
	$(CAMLC) $(COMPFLAGS) -nopervasives -c oo.mli

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(OPTCOMPFLAGS) -c $<

$(OBJS) std_exit.cmo: pervasives.cmi
$(OBJS:.cmo=.cmx) std_exit.cmx: pervasives.cmi

$(OBJS) std_exit.cmo: $(COMPILER)
$(OBJS:.cmo=.cmi) std_exit.cmi: $(COMPILER)
$(OBJS:.cmo=.cmx) std_exit.cmx: $(OPTCOMPILER)

clean::
	rm -f *.cm* *.o *.a
	rm -f *~

include .depend

depend:
	$(CAMLDEP) *.mli *.ml > .depend
