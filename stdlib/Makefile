include ../config/Makefile

RUNTIME=../boot/cslrun
COMPILER=../cslc
CAMLC=$(RUNTIME) $(COMPILER)
OPTCOMPILER=../cslopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
CAMLDEP=../boot/cslrun ../tools/csldep
CPPFLAGS=-DUNIX

OBJS=pervasives.cmo list.cmo string.cmo char.cmo array.cmo sys.cmo \
  hashtbl.cmo sort.cmo filename.cmo obj.cmo lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo stream.cmo \
  printf.cmo format.cmo arg.cmo printexc.cmo gc.cmo \
  digest.cmo random.cmo oo.cmo

all: stdlib.cma std_exit.cmo cslheader

allopt: stdlib.cmxa std_exit.cmx

install:
	cp stdlib.cma std_exit.cmo *.cmi *.mli cslheader $(LIBDIR)

installopt:
	cp stdlib.cmxa stdlib.a std_exit.o *.cmx $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) stdlib.a

stdlib.cma: $(OBJS)
	$(CAMLC) -a -o stdlib.cma $(OBJS)

stdlib.cmxa: $(OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o stdlib.cmxa $(OBJS:.cmo=.cmx)

cslheader: header.c ../config/Makefile
	if $(SHARPBANGSCRIPTS); \
	then echo "#!$(BINDIR)/cslrun" > cslheader; \
	else $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) header.c -o cslheader; \
	     strip cslheader; fi

clean::
	rm -f cslheader

pervasives.cmi: pervasives.mli
	$(CAMLC) $(COMPFLAGS) -nopervasives -c pervasives.mli

pervasives.cmo: pervasives.ml
	$(CAMLC) $(COMPFLAGS) -nopervasives -c pervasives.ml

pervasives.cmx: pervasives.ml
	$(CAMLOPT) $(COMPFLAGS) -nopervasives -c pervasives.ml

filename.ml: filename.mlp ../config/Makefile
	@rm -f filename.ml
	$(CPP) $(CPPFLAGS) filename.mlp > filename.ml || \
        { rm -f filename.ml; exit 2; }
	@chmod -w filename.ml

clean::
	rm -f filename.ml

beforedepend:: filename.ml

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

$(OBJS) std_exit.cmo: pervasives.cmi
$(OBJS:.cmo=.cmx) std_exit.cmx: pervasives.cmi

$(OBJS) std_exit.cmo: $(COMPILER)
$(OBJS:.cmo=.cmi) std_exit.cmi: $(COMPILER)
$(OBJS:.cmo=.cmx) std_exit.cmx: $(OPTCOMPILER)

clean::
	rm -f *.cm* *.o *.a
	rm -f *~

include .depend

depend: beforedepend
	$(CAMLDEP) *.mli *.ml > .depend
