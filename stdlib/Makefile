include ../Makefile.config

RUNTIME=../boot/cslrun
COMPILER=../cslc
CAMLC=$(RUNTIME) $(COMPILER)
OPTCOMPILER=../cslopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
CAMLDEP=../boot/cslrun ../tools/csldep

OBJS=pervasives.cmo list.cmo string.cmo char.cmo array.cmo sys.cmo \
  hashtbl.cmo sort.cmo filename.cmo obj.cmo lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo stream.cmo \
  printf.cmo format.cmo arg.cmo printexc.cmo gc.cmo \
  digest.cmo random.cmo

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

cslheader: header.c ../Makefile.config
	if $(SHARPBANGSCRIPTS); \
	then echo "#!$(BINDIR)/cslrun" > cslheader; \
	else $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) header.c -o cslheader; \
	     strip cslheader; fi

pervasives.cmi: pervasives.mli
	$(CAMLC) $(COMPFLAGS) -nopervasives -c pervasives.mli

pervasives.cmo: pervasives.ml
	$(CAMLC) $(COMPFLAGS) -nopervasives -c pervasives.ml

pervasives.cmx: pervasives.ml
	$(CAMLOPT) $(COMPFLAGS) -nopervasives -c pervasives.ml

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

$(OBJS) std_exit.cmo: pervasives.cmi
$(OBJS:.cmo=.cmx) std_exit.cmx: pervasives.cmi

$(OBJS): $(COMPILER)
$(OBJS:.cmo=.cmi): $(COMPILER)
$(OBJS:.cmo=.cmx): $(OPTCOMPILER)

clean:
	rm -f *.cm* *.o *.a
	rm -f cslheader
	rm -f *~

include .depend

depend:
	$(CAMLDEP) *.mli *.ml > .depend
