include ../Makefile.config

RUNTIME=../boot/camlrun
COMPILER=../camlc
CAMLC=$(RUNTIME) $(COMPILER)
CAMLDEP=../tools/camldep

OBJS=pervasives.cmo list.cmo string.cmo char.cmo array.cmo sys.cmo \
  hashtbl.cmo sort.cmo filename.cmo obj.cmo lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo \
  printf.cmo format.cmo arg.cmo printexc.cmo gc.cmo

all: stdlib.cma cslheader

install:
	cp stdlib.cma *.cmi *.mli cslheader $(LIBDIR)

stdlib.cma: $(OBJS)
	$(CAMLC) -a -o stdlib.cma $(OBJS)

cslheader: header.c ../Makefile.config
	if $(SHARPBANGSCRIPTS); \
	then echo "#!$(BINDIR)/cslrun" > cslheader; \
	else $(CC) $(CCCOMPOPTS) $(CCLINKOPTS) header.c -o cslheader; \
	     strip cslheader; fi

pervasives.cmi: pervasives.mli
	$(CAMLC) -nopervasives -c pervasives.mli

pervasives.cmo: pervasives.ml
	$(CAMLC) -nopervasives -c pervasives.ml

.SUFFIXES: .mli .ml .cmi .cmo

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

$(OBJS): pervasives.cmi

$(OBJS): $(COMPILER)
$(OBJS:.cmo=.cmi): $(COMPILER)

clean:
	rm -f *.cm[ioa]
	rm -f cslheader
	rm -f *~

include .depend

depend:
	$(CAMLDEP) *.mli *.ml > .depend
