# Makefile for the dynamic link library

include ../../config/Makefile

CAMLC=../../boot/cslrun ../../boot/cslc
INCLUDES=-I ../../utils -I ../../typing -I ../../bytecomp
COMPFLAGS=-I ../../boot $(INCLUDES)

OBJS=dynlink.cmo
COMPILEROBJS=misc.cmo tbl.cmo clflags.cmo config.cmo ident.cmo \
  predef.cmo runtimedef.cmo symtable.cmo opcodes.cmo

all: dynlink.cma extract_crc

allopt:

dynlink.cma: $(OBJS)
	$(CAMLC) $(COMPFLAGS) -a -o dynlink.cma $(COMPILEROBJS) $(OBJS)

extract_crc: dynlink.cma extract_crc.cmo
	$(CAMLC) $(COMPFLAGS) -o extract_crc dynlink.cma extract_crc.cmo

install:
	cp dynlink.cmi dynlink.cma extract_crc $(LIBDIR)

installopt:

clean:
	rm -f extract_crc *.cm[ioa]

realclean:

.SUFFIXES: .ml .mli .cmo .cmi

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

depend:
	../../tools/csldep *.mli *.ml > .depend

include .depend
