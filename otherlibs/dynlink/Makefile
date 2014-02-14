#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../LICENSE.   #
#                                                                       #
#########################################################################

# Makefile for the dynamic link library

include ../../config/Makefile

CAMLC=../../boot/ocamlrun ../../ocamlc
CAMLOPT=../../ocamlcompopt.sh
INCLUDES=-I ../../utils -I ../../typing -I ../../bytecomp -I ../../asmcomp
COMPFLAGS=-w +33..39 -warn-error A -I ../../stdlib $(INCLUDES)

OBJS=dynlinkaux.cmo dynlink.cmo

COMPILEROBJS=\
  ../../utils/misc.cmo ../../utils/config.cmo ../../utils/clflags.cmo \
  ../../utils/tbl.cmo ../../utils/consistbl.cmo \
  ../../utils/terminfo.cmo ../../utils/warnings.cmo \
  ../../parsing/asttypes.cmi \
  ../../parsing/location.cmo ../../parsing/longident.cmo \
  ../../typing/ident.cmo ../../typing/path.cmo \
  ../../typing/primitive.cmo ../../typing/types.cmo \
  ../../typing/btype.cmo ../../typing/subst.cmo ../../typing/predef.cmo \
  ../../typing/datarepr.cmo ../../typing/cmi_format.cmo ../../typing/env.cmo \
  ../../bytecomp/lambda.cmo ../../bytecomp/instruct.cmo \
  ../../bytecomp/cmo_format.cmi ../../bytecomp/opcodes.cmo \
  ../../bytecomp/runtimedef.cmo ../../bytecomp/bytesections.cmo \
  ../../bytecomp/dll.cmo ../../bytecomp/meta.cmo \
  ../../bytecomp/symtable.cmo

NATOBJS=dynlink.cmx

all: dynlink.cma extract_crc

allopt: dynlink.cmxa

dynlink.cma: $(OBJS)
	$(CAMLC) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a -o dynlink.cma \
	         $(OBJS)

dynlink.cmxa: $(NATOBJS)
	$(CAMLOPT) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a -o dynlink.cmxa \
	           $(NATOBJS)

dynlinkaux.cmo: $(COMPILEROBJS)
	$(CAMLC) $(COMPFLAGS) -pack -o dynlinkaux.cmo $(COMPILEROBJS)

dynlinkaux.cmi: dynlinkaux.cmo

dynlink.cmx: dynlink.cmi natdynlink.ml
	cp natdynlink.ml  dynlink.mlopt
	$(CAMLOPT) -c $(COMPFLAGS) -impl dynlink.mlopt
	rm -f dynlink.mlopt

extract_crc: dynlink.cma extract_crc.cmo
	$(CAMLC) $(COMPFLAGS) -o extract_crc dynlink.cma extract_crc.cmo

install:
	cp dynlink.cmi dynlink.cma dynlink.mli $(LIBDIR)
	cp extract_crc $(LIBDIR)/extract_crc$(EXE)

installopt:
	if $(NATDYNLINK); then \
	  cp $(NATOBJS) dynlink.cmxa dynlink.$(A) $(LIBDIR) && \
	  cd $(LIBDIR) && $(RANLIB) dynlink.$(A); \
	fi

partialclean:
	rm -f extract_crc *.cm[ioax] *.cmxa

clean: partialclean
	rm -f *.$(A) *.$(O) *.so *.dll dynlink.mlopt

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:

dynlink.cmo: dynlinkaux.cmi dynlink.cmi
extract_crc.cmo: dynlink.cmi
