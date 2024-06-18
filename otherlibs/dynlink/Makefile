#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                     Mark Shinwell, Jane Street Europe                  *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*   Copyright 2018--2019 Jane Street Group LLC                           *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Makefile for the dynamic link library

# FIXME reduce redundancy by including ../Makefile

ROOTDIR = ../..

include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

OCAMLC=$(BEST_OCAMLC) $(STDLIBFLAGS) -g
OCAMLOPT=$(BEST_OCAMLOPT) $(STDLIBFLAGS) -g

# COMPFLAGS should be in sync with the toplevel Makefile's COMPFLAGS.
COMPFLAGS=-strict-sequence -principal -absname \
          -w +a-4-9-40-41-42-44-45-48 -warn-error +A \
          -bin-annot -strict-formats
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS += -O3
endif

%.cmo: COMPFLAGS += -I byte
%.cmi: COMPFLAGS += -I byte
OPTCOMPFLAGS += -I native

OBJS=dynlink_config.cmo dynlink_types.cmo dynlink_platform_intf.cmo \
  dynlink_common.cmo byte/dynlink_symtable.cmo byte/dynlink.cmo

NATOBJS=dynlink_config.cmx dynlink_types.cmx dynlink_platform_intf.cmx \
  dynlink_common.cmx native/dynlink.cmx

%/dynlink.cmi: dynlink.cmi dynlink.mli
	cp $^ $*/

all: dynlink.cma

allopt: dynlink.cmxa

dynlink.cma: $(OBJS)
	$(V_LINKC)$(OCAMLC) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a -I byte -o $@ $^

dynlink.cmxa: $(NATOBJS)
	$(V_LINKOPT)$(OCAMLOPT) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a -I native \
	            -o $@ $^
# As for all other .cmxa files, ensure that the .cmx files are in the same
# directory. If this were omitted, ocamldoc in particular will fail to build
# with a -opaque warning. Note that installopt refers to $(NATOBJS) so doesn't
# require this file to exist, hence its inclusion in the recipe for dynlink.cmxa
# rather than as a dependency elsewhere.
	cp native/dynlink.cmx dynlink.cmx

# Since there is no .mli for [Dynlink_platform_intf], we need to be
# careful that compilation of the .cmx file does not write the .cmi file again,
# which would cause rebuilding of ocamlopt.  The easiest way to do this seems
# to be to copy the .ml file, which is a valid .mli, to the .mli.
dynlink_platform_intf.mli: dynlink_platform_intf.ml
	cp $< $@

INSTALL_LIBDIR_DYNLINK = $(INSTALL_LIBDIR)/dynlink

install:
# If installing over a previous OCaml version, ensure dynlink is removed from
# the previous installation.
	rm -f "$(INSTALL_LIBDIR)"/dynlink.cm* "$(INSTALL_LIBDIR)/dynlink.mli" \
        "$(INSTALL_LIBDIR)/dynlink.$(A)" \
        $(addprefix "$(INSTALL_LIBDIR)/", $(notdir $(NATOBJS)))
	$(MKDIR) "$(INSTALL_LIBDIR_DYNLINK)"
	$(INSTALL_DATA) \
	  dynlink.cmi dynlink.cma META \
	  "$(INSTALL_LIBDIR_DYNLINK)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  dynlink.cmti dynlink.mli \
	  "$(INSTALL_LIBDIR_DYNLINK)"
endif

installopt:
ifeq "$(strip $(NATDYNLINK))" "true"
	$(INSTALL_DATA) \
	  $(NATOBJS) dynlink.cmxa dynlink.$(A) \
	  "$(INSTALL_LIBDIR_DYNLINK)"
endif

partialclean:
	rm -f *.cm[ioaxt] *.cmti *.cmxa \
	      byte/*.cm[iot] byte/*.cmti \
	      native/*.cm[ixt] native/*.cmti native/*.o native/*.obj

clean: partialclean
	rm -f *.a *.lib *.o *.obj *.so *.dll dynlink_platform_intf.mli \
	      byte/dynlink.mli native/dynlink.mli

.PHONY: distclean
distclean: clean
	rm -f META dynlink_config.ml dynlink_cmo_format.mli \
	dynlink_cmxs_format.mli

.PHONY: beforedepend
beforedepend: dynlink_platform_intf.mli

.PHONY: depend
DEPEND_DUMMY_FILES=\
  byte/dynlink.mli \
  native/dynlink.mli

depend: beforedepend
	$(V_GEN)touch $(DEPEND_DUMMY_FILES) && \
	$(OCAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash \
	  -I byte -bytecode *.mli *.ml byte/*.mli byte/*.ml > .depend && \
	$(OCAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash \
	  -I native -native *.ml native/dynlink.ml >> .depend && \
	rm -f $(DEPEND_DUMMY_FILES)

include .depend

%.cmi: %.mli
	$(V_OCAMLC)$(OCAMLC) -c $(COMPFLAGS) $<

%.cmo: %.ml
	$(V_OCAMLC)$(OCAMLC) -c $(COMPFLAGS) $<

%.cmx: %.ml
	$(V_OCAMLOPT)$(OCAMLOPT) -c $(COMPFLAGS) $(OPTCOMPFLAGS) $<
