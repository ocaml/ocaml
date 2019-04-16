#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Makefile for the dynamic link library

# FIXME reduce redundancy by including ../Makefile

ROOTDIR = ../..

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun

OCAMLC    = $(CAMLRUN) $(ROOTDIR)/ocamlc -nostdlib -I $(ROOTDIR)/stdlib
OCAMLOPT  = $(CAMLRUN) $(ROOTDIR)/ocamlopt -nostdlib -I $(ROOTDIR)/stdlib

INCLUDES=$(addprefix -I $(ROOTDIR)/,utils typing bytecomp asmcomp)
COMPFLAGS=$(INCLUDES) -absname -w +a-4-9-40-41-42-44-45-48 -bin-annot -g \
   -I $(ROOTDIR)/stdlib -warn-error A \
   -safe-string -strict-sequence -strict-formats
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS=-O3
else
OPTCOMPFLAGS=
endif

OBJS=dynlink_compilerlibs.cmo dynlink_types.cmo \
  dynlink_platform_intf.cmo dynlink_common.cmo dynlink.cmo
NATOBJS=dynlink_types.cmx dynlink_platform_intf.cmx \
  dynlink_common.cmx dynlink.cmx

COMPILEROBJS = $(addprefix $(ROOTDIR)/,\
  utils/config.cmo utils/build_path_prefix_map.cmo utils/misc.cmo \
  utils/identifiable.cmo utils/numbers.cmo utils/arg_helper.cmo \
  utils/clflags.cmo utils/consistbl.cmo \
  utils/terminfo.cmo utils/warnings.cmo utils/load_path.cmo \
  parsing/location.cmo parsing/longident.cmo \
  parsing/docstrings.cmo parsing/syntaxerr.cmo \
  parsing/ast_helper.cmo parsing/ast_mapper.cmo parsing/ast_iterator.cmo \
  parsing/attr_helper.cmo parsing/builtin_attributes.cmo \
  typing/ident.cmo typing/path.cmo typing/primitive.cmo typing/types.cmo \
  typing/btype.cmo typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/cmi_format.cmo typing/env.cmo \
  bytecomp/lambda.cmo bytecomp/instruct.cmo \
  bytecomp/opcodes.cmo bytecomp/runtimedef.cmo bytecomp/bytesections.cmo \
  bytecomp/dll.cmo bytecomp/meta.cmo bytecomp/symtable.cmo)

COMPILERINTFS = $(addprefix $(ROOTDIR)/,\
  bytecomp/cmo_format.cmi)

all: dynlink.cma extract_crc

allopt: dynlink.cmxa

dynlink.cma: $(OBJS)
	$(OCAMLC) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a -o $@ $^

dynlink.cmxa: $(NATOBJS)
	$(OCAMLOPT) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a -o $@ $^

dynlink_compilerlibs.cmo: $(COMPILEROBJS) $(COMPILERINTFS)
	$(OCAMLC) $(COMPFLAGS) -pack -o $@ $^

# This rule is ok since there is no corresponding rule for native code
# compilation (otherwise we would fall foul of the problem in the next
# comment).
dynlink_compilerlibs.cmi: dynlink_compilerlibs.cmo

# Since there is no .mli for [Dynlink_platform_intf], we need to be
# careful that compilation of the .cmx file does not write the .cmi file again,
# which would cause rebuilding of ocamlopt.  The easiest way to do this seems
# to be to copy the .ml file, which is a valid .mli, to the .mli.
dynlink_platform_intf.mli: dynlink_platform_intf.ml
	cp $< $@
dynlink_platform_intf.cmi: dynlink_platform_intf.mli \
    dynlink_types.cmi
	$(OCAMLC) $(COMPFLAGS) -c $<

dynlink_platform_intf.cmo: dynlink_platform_intf.ml \
    dynlink_platform_intf.cmi \
    dynlink_types.cmo
	$(OCAMLC) $(COMPFLAGS) -c dynlink_platform_intf.ml

dynlink_platform_intf.cmx: dynlink_platform_intf.ml \
    dynlink_platform_intf.cmi \
    dynlink_types.cmx
	$(OCAMLOPT) $(COMPFLAGS) -c dynlink_platform_intf.ml

dynlink_types.cmi: dynlink_types.mli
	$(OCAMLC) $(COMPFLAGS) -c dynlink_types.mli

dynlink_types.cmo: dynlink_types.ml dynlink_types.cmi
	$(OCAMLC) $(COMPFLAGS) -c dynlink_types.ml

dynlink_types.cmx: dynlink_types.ml dynlink_types.cmi
	$(OCAMLOPT) $(COMPFLAGS) -c dynlink_types.ml

dynlink_common.cmi: dynlink_common.mli \
    dynlink_platform_intf.cmi \
    dynlink_types.cmi
	$(OCAMLC) $(COMPFLAGS) -c dynlink_common.mli

dynlink_common.cmo: dynlink_common.ml \
    dynlink_common.cmi \
    dynlink_platform_intf.cmo
	$(OCAMLC) $(COMPFLAGS) -c dynlink_common.ml

dynlink_common.cmx: dynlink_common.ml \
    dynlink_common.cmi \
    dynlink_platform_intf.cmx
	$(OCAMLOPT) $(COMPFLAGS) -c dynlink_common.ml

dynlink.cmi: dynlink.mli dynlink_compilerlibs.cmi
	$(OCAMLC) -c $(COMPFLAGS) dynlink.mli

dynlink.cmo: dynlink.cmi dynlink_common.cmi \
      dynlink_types.cmo dynlink_common.cmo dynlink.ml \
      dynlink_compilerlibs.cmo
	$(OCAMLC) -c $(COMPFLAGS) -impl dynlink.ml

dynlink.cmx: dynlink.cmi dynlink_common.cmi \
      dynlink_types.cmx dynlink_common.cmx natdynlink.ml
	cp natdynlink.ml dynlink.mlopt
	$(OCAMLOPT) -c $(COMPFLAGS) -impl dynlink.mlopt
	rm -f dynlink.mlopt

extract_crc.cmo: extract_crc.ml dynlink.cmi
	$(OCAMLC) -c $(COMPFLAGS) extract_crc.ml

extract_crc: $(COMPILEROBJS) dynlink.cma extract_crc.cmo
	$(OCAMLC) -o $@ $^

install:
	$(INSTALL_DATA) \
	  dynlink.cmi dynlink.cma \
	  "$(INSTALL_LIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  dynlink.cmti dynlink.mli \
	  "$(INSTALL_LIBDIR)"
endif
	$(INSTALL_PROG) \
	  extract_crc "$(INSTALL_LIBDIR)/extract_crc$(EXE)"

installopt:
	if $(NATDYNLINK); then \
	  $(INSTALL_DATA) \
	    $(NATOBJS) dynlink.cmxa dynlink.$(A) \
	    "$(INSTALL_LIBDIR)" && \
	  cd "$(INSTALL_LIBDIR)" && $(RANLIB) dynlink.$(A); \
	fi

partialclean:
	rm -f extract_crc *.cm[ioaxt] *.cmti *.cmxa

clean: partialclean
	rm -f *.$(A) *.$(O) *.so *.dll dynlink.mlopt \
	      dynlink_platform_intf.mli

depend:
