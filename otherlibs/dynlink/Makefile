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

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun

OCAMLC    = $(CAMLRUN) $(ROOTDIR)/ocamlc -nostdlib -I $(ROOTDIR)/stdlib
OCAMLOPT  = $(CAMLRUN) $(ROOTDIR)/ocamlopt -nostdlib -I $(ROOTDIR)/stdlib

# COMPFLAGS should be in sync with the toplevel Makefile's COMPFLAGS.
COMPFLAGS=-strict-sequence -principal -absname -w +a-4-9-41-42-44-45-48-66 \
	  -warn-error A \
          -bin-annot -safe-string -strict-formats
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS=-O3
else
OPTCOMPFLAGS=
endif

LOCAL_SRC=dynlink_compilerlibs

OBJS=dynlink_compilerlibs_bytecode/dynlink_compilerlibs.cmo \
  dynlink_types.cmo dynlink_platform_intf.cmo dynlink_common.cmo dynlink.cmo

NATOBJS=dynlink_compilerlibs_native/dynlink_compilerlibs.cmx \
  dynlink_types.cmx dynlink_platform_intf.cmx dynlink_common.cmx dynlink.cmx

# We need/desire access to compilerlibs for various reasons:
# - The bytecode dynamic linker is in compilerlibs and has many dependencies
#   from there.
# - It stops duplication of code (e.g. magic numbers from [Config]).
# - It allows future improvement by re-using various types.
# We have to pack our own version of compilerlibs (even if compilerlibs
# becomes packed in the future by default) otherwise problems will be caused
# if a user tries to link dynlink.cm{x,}a with code either having modules
# of the same names or code that is already linked against compilerlibs.
#
# The modules needed from compilerlibs have to be recompiled so that the
# -for-pack option can be specified.  Packing without such option having been
# specified, as used to be performed in this Makefile, is currently permitted
# for bytecode (but may be disallowed in the future) but not native.

# .mli files from compilerlibs that don't have a corresponding .ml file.
COMPILERLIBS_INTFS=\
  parsing/asttypes.mli \
  parsing/parsetree.mli \
  typing/outcometree.mli \
  bytecomp/cmo_format.mli \
  asmcomp/cmxs_format.mli

# .ml files from compilerlibs that have corresponding .mli files.
COMPILERLIBS_SOURCES=\
  utils/config.ml \
  utils/build_path_prefix_map.ml \
  utils/misc.ml \
  utils/identifiable.ml \
  utils/numbers.ml \
  utils/arg_helper.ml \
  utils/clflags.ml \
  utils/profile.ml \
  utils/consistbl.ml \
  utils/terminfo.ml \
  utils/warnings.ml \
  utils/load_path.ml \
  parsing/location.ml \
  parsing/longident.ml \
  parsing/docstrings.ml \
  parsing/syntaxerr.ml \
  parsing/ast_helper.ml \
  parsing/ast_mapper.ml \
  parsing/attr_helper.ml \
  parsing/builtin_attributes.ml \
  parsing/pprintast.ml \
  typing/ident.ml \
  typing/path.ml \
  typing/primitive.ml \
  typing/types.ml \
  typing/btype.ml \
  typing/subst.ml \
  typing/predef.ml \
  typing/datarepr.ml \
  typing/cmi_format.ml \
  typing/persistent_env.ml \
  typing/env.ml \
  bytecomp/lambda.ml \
  bytecomp/instruct.ml \
  bytecomp/opcodes.ml \
  bytecomp/runtimedef.ml \
  bytecomp/bytesections.ml \
  bytecomp/dll.ml \
  bytecomp/meta.ml \
  bytecomp/symtable.ml

# Rules to make a local copy of the .ml and .mli files required.  We also
# provide .ml files for .mli-only modules---without this, such modules do
# not seem to be located by the type checker inside bytecode packs.

dynlink_compilerlibs/Makefile.copy-sources:
	mkdir -p dynlink_compilerlibs
	mkdir -p dynlink_compilerlibs_bytecode
	mkdir -p dynlink_compilerlibs_native
	echo > $@
	for ml in $(COMPILERLIBS_SOURCES); do \
          echo "dynlink_compilerlibs/$$(basename $$ml): $(ROOTDIR)/$$ml" \
            >> $@; \
          echo "dynlink_compilerlibs/$$(basename $$ml)i: $(ROOTDIR)/$${ml}i" \
            >> $@; \
        done;
	for mli in $(COMPILERLIBS_INTFS); do \
          echo "dynlink_compilerlibs/$$(basename $$mli): $(ROOTDIR)/$$mli" \
            >> $@; \
          echo \
            "dynlink_compilerlibs/$$(basename $$mli .mli).ml: $(ROOTDIR)/$$mli"\
            >> $@; \
        done

-include dynlink_compilerlibs/Makefile.copy-sources

dynlink_compilerlibs/%.ml:
	cp $< $@

dynlink_compilerlibs/%.mli:
	cp $< $@

# Rules to automatically generate dependencies for the local copy of the
# compilerlibs sources.

COMPILERLIBS_SOURCES_NO_DIRS=$(notdir $(COMPILERLIBS_SOURCES))

COMPILERLIBS_INTFS_NO_DIRS=$(notdir $(COMPILERLIBS_INTFS))

COMPILERLIBS_INTFS_BASE_NAMES=$(basename $(COMPILERLIBS_INTFS_NO_DIRS))

COMPILERLIBS_INTFS_ML_NO_DIRS=$(addsuffix .ml, $(COMPILERLIBS_INTFS_BASE_NAMES))

COMPILERLIBS_COPIED_INTFS=\
  $(addprefix dynlink_compilerlibs/, $(COMPILERLIBS_INTFS_ML_NO_DIRS))

COMPILERLIBS_COPIED_SOURCES=\
  $(addprefix dynlink_compilerlibs/, $(COMPILERLIBS_SOURCES_NO_DIRS)) \
  $(COMPILERLIBS_COPIED_INTFS)

COMPILERLIBS_SOURCES_INTFS=\
  $(addsuffix i, $(COMPILERLIBS_SOURCES))

COMPILERLIBS_COPIED_SOURCES_INTFS=\
  $(addsuffix i, $(COMPILERLIBS_COPIED_SOURCES))

dynlink_compilerlibs/.depend: $(COMPILERLIBS_COPIED_SOURCES) \
  $(COMPILERLIBS_COPIED_SOURCES_INTFS)
	$(CAMLRUN) $(ROOTDIR)/ocamlc -depend -slash -I dynlink_compilerlibs \
	$(COMPILERLIBS_COPIED_SOURCES) \
	$(COMPILERLIBS_COPIED_SOURCES_INTFS) \
        > dynlink_compilerlibs/.depend

-include dynlink_compilerlibs/.depend

# Rules to build the local copy of the compilerlibs sources in such a way
# that the resulting .cm{o,x} files can be packed.

COMPILERLIBS_CMO=$(COMPILERLIBS_COPIED_SOURCES:.ml=.cmo)
COMPILERLIBS_CMX=$(COMPILERLIBS_COPIED_SOURCES:.ml=.cmx)

dynlink_compilerlibs/%.cmi: dynlink_compilerlibs/%.mli
	$(OCAMLC) -c -for-pack Dynlink_compilerlibs $(COMPFLAGS) \
          -I dynlink_compilerlibs -o $@ $<

dynlink_compilerlibs/%.cmo: dynlink_compilerlibs/%.ml \
  dynlink_compilerlibs/%.cmi
	$(OCAMLC) -c -for-pack Dynlink_compilerlibs $(COMPFLAGS) \
          -I dynlink_compilerlibs -o $@ $<

dynlink_compilerlibs/%.cmx: dynlink_compilerlibs/%.ml \
  dynlink_compilerlibs/%.cmi
	$(OCAMLOPT) -c -for-pack Dynlink_compilerlibs $(COMPFLAGS) \
          $(OPTCOMPFLAGS) -I dynlink_compilerlibs -o $@ $<

# Rules for building the [Dynlink_compilerlibs] bytecode and native packs
# from their components.

dynlink_compilerlibs_bytecode/dynlink_compilerlibs.cmo: $(COMPILERLIBS_CMO)
	$(OCAMLC) $(COMPFLAGS) -pack -o $@ $(COMPILERLIBS_CMO)

dynlink_compilerlibs_native/dynlink_compilerlibs.cmx: $(COMPILERLIBS_CMX)
	$(OCAMLOPT) $(COMPFLAGS) $(OPTCOMPFLAGS) -pack -o $@ $(COMPILERLIBS_CMX)

# Rules for building the interface of the [Dynlink_compilerlibs] packs.
# To avoid falling foul of the problem described below, the .cmo and .cmx
# files for the dynlink-specific compilerlibs packs generated here---and in
# particular the corresponding .cmi files -- are kept in separate directories.

dynlink_compilerlibs_bytecode/dynlink_compilerlibs.cmi: \
  dynlink_compilerlibs_bytecode/dynlink_compilerlibs.cmo

dynlink_compilerlibs.cmi: \
  dynlink_compilerlibs_bytecode/dynlink_compilerlibs.cmi
	cp -a $< $@

# The main dynlink rules start here.

all: dynlink.cma extract_crc

allopt: dynlink.cmxa

dynlink.cma: $(OBJS)
	$(OCAMLC) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a \
          -I dynlink_compilerlibs_bytecode -o $@ $^

dynlink.cmxa: $(NATOBJS)
	$(OCAMLOPT) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a \
          -I dynlink_compilerlibs_native -o $@ $^

# Since there is no .mli for [Dynlink_platform_intf], we need to be
# careful that compilation of the .cmx file does not write the .cmi file again,
# which would cause rebuilding of ocamlopt.  The easiest way to do this seems
# to be to copy the .ml file, which is a valid .mli, to the .mli.
dynlink_platform_intf.mli: dynlink_platform_intf.ml
	cp $< $@
dynlink_platform_intf.cmi: dynlink_platform_intf.mli \
    dynlink_types.cmi
	$(OCAMLC) $(COMPFLAGS) -I dynlink_compilerlibs_bytecode -c $<

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
    dynlink_compilerlibs.cmi \
    dynlink_platform_intf.cmo
	$(OCAMLC) $(COMPFLAGS) -c dynlink_common.ml

dynlink_common.cmx: dynlink_common.ml \
    dynlink_common.cmi \
    dynlink_compilerlibs.cmi \
    dynlink_platform_intf.cmx \
    dynlink_compilerlibs_native/dynlink_compilerlibs.cmx
	$(OCAMLOPT) $(COMPFLAGS) -I dynlink_compilerlibs_native/ \
          -c dynlink_common.ml

dynlink.cmi: dynlink.mli dynlink_compilerlibs.cmi
	$(OCAMLC) -c $(COMPFLAGS) dynlink.mli

dynlink.cmo: dynlink.cmi dynlink_common.cmi \
      dynlink_types.cmo dynlink_common.cmo dynlink.ml \
      dynlink_compilerlibs.cmi
	$(OCAMLC) -c $(COMPFLAGS) dynlink.ml

dynlink.cmx: dynlink.cmi dynlink_common.cmi \
      dynlink_types.cmx dynlink_common.cmx natdynlink.ml \
      dynlink_compilerlibs.cmi \
      dynlink_compilerlibs_native/dynlink_compilerlibs.cmx
	cp natdynlink.ml dynlink.mlopt
	$(OCAMLOPT) -c $(COMPFLAGS) -I dynlink_compilerlibs_native/ \
          -impl dynlink.mlopt
	rm -f dynlink.mlopt

extract_crc.cmo: extract_crc.ml dynlink.cmi dynlink_compilerlibs.cmi
	$(OCAMLC) -c $(COMPFLAGS) -I ../utils extract_crc.ml

extract_crc: $(COMPILEROBJS) dynlink.cma \
  dynlink_compilerlibs_bytecode/dynlink_compilerlibs.cmo \
  extract_crc.cmo
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
