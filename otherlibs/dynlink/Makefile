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

COMPFLAGS += -I byte
OPTCOMPFLAGS += -I native

LOCAL_SRC=dynlink_compilerlibs

OBJS=byte/dynlink_compilerlibs.cmo dynlink_types.cmo \
  dynlink_platform_intf.cmo dynlink_common.cmo byte/dynlink.cmo

NATOBJS=native/dynlink_compilerlibs.cmx dynlink_types.cmx \
  dynlink_platform_intf.cmx dynlink_common.cmx native/dynlink.cmx

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
  file_formats/cmo_format.mli \
  file_formats/cmxs_format.mli

# .ml files from compilerlibs that have corresponding .mli files.
COMPILERLIBS_SOURCES=\
  utils/binutils.ml \
  utils/config.ml \
  utils/build_path_prefix_map.ml \
  utils/misc.ml \
  utils/identifiable.ml \
  utils/numbers.ml \
  utils/arg_helper.ml \
  utils/local_store.ml \
  utils/load_path.ml \
  utils/clflags.ml \
  utils/profile.ml \
  utils/consistbl.ml \
  utils/terminfo.ml \
  utils/warnings.ml \
  utils/int_replace_polymorphic_compare.ml \
  utils/lazy_backtrack.ml \
  parsing/location.ml \
  parsing/longident.ml \
  parsing/docstrings.ml \
  parsing/syntaxerr.ml \
  parsing/ast_helper.ml \
  parsing/ast_mapper.ml \
  parsing/attr_helper.ml \
  parsing/builtin_attributes.ml \
  typing/ident.ml \
  typing/path.ml \
  typing/primitive.ml \
  typing/type_immediacy.ml \
  typing/shape.ml \
  typing/types.ml \
  typing/btype.ml \
  typing/subst.ml \
  typing/predef.ml \
  typing/datarepr.ml \
  file_formats/cmi_format.ml \
  typing/persistent_env.ml \
  typing/env.ml \
  lambda/debuginfo.ml \
  lambda/lambda.ml \
  lambda/runtimedef.ml \
  bytecomp/instruct.ml \
  bytecomp/opcodes.ml \
  bytecomp/bytesections.ml \
  bytecomp/dll.ml \
  bytecomp/meta.ml \
  bytecomp/symtable.ml

# Rules to make a local copy of the .ml and .mli files required.  We also
# provide .ml files for .mli-only modules---without this, such modules do
# not seem to be located by the type checker inside bytecode packs.
# Note: .ml-only modules are not supported by the (.mli.cmi) rule below.

$(LOCAL_SRC)/Makefile: $(LOCAL_SRC)/Makefile.copy-sources Makefile
	cp -f $< $@
	for ml in $(COMPILERLIBS_SOURCES); do \
          echo "$(LOCAL_SRC)/$$(basename $$ml): $(ROOTDIR)/$$ml" \
            >> $@; \
          echo "$(LOCAL_SRC)/$$(basename $$ml)i: $(ROOTDIR)/$${ml}i" \
            >> $@; \
        done;
	for mli in $(COMPILERLIBS_INTFS); do \
          echo "$(LOCAL_SRC)/$$(basename $$mli): $(ROOTDIR)/$$mli" \
            >> $@; \
          echo \
            "$(LOCAL_SRC)/$$(basename $$mli .mli).ml: $(ROOTDIR)/$$mli"\
            >> $@; \
        done

# Rules to automatically generate dependencies for the local copy of the
# compilerlibs sources.

COMPILERLIBS_SOURCES_NO_DIRS=$(notdir $(COMPILERLIBS_SOURCES))

COMPILERLIBS_INTFS_NO_DIRS=$(notdir $(COMPILERLIBS_INTFS))

COMPILERLIBS_INTFS_BASE_NAMES=$(basename $(COMPILERLIBS_INTFS_NO_DIRS))

COMPILERLIBS_INTFS_ML_NO_DIRS=$(addsuffix .ml, $(COMPILERLIBS_INTFS_BASE_NAMES))

COMPILERLIBS_COPIED_INTFS=\
  $(addprefix $(LOCAL_SRC)/, $(COMPILERLIBS_INTFS_ML_NO_DIRS))

COMPILERLIBS_COPIED_SOURCES=\
  $(addprefix $(LOCAL_SRC)/, $(COMPILERLIBS_SOURCES_NO_DIRS)) \
  $(COMPILERLIBS_COPIED_INTFS)

COMPILERLIBS_SOURCES_INTFS=\
  $(addsuffix i, $(COMPILERLIBS_SOURCES))

COMPILERLIBS_COPIED_SOURCES_INTFS=\
  $(addsuffix i, $(COMPILERLIBS_COPIED_SOURCES))

# $(LOCAL_SRC)/Makefile uses the variables above in dependencies, so must be
# include'd after they've been defined.
-include $(LOCAL_SRC)/Makefile

# Rules to build the local copy of the compilerlibs sources in such a way
# that the resulting .cm{o,x} files can be packed.

COMPILERLIBS_CMO=$(COMPILERLIBS_COPIED_SOURCES:.ml=.cmo)
COMPILERLIBS_CMX=$(COMPILERLIBS_COPIED_SOURCES:.ml=.cmx)

$(LOCAL_SRC)/%.cmi: $(LOCAL_SRC)/%.mli
	$(OCAMLC) -c -for-pack Dynlink_compilerlibs $(COMPFLAGS) \
          -I $(LOCAL_SRC) -o $@ $(LOCAL_SRC)/$*.mli

$(LOCAL_SRC)/%.cmo: $(LOCAL_SRC)/%.ml
	$(OCAMLC) -c -for-pack Dynlink_compilerlibs $(COMPFLAGS) \
          -I $(LOCAL_SRC) -o $@ $(LOCAL_SRC)/$*.ml

$(LOCAL_SRC)/%.cmx: $(LOCAL_SRC)/%.ml
	$(OCAMLOPT) -c -for-pack Dynlink_compilerlibs $(COMPFLAGS) \
          $(OPTCOMPFLAGS) -I $(LOCAL_SRC) -o $@ $(LOCAL_SRC)/$*.ml

# Rules for building the [Dynlink_compilerlibs] bytecode and native packs
# from their components.

# A multi-target pattern rule can tell GNU make that the recipe simultaneously
# produces both the cmo and cmi file for Dynlink_compilerlibs. GNU make 4.3+
# would be required to do the same thing with a static rule (with its grouped
# targets feature).
byt%/dynlink_compilerlibs.cmo byt%/dynlink_compilerlibs.cmi: $(COMPILERLIBS_CMO)
	@$(if $(filter-out e,$*),\
        $(error Should only build byte/dynlink_compilerlibs.cmo!))
	$(OCAMLC) $(COMPFLAGS) -pack -o byte/dynlink_compilerlibs.cmo $^

native/dynlink_compilerlibs.cmx: $(COMPILERLIBS_CMX)
	$(OCAMLOPT) $(COMPFLAGS) $(OPTCOMPFLAGS) -pack -o $@ $(COMPILERLIBS_CMX)

%/dynlink.cmi: dynlink.cmi dynlink.mli
	cp $^ $*/

# Rules for building the interface of the [Dynlink_compilerlibs] packs.
# To avoid falling foul of the problem described below, the .cmo and .cmx
# files for the dynlink-specific compilerlibs packs generated here---and in
# particular the corresponding .cmi files -- are kept in separate directories.

# The main dynlink rules start here.

all: dynlink.cma

allopt: dynlink.cmxa

dynlink.cma: $(OBJS)
	$(OCAMLC) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a -I byte -o $@ $^

dynlink.cmxa: $(NATOBJS)
	$(OCAMLOPT) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a -I native \
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
	      native/*.cm[ixt] native/*.cmti native/*.o native/*.obj \
	      $(LOCAL_SRC)/*.cm[ioaxt] $(LOCAL_SRC)/*.cmti \
        $(LOCAL_SRC)/*.o $(LOCAL_SRC)/*.obj

clean: partialclean
	rm -f *.a *.lib *.o *.obj *.so *.dll dynlink_platform_intf.mli \
	      $(LOCAL_SRC)/*.ml $(LOCAL_SRC)/*.mli $(LOCAL_SRC)/Makefile \
	      $(LOCAL_SRC)/.depend byte/dynlink.mli native/dynlink.mli

.PHONY: distclean
distclean: clean
	rm -f META

.PHONY: beforedepend
beforedepend: dynlink_platform_intf.mli

.PHONY: depend
DEPEND_DUMMY_FILES=\
  native/dynlink_compilerlibs.ml \
  byte/dynlink_compilerlibs.mli \
  byte/dynlink.mli \
  native/dynlink.mli

depend: beforedepend
	touch $(DEPEND_DUMMY_FILES)
	$(OCAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash \
	  -I byte -bytecode *.mli *.ml byte/dynlink.ml > .depend
	$(OCAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash \
	  -I native -native *.ml native/dynlink.ml >> .depend
	rm -f $(DEPEND_DUMMY_FILES)

include .depend

%.cmi: %.mli
	$(OCAMLC) -c $(COMPFLAGS) $<

%.cmo: %.ml
	$(OCAMLC) -c $(COMPFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(COMPFLAGS) $(OPTCOMPFLAGS) $<
