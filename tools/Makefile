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

MAKEFLAGS := -r -R
ROOTDIR = ..

-include $(ROOTDIR)/Makefile.config
-include $(ROOTDIR)/Makefile.common

ifeq ($(SYSTEM),unix)
override define shellquote
$i := $$(subst ",\",$$(subst $$$$,\$$$$,$$(subst `,\`,$i)))#")#
endef
$(foreach i,BINDIR LIBDIR STUBLIBDIR MANDIR,$(eval $(shellquote)))
endif

DESTDIR ?=
# Setup GNU make variables storing per-target source and target,
# a list of installed tools, and a function to quote a filename for
# the shell.
override installed_tools := ocamldep ocamlprof ocamlcp \
                   ocamlmktop ocamlmklib ocamlobjinfo

install_files :=
define byte2native
$(patsubst %.cmo,%.cmx,$(patsubst %.cma,%.cmxa,$1))
endef

# $1 = target, $2 = OCaml object dependencies, $3 = other dependencies
# There is a lot of subtle code here.  The multiple layers of expansion
# are due to `make`'s eval() function, which evaluates the string
# passed to it as a makefile fragment.  So it is crucial that variables
# not get expanded too many times.
define byte_and_opt_
# This check is defensive programming
$(and $(filter-out 1,$(words $1)),$(error \
   cannot build file with whitespace in name))
$1: $3 $2
	$$(CAMLC) $$(LINKFLAGS) -I $$(ROOTDIR) -o $$@ $2

$1.opt: $3 $$(call byte2native,$2)
	$$(CAMLOPT_CMD) $$(LINKFLAGS) -I $$(ROOTDIR) -o $$@ \
	                $$(call byte2native,$2)

all: $1

opt.opt: $1.opt

ifeq '$(filter $(installed_tools),$1)' '$1'
install_files += $1
endif
clean::
	rm -f -- $1 $1.opt

endef

# Escape any $ characters in the arguments and eval the result.
define byte_and_opt
$(eval $(call \
 byte_and_opt_,$(subst $$,$$$$,$1),$(subst $$,$$$$,$2),$(subst $$,$$$$,$3)))
endef

CAMLC = $(BOOT_OCAMLC) -g -nostdlib -I $(ROOTDIR)/boot \
        -use-prims $(ROOTDIR)/runtime/primitives -I $(ROOTDIR)
CAMLOPT = $(CAMLRUN) $(ROOTDIR)/ocamlopt -g -nostdlib -I $(ROOTDIR)/stdlib
CAMLLEX = $(CAMLRUN) $(ROOTDIR)/boot/ocamllex
INCLUDES = $(addprefix -I $(ROOTDIR)/,utils parsing typing bytecomp \
                       middle_end middle_end/closure middle_end/flambda \
                       middle_end/flambda/base_types driver toplevel \
                       file_formats lambda)
COMPFLAGS = -absname -w +a-4-9-41-42-44-45-48 -strict-sequence -warn-error A \
 -principal -safe-string -strict-formats -bin-annot $(INCLUDES)
LINKFLAGS = $(INCLUDES)
VPATH := $(filter-out -I,$(INCLUDES))

.PHONY: all allopt opt.opt # allopt and opt.opt are synonyms
allopt: opt.opt

# The dependency generator

CAMLDEP_OBJ=ocamldep.cmo
CAMLDEP_IMPORTS= \
  $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
  $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma
ocamldep: LINKFLAGS += -compat-32
$(call byte_and_opt,ocamldep,$(CAMLDEP_IMPORTS) $(CAMLDEP_OBJ),)
ocamldep: depend.cmi
ocamldep.opt: depend.cmi

# ocamldep is precious: sometimes we are stuck in the middle of a
# bootstrap and we need to remake the dependencies
clean::
	if test -f ocamldep; then mv -f ocamldep ocamldep.bak; else :; fi
	rm -f ocamldep.opt


# The profiler

CSLPROF=ocamlprof.cmo
CSLPROF_IMPORTS=config.cmo build_path_prefix_map.cmo misc.cmo identifiable.cmo \
  numbers.cmo arg_helper.cmo clflags.cmo terminfo.cmo \
  warnings.cmo location.cmo longident.cmo docstrings.cmo \
  syntaxerr.cmo ast_helper.cmo \
  camlinternalMenhirLib.cmo parser.cmo \
  pprintast.cmo \
  lexer.cmo parse.cmo

$(call byte_and_opt,ocamlprof,$(CSLPROF_IMPORTS) profiling.cmo $(CSLPROF),)

ocamlcp_cmos = config.cmo build_path_prefix_map.cmo misc.cmo profile.cmo \
               warnings.cmo identifiable.cmo numbers.cmo arg_helper.cmo \
               clflags.cmo \
               terminfo.cmo location.cmo load_path.cmo ccomp.cmo compenv.cmo \
               main_args.cmo

$(call byte_and_opt,ocamlcp,$(ocamlcp_cmos) ocamlcp.cmo,)

opt:: profiling.cmx

install::
	$(INSTALL_DATA) \
	  profiling.cmi profiling.cmo \
	  "$(INSTALL_LIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  profiling.cmt profiling.cmti \
	  "$(INSTALL_LIBDIR)"
endif

installopt::
	$(INSTALL_DATA) \
          profiling.cmx profiling.$(O) \
	  "$(INSTALL_LIBDIR)"

# To help building mixed-mode libraries (OCaml + C)

$(call byte_and_opt,ocamlmklib,ocamlmklibconfig.cmo config.cmo \
	         build_path_prefix_map.cmo misc.cmo ocamlmklib.cmo,)


ocamlmklibconfig.ml: $(ROOTDIR)/Makefile.config Makefile
	(echo 'let bindir = "$(BINDIR)"'; \
         echo 'let supports_shared_libraries = $(SUPPORTS_SHARED_LIBRARIES)';\
         echo 'let default_rpath = "$(RPATH)"'; \
         echo 'let mksharedlibrpath = "$(MKSHAREDLIBRPATH)"'; \
         echo 'let toolpref = "$(TOOLPREF)"';) \
        > ocamlmklibconfig.ml

beforedepend:: ocamlmklibconfig.ml

clean::
	rm -f ocamlmklibconfig.ml

# To make custom toplevels

OCAMLMKTOP=ocamlmktop.cmo
OCAMLMKTOP_IMPORTS=config.cmo build_path_prefix_map.cmo misc.cmo \
       identifiable.cmo numbers.cmo arg_helper.cmo clflags.cmo \
       load_path.cmo profile.cmo ccomp.cmo

$(call byte_and_opt,ocamlmktop,$(OCAMLMKTOP_IMPORTS) $(OCAMLMKTOP),)

# Converter olabl/ocaml 2.99 to ocaml 3

LIBRARY3=config.cmo build_path_prefix_map.cmo misc.cmo warnings.cmo location.cmo

ifeq ($(UNIX_OR_WIN32),unix)
LN := ln -sf
else
LN := cp -pf
endif

install::
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	for i in $(install_files); \
	do \
	  $(INSTALL_PROG) "$$i" "$(INSTALL_BINDIR)/$$i.byte$(EXE)"; \
	  if test -f "$$i".opt; then \
	    $(INSTALL_PROG) "$$i.opt" "$(INSTALL_BINDIR)/$$i.opt$(EXE)" && \
	    (cd "$(INSTALL_BINDIR)/" && $(LN) "$$i.opt$(EXE)" "$$i$(EXE)"); \
	  else \
	    (cd "$(INSTALL_BINDIR)/" && $(LN) "$$i.byte$(EXE)" "$$i$(EXE)"); \
	  fi; \
	done
else
	for i in $(install_files); \
	do \
	  if test -f "$$i".opt; then \
	    $(INSTALL_PROG) "$$i.opt" "$(INSTALL_BINDIR)/$$i.opt$(EXE)"; \
	    (cd "$(INSTALL_BINDIR)/" && $(LN) "$$i.opt$(EXE)" "$$i$(EXE)"); \
	  fi; \
	done
endif

# The preprocessor for asm generators

CVT_EMIT=cvt_emit.cmo

cvt_emit: $(CVT_EMIT)
	$(CAMLC) $(LINKFLAGS) -o cvt_emit $(CVT_EMIT)

# cvt_emit is precious: sometimes we are stuck in the middle of a
# bootstrap and we need to remake the dependencies
.PRECIOUS: cvt_emit
clean::
	if test -f cvt_emit; then mv -f cvt_emit cvt_emit.bak; else :; fi

clean::
	rm -f cvt_emit.ml

beforedepend:: cvt_emit.ml

# Reading cmt files

READ_CMT= \
          $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
          $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
          \
          read_cmt.cmo

# Reading cmt files
$(call byte_and_opt,read_cmt,$(READ_CMT),)

install::
	if test -f read_cmt.opt; then \
	  $(INSTALL_PROG) read_cmt.opt "$(INSTALL_BINDIR)/ocamlcmt$(EXE)"; \
	else \
	  $(INSTALL_PROG) read_cmt "$(INSTALL_BINDIR)/ocamlcmt$(EXE)"; \
	fi


# The bytecode disassembler

DUMPOBJ= \
          $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
          $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
          \
          opnames.cmo dumpobj.cmo

$(call byte_and_opt,dumpobj,$(DUMPOBJ),)

make_opcodes: make_opcodes.ml
	$(CAMLC) make_opcodes.ml -o $@

opnames.ml: $(ROOTDIR)/runtime/caml/instruct.h make_opcodes
	$(ROOTDIR)/runtime/ocamlrun make_opcodes -opnames < $< > $@

clean::
	rm -f opnames.ml make_opcodes make_opcodes.ml

beforedepend:: opnames.ml

# Display info on compiled files

DEF_SYMBOL_PREFIX = '-Dsymbol_prefix=""'

ifeq "$(SYSTEM)" "macosx"
DEF_SYMBOL_PREFIX = '-Dsymbol_prefix="_"'
endif

ifeq "$(SYSTEM)" "cygwin"
DEF_SYMBOL_PREFIX = '-Dsymbol_prefix="_"'
endif

objinfo_helper$(EXE): objinfo_helper.$(O)
	$(CC) $(BFD_LDFLAGS) $(OC_CFLAGS) $(OUTPUTEXE)$@ $< $(BFD_LDLIBS)

objinfo_helper.$(O): $(ROOTDIR)/runtime/caml/s.h

objinfo_helper.$(O): \
  OC_CPPFLAGS += -I$(ROOTDIR)/runtime $(DEF_SYMBOL_PREFIX) $(BFD_CPPFLAGS)

OBJINFO=$(ROOTDIR)/compilerlibs/ocamlcommon.cma \
        $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
        $(ROOTDIR)/compilerlibs/ocamlmiddleend.cma \
        objinfo.cmo

$(call byte_and_opt,ocamlobjinfo,$(OBJINFO),objinfo_helper$(EXE))

install::
	$(INSTALL_PROG) \
	  objinfo_helper$(EXE) "$(INSTALL_LIBDIR)/objinfo_helper$(EXE)"

primreq=$(ROOTDIR)/compilerlibs/ocamlcommon.cma \
        $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
        primreq.cmo

# Scan object files for required primitives
$(call byte_and_opt,primreq,$(primreq),)

LINTAPIDIFF=$(ROOTDIR)/compilerlibs/ocamlcommon.cmxa \
        $(ROOTDIR)/compilerlibs/ocamlbytecomp.cmxa \
        $(ROOTDIR)/compilerlibs/ocamlmiddleend.cmxa \
	$(ROOTDIR)/otherlibs/str/str.cmxa \
	lintapidiff.cmx

lintapidiff.opt: INCLUDES+= -I $(ROOTDIR)/otherlibs/str
lintapidiff.opt: $(LINTAPIDIFF)
	$(CAMLOPT_CMD) $(LINKFLAGS) -I $(ROOTDIR) -o $@ $(LINTAPIDIFF)
clean::
	rm -f -- lintapidiff.opt lintapidiff.cm? lintapidiff.o lintapidiff.obj


clean::
	rm -f "objinfo_helper" "objinfo_helper.manifest"
	rm -f "objinfo_helper.exe" "objinfo_helper.exe.manifest"

# Eventlog metadata file

install::
	$(INSTALL_DATA) \
	  eventlog_metadata \
	  "$(INSTALL_LIBDIR)"

# Copy a bytecode executable, stripping debug info

stripdebug=$(ROOTDIR)/compilerlibs/ocamlcommon.cma \
           $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
           stripdebug.cmo

$(call byte_and_opt,stripdebug,$(stripdebug),)

# Compare two bytecode executables

CMPBYT=$(ROOTDIR)/compilerlibs/ocamlcommon.cma \
       $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
       cmpbyt.cmo

$(call byte_and_opt,cmpbyt,$(CMPBYT),)

CAMLTEX= $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
	$(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
	$(ROOTDIR)/compilerlibs/ocamltoplevel.cma \
	$(ROOTDIR)/otherlibs/str/str.cma \
	$(ROOTDIR)/otherlibs/$(UNIXLIB)/unix.cma \
	caml_tex.ml

#Scan latex files, and run ocaml code examples

caml-tex: INCLUDES += $(addprefix -I $(ROOTDIR)/otherlibs/,str $(UNIXLIB))
caml-tex: $(CAMLTEX)
	$(ROOTDIR)/runtime/ocamlrun $(ROOTDIR)/ocamlc -nostdlib \
	                            -I $(ROOTDIR)/stdlib $(LINKFLAGS) -linkall \
	                            -o $@ -no-alias-deps $(CAMLTEX)

# we need str and unix which depend on the bytecode version of other tools
# thus we delay building caml-tex to the opt.opt stage
ifneq "$(WITH_CAMLTEX)" ""
opt.opt:caml-tex
endif
clean::
	rm -f -- caml-tex caml_tex.cm?

# Common stuff

%.ml: %.mll
	$(CAMLLEX) $(OCAMLLEX_FLAGS) $<

%.cmo: %.ml
	$(CAMLC) -c $(COMPFLAGS) - $<

%.cmi: %.mli
	$(CAMLC) -c $(COMPFLAGS) - $<

%.cmx: %.ml
	$(CAMLOPT) $(COMPFLAGS) -c - $<

clean::
	rm -f *.cmo *.cmi *.cma *.dll *.so *.lib *.a

CAMLDEP=$(BOOT_OCAMLC) -depend
DEPFLAGS=-slash
DEPINCLUDES=$(INCLUDES)
depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) $(DEPINCLUDES) *.mli *.ml > .depend

.PHONY: clean install beforedepend depend

include .depend
