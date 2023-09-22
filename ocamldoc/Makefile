#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *
#*                                                                        *
#*   Copyright 2001 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

ROOTDIR = ..

include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

OCAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)
OCAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)

# For installation
##############

programs := ocamldoc ocamldoc.opt

OCAMLDOC_LIBCMA=odoc_info.cma
OCAMLDOC_LIBCMI=odoc_info.cmi
OCAMLDOC_LIBCMXA=odoc_info.cmxa
OCAMLDOC_LIBA=odoc_info.$(A)

OCAMLDOC_LIBMLIS=$(addsuffix .mli,\
  odoc_dep odoc_dot odoc_extension odoc_html odoc_info odoc_latex \
  odoc_latex_style odoc_man odoc_messages odoc_ocamlhtml odoc_parameter \
  odoc_texi odoc_text_lexer odoc_to_text odoc_type odoc_value)
OCAMLDOC_LIBCMIS=$(OCAMLDOC_LIBMLIS:.mli=.cmi)
OCAMLDOC_LIBCMTS=$(OCAMLDOC_LIBMLIS:.mli=.cmt) $(OCAMLDOC_LIBMLIS:.mli=.cmti)

ODOC_TEST=odoc_test.cmo

# Compilation
#############


INCLUDE_DIRS = $(addprefix $(ROOTDIR)/,\
  utils parsing typing driver bytecomp toplevel)
INCLUDES_DEP = $(addprefix -I ,$(INCLUDE_DIRS))
INCLUDES_NODEP = $(addprefix -I $(ROOTDIR)/,\
  compilerlibs otherlibs/str otherlibs/dynlink \
  otherlibs/dynlink/native otherlibs/unix)

OC_OCAMLDEPDIRS = $(INCLUDE_DIRS)
INCLUDES=$(INCLUDES_DEP) $(INCLUDES_NODEP)

COMPFLAGS = \
  -g $(INCLUDES) -absname -w +a-4-9-41-42-44-45-48 -warn-error +A \
  -strict-sequence -strict-formats -bin-annot -principal

LINKFLAGS = $(INCLUDES)

CMOFILES=\
  odoc_config.cmo \
  odoc_messages.cmo \
  odoc_global.cmo \
  odoc_types.cmo \
  odoc_misc.cmo \
  odoc_text_parser.cmo \
  odoc_text_lexer.cmo \
  odoc_text.cmo \
  odoc_name.cmo \
  odoc_parameter.cmo \
  odoc_value.cmo \
  odoc_type.cmo \
  odoc_extension.cmo \
  odoc_exception.cmo \
  odoc_class.cmo \
  odoc_module.cmo \
  odoc_print.cmo \
  odoc_str.cmo \
  odoc_comments_global.cmo \
  odoc_parser.cmo \
  odoc_lexer.cmo \
  odoc_see_lexer.cmo \
  odoc_env.cmo \
  odoc_merge.cmo \
  odoc_sig.cmo \
  odoc_ast.cmo \
  odoc_search.cmo \
  odoc_scan.cmo \
  odoc_cross.cmo \
  odoc_comments.cmo \
  odoc_dep.cmo \
  odoc_analyse.cmo \
  odoc_info.cmo

CMXFILES = $(CMOFILES:.cmo=.cmx)
CMIFILES = $(CMOFILES:.cmo=.cmi)

EXECMOFILES=\
  $(CMOFILES) \
  odoc_dag2html.cmo \
  odoc_to_text.cmo \
  odoc_ocamlhtml.cmo \
  odoc_html.cmo \
  odoc_man.cmo \
  odoc_latex_style.cmo \
  odoc_latex.cmo \
  odoc_texi.cmo \
  odoc_dot.cmo \
  odoc_gen.cmo \
  odoc_args.cmo \
  odoc.cmo

EXECMXFILES = $(EXECMOFILES:.cmo=.cmx)
EXECMIFILES = $(EXECMOFILES:.cmo=.cmi)

LIBCMOFILES = $(CMOFILES)
LIBCMXFILES = $(LIBCMOFILES:.cmo=.cmx)
LIBCMIFILES = $(LIBCMOFILES:.cmo=.cmi)

.PHONY: all
all: lib exe

.PHONY: exe
exe: $(OCAMLDOC)

.PHONY: lib
lib: $(OCAMLDOC_LIBCMA) $(OCAMLDOC_LIBCMI) $(ODOC_TEST)

.PHONY: opt.opt allopt # allopt and opt.opt are synonyms
opt.opt: exeopt libopt
allopt: opt.opt

.PHONY: exeopt
exeopt: $(OCAMLDOC_OPT)

.PHONY: libopt
libopt: $(OCAMLDOC_LIBCMXA) $(OCAMLDOC_LIBCMI)

OCAMLDOC_LIBRARIES = ocamlcommon unix str dynlink

OCAMLDOC_BCLIBRARIES = $(OCAMLDOC_LIBRARIES:%=%.cma)
OCAMLDOC_NCLIBRARIES = $(OCAMLDOC_LIBRARIES:%=%.cmxa)

$(eval $(call PROGRAM_SYNONYM,ocamldoc))

$(OCAMLDOC): $(EXECMOFILES)
	$(V_LINKC)$(OCAMLC) -o $@ -linkall $(LINKFLAGS) $(OCAMLDOC_BCLIBRARIES) $^

$(eval $(call PROGRAM_SYNONYM,ocamldoc.opt))

$(OCAMLDOC_OPT): $(EXECMXFILES)
	$(V_LINKOPT)$(OCAMLOPT) -o $@ -linkall $(LINKFLAGS) $(OCAMLDOC_NCLIBRARIES) $^

$(OCAMLDOC_LIBCMA): $(LIBCMOFILES)
	$(V_LINKC)$(OCAMLC) -a -o $@ $(LINKFLAGS) $^

$(OCAMLDOC_LIBCMXA): $(LIBCMXFILES)
	$(V_LINKOPT)$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $^

# Lexers and parsers

LEXERS = $(addsuffix .mll,\
  odoc_text_lexer odoc_lexer odoc_ocamlhtml odoc_see_lexer)

PARSERS = $(addsuffix .mly,odoc_parser  odoc_text_parser)

DEPEND_PREREQS = $(LEXERS:.mll=.ml) \
  $(PARSERS:.mly=.mli) $(PARSERS:.mly=.ml)

# generic rules :
#################

%.cmo: %.ml
	$(V_OCAMLC)$(OCAMLC) $(COMPFLAGS) -c $<

%.cmi: %.mli
	$(V_OCAMLC)$(OCAMLC) $(COMPFLAGS) -c $<

%.cmx: %.ml
	$(V_OCAMLOPT)$(OCAMLOPT) $(COMPFLAGS) -c $<

# Installation targets
######################

# TODO: it may be good to split the following rule in several ones, e.g.
# install-programs, install-doc, install-libs

.PHONY: install
install:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)/ocamldoc"
	$(INSTALL_PROG) $(OCAMLDOC) "$(INSTALL_BINDIR)"
	$(INSTALL_DATA) \
	  ocamldoc.hva *.cmi $(OCAMLDOC_LIBCMA) META \
	  "$(INSTALL_LIBDIR)/ocamldoc"
	$(INSTALL_DATA) \
	  $(OCAMLDOC_LIBCMIS) \
	  "$(INSTALL_LIBDIR)/ocamldoc"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  $(OCAMLDOC_LIBMLIS) $(OCAMLDOC_LIBCMTS) \
	  "$(INSTALL_LIBDIR)/ocamldoc"
endif

# Note: at the moment, $(INSTALL_MANODIR) is created even if the doc has
# not been built. This is not clean and should be changed.

.PHONY: installopt
installopt:
	if test -f $(OCAMLDOC_OPT); then $(MAKE) installopt_really ; fi

.PHONY: installopt_really
installopt_really:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)/ocamldoc"
	$(INSTALL_PROG) $(OCAMLDOC_OPT) "$(INSTALL_BINDIR)"
	$(INSTALL_DATA) \
	  $(OCAMLDOC_LIBCMIS) \
	  "$(INSTALL_LIBDIR)/ocamldoc"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  $(OCAMLDOC_LIBMLIS) $(OCAMLDOC_LIBCMTS) \
	  "$(INSTALL_LIBDIR)/ocamldoc"
endif
	$(INSTALL_DATA) \
	  ocamldoc.hva *.cmx $(OCAMLDOC_LIBA) $(OCAMLDOC_LIBCMXA) \
	  "$(INSTALL_LIBDIR)/ocamldoc"

# TODO: also split into several rules

# backup, clean and depend :
############################

.PHONY: clean
clean:
	rm -f \#*\#
	rm -f $(programs) $(programs:=.exe)
	rm -f *.cma *.cmxa *.cmo *.cmi *.cmx *.cmt *.cmti *.a *.lib *.o *.obj
	rm -f odoc_parser.output odoc_text_parser.output
	rm -f odoc_lexer.ml odoc_text_lexer.ml odoc_see_lexer.ml odoc_ocamlhtml.ml
	rm -f odoc_parser.ml odoc_parser.mli odoc_text_parser.ml odoc_text_parser.mli

.PHONY: distclean
distclean: clean
	rm -f META

.PHONY: depend
depend: $(DEPEND_PREREQS)
	$(OCAMLDEP_CMD) *.mll *.mly *.ml *.mli > .depend

include .depend
