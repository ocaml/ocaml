SRC = ../../..

CSLDIR = $(SRC)

LD_PATH := "$(SRC)/otherlibs/unix/:$(SRC)/otherlibs/str/"
SET_LD_PATH = CAML_LD_LIBRARY_PATH=$(LD_PATH)


OCAMLDOC = $(if $(wildcard $(CSLDIR)/ocamldoc/ocamldoc.opt),\
  $(CSLDIR)/ocamldoc/ocamldoc.opt,\
  $(SET_LD_PATH) $(CSLDIR)/runtime/ocamlrun $(CSLDIR)/ocamldoc/ocamldoc) \
   -hide Stdlib -lib Stdlib -nostdlib \
   -pp "$(AWK) -v ocamldoc=true -f $(SRC)/stdlib/expand_module_aliases.awk"


#Import mli file lists
include $(SRC)/ocamldoc/Makefile.docfiles


TEXQUOTE = $(SRC)/runtime/ocamlrun ../../tools/texquote2

CORE_INTF = Stdlib.tex

STDLIB_INTF = $(STDLIB_MODs:%=%.tex)

COMPILER_LIBS_PLUGIN_HOOKS = Pparse.tex Typemod.tex

COMPILER_LIBS_INTF = Asthelper.tex Astmapper.tex Asttypes.tex \
  Lexer.tex Location.tex Longident.tex Parse.tex Pprintast.tex Printast.tex \
  $(COMPILER_LIBS_PLUGIN_HOOKS)

OTHERLIB_INTF = Unix.tex UnixLabels.tex Str.tex \
  Thread.tex Mutex.tex Condition.tex Semaphore.tex Event.tex \
  Dynlink.tex Bigarray.tex

INTF = $(CORE_INTF) $(STDLIB_INTF) $(COMPILER_LIBS_INTF) $(OTHERLIB_INTF)

BLURB = core.tex builtin.tex stdlib-blurb.tex compilerlibs.tex \
  libunix.tex libstr.tex old.tex \
  libthreads.tex libdynlink.tex

FILES = $(BLURB) $(INTF)


etex-files: $(BLURB)
all: libs

libs: $(FILES)


# ocamldoc.out is used as witness for the generation of the stdlib tex files to
# avoid issues with parallel make invocations.
$(INTF): ocamldoc.out
ocamldoc.out: $(DOC_ALL)
	$(OCAMLDOC) -latex \
	  $(DOC_ALL_INCLUDES) \
	  $(DOC_ALL_MLIS) \
          $(DOC_ALL_TEXT:%=-text %) \
	  -sepfiles \
	  -latextitle "1,subsection*" \
	  -latextitle "2,subsubsection*" \
	  -latex-type-prefix "TYP" \
	  -latex-module-prefix "" \
	  -latex-module-type-prefix "" \
	  -latex-value-prefix ""
	mv Ast_helper.tex Asthelper.tex
	mv Ast_mapper.tex Astmapper.tex
	mv Ocaml_operators.tex Ocamloperators.tex

%.tex: %.etex
	$(TEXQUOTE) < $< > $*.texquote_error.tex
	mv $*.texquote_error.tex $@


.PHONY: clean
clean:
	rm -f *.tex ocamldoc.out ocamldoc.sty
	rm -f compiler_libs.txt
