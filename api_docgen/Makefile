#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*             Florian Angeletti, projet Cambium, Inria Paris             *
#*                                                                        *
#*   Copyright 2020 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************
ROOTDIR = ..

include $(ROOTDIR)/stdlib/StdlibModules
include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries
include $(ROOTDIR)/ocamldoc/Makefile.best_ocamldoc
include Makefile.docfiles

DOC_COMPILERLIBS_DIRS= $(addprefix $(ROOTDIR)/, \
parsing utils typing bytecomp driver file_formats lambda\
)

DOC_STDLIB_DIRS = $(addprefix $(ROOTDIR)/, stdlib \
	otherlibs/str \
	otherlibs/$(UNIXLIB) otherlibs/dynlink \
	otherlibs/systhreads\
)

DOC_STDLIB_INCLUDES=$(addprefix -I , $(DOC_STDLIB_DIRS))

DOC_ALL_INCLUDES = \
	$(DOC_STDLIB_INCLUDES) \
	$(addprefix -I ,$(DOC_COMPILERLIBS_DIRS))

ALL_MAN= $(ALL_DOC:%=man/%.3o)
ALL_LATEX= $(ALL_DOC:%=latex/%.tex)

all: html pdf man
.PHONY:all
.SUFFIXES:

DIRS = build/libref build/compilerlibref man/ latex/ texi/ \
	html/ html/libref html/compilerlibref

$(DIRS):
	$(MKDIR) $@

pdf: latex/alldoc.pdf
latex/alldoc.pdf: latex/stdlib_input.tex latex/compilerlibs_input.tex

Compiler_libs.mld: Compiler_libs.pre.mld
	cp $< $@ && echo "{!modules:$(compilerlibref_C)}" >> $@

vpath %.mli $(ROOTDIR)/stdlib $(DOC_COMPILERLIBS_DIRS)  $(DOC_STDLIB_DIRS)

man: man/Stdlib.3o
html: html/libref/Stdlib.html html/compilerlibref/Compiler_libs.html
latex: latex/Stdlib.tex
texi: texi/stdlib.texi

LD_PATH := "$(ROOTDIR)/otherlibs/unix/:$(ROOTDIR)/otherlibs/str/"
SET_LD_PATH = CAML_LD_LIBRARY_PATH=$(LD_PATH)


$(libref:%=build/libref/%.odoc): build/libref/%.odoc: %.mli | build/libref
	$(OCAMLDOC_RUN) -nostdlib -hide Stdlib -lib Stdlib \
	-pp \
"$(AWK) -v ocamldoc=true -f $(ROOTDIR)/stdlib/expand_module_aliases.awk" \
	$(DOC_STDLIB_INCLUDES) $< -dump  $@

$(compilerlibref:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc: %.mli | build/compilerlibref
	$(OCAMLDOC_RUN) -nostdlib -hide Stdlib -lib Stdlib \
	$(DOC_ALL_INCLUDES) $< -dump  $@

$(compilerlibref_TEXT:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc: %.mld | build/compilerlibref
	$(OCAMLDOC_RUN) $(DOC_ALL_INCLUDES) -text $< -dump  $@

$(libref_TEXT:%=build/libref/%.odoc): build/libref/%.odoc: %.mld | build/libref
	$(OCAMLDOC_RUN) $(DOC_STDLIB_INCLUDES) -text $< -dump  $@

ALL_COMPILED_DOC=$(ALL_DOC:%=build/%.odoc)
man/Stdlib.3o: $(ALL_COMPILED_DOC) | man/
	$(MKDIR) man
	$(OCAMLDOC_RUN) -man -d man -man-mini \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(ALL_COMPILED_DOC))

html/libref/Stdlib.html: $(ALL_LIBREF:%=build/%.odoc) | html/libref
	$(OCAMLDOC_RUN) -html -d html/libref \
	-charset="utf8" \
	-nonavbar -nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(ALL_LIBREF:%=build/%.odoc))

html/compilerlibref/Compiler_libs.html: $(ALL_COMPILERLIBREF:%=build/%.odoc) \
| html/compilerlibref
	$(OCAMLDOC_RUN) -html -d html/compilerlibref \
	-nonavbar -nostdlib -hide Stdlib -t "OCaml compiler library" \
	-charset="utf8" \
	-intro Compiler_libs.mld \
	$(addprefix -load , $(ALL_COMPILERLIBREF:%=build/%.odoc))

texi/stdlib.texi: $(ALL_COMPILED_DOC) | texi/
	$(OCAMLDOC_RUN) -texi -o $@ \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(ALL_COMPILED_DOC))

latex/Stdlib.tex: $(ALL_COMPILED_DOC) | latex/
	$(OCAMLDOC_RUN) -latex -o latex/all.tex \
	-hide Stdlib -lib Stdlib $(DOC_ALL_INCLUDES) \
	-sepfiles \
	-latextitle "1,subsection*" \
	-latextitle "2,subsubsection*" \
	-latex-type-prefix "TYP" \
	-latex-module-prefix "" \
	-latex-module-type-prefix "" \
	-latex-value-prefix "" \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(ALL_COMPILED_DOC))

latex/alldoc.pdf: latex/Stdlib.tex latex/alldoc.tex | latex/
	cd latex && TEXINPUTS=$${TEXINPUTS}:$(ROOTDIR)/ocamldoc pdflatex alldoc
	cd latex && TEXINPUTS=$${TEXINPUTS}:$(ROOTDIR)/ocamldoc pdflatex alldoc
latex/alldoc.tex:alldoc.tex | latex/
	cp $< $@

stdlib_INPUT=$(foreach module,\
$(filter-out stdlib.mli camlinternal%,$(stdlib_UNPREFIXED)),\
\\input{$(call capitalize,$(module)).tex}\
)
latex/stdlib_input.tex: | latex/
	echo $(stdlib_INPUT) > $@

compilerlibs_INPUT=$(foreach module,\
$(filter-out camlinternal%,$(compilerlibref)),\
\\input{$(call capitalize,$(module)).tex})
latex/compilerlibs_input.tex: | latex/
	echo $(compilerlibs_INPUT) > $@

INSTALL_MANODIR=$(INSTALL_MANDIR)/man3
.PHONY:install
install:
	$(MKDIR) "$(INSTALL_MANODIR)"
	if test -d man; then \
	  $(INSTALL_DATA) man/* "$(INSTALL_MANODIR)"; \
	else : ; fi

clean:
	rm -rf build/ man/ html/ latex/ texi/ Compiler_libs.mld
