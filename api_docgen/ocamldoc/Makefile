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
# Used by included Makefiles
ROOTDIR = ../..
include ../Makefile.common
include ../../ocamldoc/Makefile.best_ocamldoc
vpath %.mli ../../stdlib $(DOC_COMPILERLIBS_DIRS)  $(DOC_STDLIB_DIRS)


man: build/man/Stdlib.3o
latex: build/latex/Stdlib.tex
html: \
  build/html/libref/Stdlib.html \
  build/html/compilerlibref/Compiler_libs.html
texi: build/texi/stdlib.texi

DOC_STDLIB_INCLUDES= $(addprefix -I , $(DOC_STDLIB_DIRS))

DOC_ALL_INCLUDES = \
  $(DOC_STDLIB_INCLUDES) \
  $(addprefix -I ,$(DOC_COMPILERLIBS_DIRS))

libref=$(stdlib_UNPREFIXED) $(otherlibref)

ALL_MAN= $(ALL_DOC:%=build/man/%.3o)
ALL_LATEX= $(ALL_DOC:%=build/latex/%.tex)

build/latex/ifocamldoc.tex: | build/latex
	printf '\\newif\\ifocamldoc\\ocamldoctrue\n' > $@

$(libref:%=build/libref/%.odoc): build/libref/%.odoc: %.mli | build/libref
	$(OCAMLDOC_RUN) -nostdlib -hide Stdlib -lib Stdlib \
	-pp \
"$(AWK) -v ocamldoc=true -f ../../stdlib/expand_module_aliases.awk" \
	$(DOC_STDLIB_INCLUDES) $< -dump  $@

$(compilerlibref:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc: %.mli | build/compilerlibref
	$(OCAMLDOC_RUN) -nostdlib -hide Stdlib \
	$(DOC_ALL_INCLUDES) $< -dump  $@

$(compilerlibref_TEXT:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc: build/%.mld | build/compilerlibref
	$(OCAMLDOC_RUN) $(DOC_ALL_INCLUDES) -text $< -dump  $@

$(libref_TEXT:%=build/libref/%.odoc):\
build/libref/%.odoc: build/%.mld | build/libref
	$(OCAMLDOC_RUN) $(DOC_STDLIB_INCLUDES) -text $< -dump  $@

ALL_COMPILED_DOC=$(ALL_DOC:%=build/%.odoc)
build/man/Stdlib.3o: $(ALL_COMPILED_DOC) | build/man
	$(OCAMLDOC_RUN) -man -d build/man -man-mini \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(ALL_COMPILED_DOC))

HTML_OPTIONS= -charset="utf8" -colorize-code -nonavbar

build/html/libref/Stdlib.html: $(ALL_LIBREF:%=build/%.odoc) | build/html/libref
	$(OCAMLDOC_RUN) -html -d build/html/libref \
	$(HTML_OPTIONS) \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(ALL_LIBREF:%=build/%.odoc))

build/html/compilerlibref/Compiler_libs.html: \
  $(ALL_COMPILERLIBREF:%=build/%.odoc) | build/html/compilerlibref
	$(OCAMLDOC_RUN) -html -d build/html/compilerlibref \
	-nostdlib -hide Stdlib -t "OCaml compiler library" \
	$(HTML_OPTIONS) \
	-intro build/Compiler_libs.mld \
	$(addprefix -load , $(ALL_COMPILERLIBREF:%=build/%.odoc))

build/texi/stdlib.texi: $(ALL_COMPILED_DOC) | build/texi
	$(OCAMLDOC_RUN) -texi -o $@ \
	-nostdlib -hide Stdlib -lib Stdlib -t "OCaml library" \
	$(addprefix -load , $(ALL_COMPILED_DOC))

build/latex/Stdlib.tex: $(ALL_COMPILED_DOC) | build/latex
	$(OCAMLDOC_RUN) -latex -o build/latex/all.tex \
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

build/latex/alldoc.pdf: build/latex/Stdlib.tex build/latex/alldoc.tex \
  | build/latex
	cd build/latex && \
          TEXINPUTS=$${TEXINPUTS}:../../ocamldoc pdflatex alldoc
	cd build/latex && \
	  TEXINPUTS=$${TEXINPUTS}:../../ocamldoc pdflatex alldoc

stdlib_INPUT=$(foreach module,\
$(filter-out stdlib.mli camlinternal%,$(stdlib_UNPREFIXED)),\
\\input{$(call capitalize,$(module)).tex}\
)
build/latex/stdlib_input.tex: | build/latex
	echo $(stdlib_INPUT) > $@

compilerlibs_INPUT=$(foreach module,\
$(filter-out camlinternal%,$(compilerlibref)),\
\\input{$(call capitalize,$(module)).tex})
build/latex/compilerlibs_input.tex: | build/latex
	echo $(compilerlibs_INPUT) > $@

.PHONY: install
install:
	$(MKDIR) "$(INSTALL_LIBRARIES_MAN_DIR)"
	if test -d build/man; then \
	  $(INSTALL_DATA) build/man/*.3o "$(INSTALL_LIBRARIES_MAN_DIR)"; \
	fi
