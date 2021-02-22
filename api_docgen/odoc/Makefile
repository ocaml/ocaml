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
include $(ROOTDIR)/api_docgen/Makefile.common

vpath %.cmti $(ROOTDIR)/stdlib $(DOC_COMPILERLIBS_DIRS) $(DOC_STDLIB_DIRS)
vpath %.cmt $(ROOTDIR)/stdlib

odoc=odoc
libref=$(STDLIB) $(otherlibref)

define page_name
	$(dir $1)page-$(notdir $1)
endef

define stdlib_prefix
	$(if $(filter-out stdlib camlinternal%,$1),\
	Stdlib.$(call capitalize,$1),\
	$(call capitalize, $1))
endef
build/latex/ifocamldoc.tex: | build/latex
	printf '\\newif\ifocamldoc\ocamldocfalse\n' > $@

stdlib_INPUT= $(foreach module,\
$(filter-out stdlib camlinternal%, $(STDLIB:stdlib__%=%)),\
\\input{libref/Stdlib.$(call capitalize,$(module)).tex}\
)

build/latex/stdlib_input.tex: | build/latex
	echo $(stdlib_INPUT)> $@

build/latex/compilerlibs_input.tex: | build/latex
	echo $(compilerlibref_C:%=\\input{compilerlibref/%})> $@

$(libref_TEXT:%=build/libref/page-%.odoc):
build/libref/page-%.odoc:$(DOCGEN)/%.mld | build/libref
	$(odoc) compile -I build/libref --package libref $< -o $@

$(compilerlibref_TEXT:%=build/compilerlibref/page-%.odoc):\
build/compilerlibref/page-%.odoc:$(DOCGEN)/build/%.mld | build/compilerlibref
	$(odoc) compile -I build/libref --package compilerlibref $< -o $@

$(libref:%=build/libref/%.odoc):\
build/libref/%.odoc: %.cmti | build/libref
	$(odoc) compile -I build/libref  --package libref $< -o $@
$(libref_EXTRA:%=build/libref/%.odoc):build/libref/%.odoc:%.cmt
	$(odoc) compile -I build/libref --package libref $< -o $@

$(compilerlibref:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc: %.cmti $(libref:%=build/libref/%.odoc) \
| build/compilerlibref
	$(odoc) compile -I build/libref -I build/compilerlibref \
	--package compilerlibref $< -o $@

build/html/odoc.css: | build/html
	$(odoc) support-files --output-dir build/html

ALL_TEXT = $(libref_TEXT:%=libref/%) $(compilerlibref_TEXT:%=compilerlibref/%)
ALL_PAGE_TEXT=$(foreach mld,$(ALL_TEXT),$(call page_name,$(mld)))
TARGET_UNITS= \
	$(compilerlibref:%=compilerlibref/%) \
	libref/stdlib $(otherlibref:%=libref/%) \
	$(addprefix libref/,$(filter camlinternal%,$(STDLIB)))
ALL_UNITS = $(compilerlibref:%=compilerlibref/%) $(libref:%=libref/%)
ALL_PAGED_DOC = $(TARGET_UNITS) $(ALL_PAGE_TEXT)

ALL_HTML = $(ALL_PAGED_DOC:%=build/%.html.stamp)
ALL_MAN = $(ALL_PAGED_DOC:%=build/%.3o.stamp)
ALL_LATEX = $(ALL_PAGED_DOC:%=build/%.tex.stamp)

build/libref/stdlib.html.stamp: $(STDLIB:%=build/libref/%.odocl) | build/libref
build/libref/stdlib.3o.stamp: $(STDLIB:%=build/libref/%.odocl) | build/libref
build/libref/stdlib.tex.stamp: $(STDLIB:%=build/libref/%.odocl) | build/libref

man: $(ALL_MAN)
html: $(ALL_HTML) build/html/odoc.css
html: build/libref/index.html.stamp build/compilerlibref/index.html.stamp
latex: $(ALL_LATEX)


build/latex/alldoc.pdf: $(ALL_LATEX) build/latex/alldoc.tex \
build/latex/stdlib_input.tex build/latex/compilerlibs_input.tex \
| build/latex
	cd build/latex && pdflatex alldoc.tex
	cd build/latex && pdflatex alldoc.tex

$(ALL_UNITS:%=build/%.odocl):%.odocl:%.odoc \
	| $(ALL_PAGED_DOC:%=build/%.odoc)
	$(odoc) link -I build/libref -I build/compilerlibref $<

$(ALL_PAGE_TEXT:%=build/%.odocl):%.odocl:%.odoc \
	| $(ALL_PAGED_DOC:%=build/%.odoc)
	$(odoc) link -I build/libref -I build/compilerlibref $<

$(ALL_HTML): %.html.stamp: %.odocl | build/html
	$(odoc) html-generate --output-dir build/html  $<
	touch $@

$(build/libref.html.stamp build/compilerlibref.html.stamp):
%.html.stamp: %.mld | build/
	$(odoc) html-generate --output-dir build/html  $<
	touch $@

stdlib_INDEX=\
	$(foreach m,$(stdlib_UNPREFIXED),$(call stdlib_prefix,$m))\
	$(call capitalize, $(otherlibref))
build/libref.mld:
	echo {0 OCaml standard library} {!modules:$(stdlib_INDEX)} > $@

build/libref/index.html.stamp: $(ALL_HTML) build/libref.mld | build/libref
	$(odoc) compile --package libref build/libref.mld
	$(odoc) link -I build/libref build/page-libref.odoc
	$(odoc) html-generate build/page-libref.odocl --output-dir build/html
	mv build/html/libref/libref.html build/html/libref/index.html
	touch $@

build/compilerlibref/index.html.stamp: $(ALL_HTML) \
  build/compilerlibref/page-Compiler_libs.html.stamp | build/compilerlibref
	cp build/html/compilerlibref/Compiler_libs.html \
           build/html/compilerlibref/index.html
	touch $@

NOT_STDLIB=$(filter-out libref/stdlib,$(ALL_PAGED_DOC))
$(NOT_STDLIB:%=build/%.tex.stamp):\
build/%.tex.stamp: build/%.odocl | build/
	$(odoc) latex-generate --with-children=true --output-dir build/latex $<
	touch $@

build/libref/stdlib.tex.stamp: build/libref/stdlib.odocl | build/libref
	$(odoc) latex-generate --with-children=false --output-dir build/latex $<
	touch $@

$(ALL_PAGED_DOC:%=build/%.3o.stamp):build/%.3o.stamp:build/%.odocl | build/
	$(odoc) man-generate --output-dir build/man  $<
	touch $@


INSTALL_MANODIR=$(INSTALL_MANDIR)/man3
.PHONY:install
install:
	$(MKDIR) "$(INSTALL_MANODIR)"
	if test -d build/man/libref ; then \
	  $(INSTALL_DATA) build/man/libref/* "$(INSTALL_MANODIR)"; \
	else : ; fi
	if test -d build/man/compilerlibref ; then \
	  $(INSTALL_DATA) build/man/libref/* "$(INSTALL_MANODIR)"; \
	else : ; fi
