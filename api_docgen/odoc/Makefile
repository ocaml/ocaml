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

vpath %.cmti ../../stdlib $(DOC_COMPILERLIBS_DIRS) $(DOC_STDLIB_DIRS)
vpath %.cmt ../../stdlib

ifeq ($(DOCUMENTATION_TOOL),odoc)
  odoc ?= $(DOCUMENTATION_TOOL_CMD)
else
  odoc ?= odoc
endif

libref = $(STDLIB) $(otherlibref)

# odoc needs a "page-" prefix for a mld documentation file
define page_name
  $(dir $1)page-$(notdir $1)
endef

define stdlib_prefix
  $(if $(filter-out stdlib camlinternal%,$1),\
  Stdlib.$(call capitalize,$1),\
  $(call capitalize, $1))
endef

# define the right conditional for the manual
build/latex/ifocamldoc.tex: | build/latex
	printf '\\newif\\ifocamldoc\\ocamldocfalse\n' > $@


# \input{} all modules in the stdlib for the latex api manual
stdlib_INPUT= $(foreach module,\
$(filter-out stdlib camlinternal%, $(STDLIB:stdlib__%=%)),\
\\input{libref/Stdlib.$(call capitalize,$(module)).tex}\
)

build/latex/stdlib_input.tex: | build/latex
	echo $(stdlib_INPUT)> $@

build/latex/compilerlibs_input.tex: | build/latex
	echo $(compilerlibref_C:%=\\input{compilerlibref/%})> $@

# The build process for odoc has 3 phases:
# 1. generation of internal individual documentation files  (.odoc)
# 2. generation of linked documentation files (.odocl)
# 3. generation of the actual (.tex,.html,.3o) documentation

# rules for the mld files
$(libref_TEXT:%=build/libref/page-%.odoc):
build/libref/page-%.odoc: build/%.mld | build/libref
	$(odoc) compile -I build/libref --package libref $< -o $@

$(compilerlibref_TEXT:%=build/compilerlibref/page-%.odoc):\
build/compilerlibref/page-%.odoc: build/%.mld | build/compilerlibref
	$(odoc) compile -I build/libref --package compilerlibref $< -o $@

# rules for the stdlib and otherlibs .doc files
$(libref:%=build/libref/%.odoc):\
build/libref/%.odoc: %.cmti | build/libref
	$(odoc) compile -I build/libref  --package libref $< -o $@

# pervasives is handled separatedly due to the lack of cmti file
$(libref_EXTRA:%=build/libref/%.odoc):build/libref/%.odoc:%.cmt
	$(odoc) compile -I build/libref --package libref $< -o $@

# rules for the compilerlib documentation
$(compilerlibref:%=build/compilerlibref/%.odoc):\
build/compilerlibref/%.odoc: %.cmti $(libref:%=build/libref/%.odoc) \
| build/compilerlibref
	$(odoc) compile -I build/libref -I build/compilerlibref \
	--package compilerlibref $< -o $@

ALL_TEXT = $(libref_TEXT:%=libref/%) $(compilerlibref_TEXT:%=compilerlibref/%)
ALL_PAGE_TEXT=$(foreach mld,$(ALL_TEXT),$(call page_name,$(mld)))
TARGET_UNITS= \
  $(compilerlibref:%=compilerlibref/%) \
  libref/stdlib $(otherlibref:%=libref/%) \
  $(addprefix libref/,$(filter camlinternal%,$(STDLIB)))
ALL_UNITS = $(compilerlibref:%=compilerlibref/%) $(libref:%=libref/%)
ALL_PAGED_DOC = $(TARGET_UNITS) $(ALL_PAGE_TEXT)

# rules for odocl generation
# Note that we are using a dependency on the whole phase 1 rather than tracking
# the individual file dependencies
%.odocl:%.odoc \
  | $(ALL_PAGED_DOC:%=build/%.odoc)
	$(odoc) link -I build/libref -I build/compilerlibref $(ODOC_LINK_ARGS) $<

%.odocl:%.odoc \
  | $(ALL_PAGED_DOC:%=build/%.odoc)
	$(odoc) link -I build/libref -I build/compilerlibref $(ODOC_LINK_ARGS) $<

build/libref/stdlib.odocl: ODOC_LINK_ARGS+=--open=""

# Rules for all three backends:

ALL_HTML = $(ALL_PAGED_DOC:%=build/%.html.stamp)
ALL_MAN = $(ALL_PAGED_DOC:%=build/%.3o.stamp)
ALL_LATEX = $(ALL_PAGED_DOC:%=build/%.tex.stamp)

build/libref/stdlib.html.stamp: $(STDLIB:%=build/libref/%.odocl) | build/libref
build/libref/stdlib.3o.stamp: $(STDLIB:%=build/libref/%.odocl) | build/libref
build/libref/stdlib.tex.stamp: $(STDLIB:%=build/libref/%.odocl) | build/libref

man: $(ALL_MAN)
html: $(ALL_HTML) build/html/odoc.css
html: build/libref/index.html.stamp build/compilerlibref/index.html.stamp

# Html rules
$(ALL_HTML): %.html.stamp: %.odocl | build/html
	$(odoc) html-generate --output-dir build/html  $<
	touch $@

build/html/odoc.css: | build/html
	$(odoc) support-files --output-dir build/html

$(build/libref.html.stamp build/compilerlibref.html.stamp):
%.html.stamp: %.mld | build/
	$(odoc) html-generate --output-dir build/html  $<
	touch $@

# Html indexes for the api documentation

# The stdlib index is generated from the list of stdlib modules.
stdlib_INDEX=\
  $(foreach m,$(stdlib_UNPREFIXED),$(call stdlib_prefix,$m))\
  $(call capitalize, $(otherlibref))
build/libref.mld:
	( echo "{0 OCaml standard library}"; \
	  echo "{!modules:$(stdlib_INDEX)}" ) > $@

build/libref/index.html.stamp: $(ALL_HTML) build/libref.mld \
| build/libref build/html/libref
	$(odoc) compile --package libref build/libref.mld
	$(odoc) link -I build/libref build/page-libref.odoc
	$(odoc) html-generate build/page-libref.odocl --output-dir build/html
	mv build/html/libref/libref.html build/html/libref/index.html
	touch $@

build/compilerlibref/index.html.stamp: $(ALL_HTML) \
  build/compilerlibref/page-Compiler_libs.html.stamp | build/html/compilerlibref
	cp build/html/compilerlibref/Compiler_libs.html \
           build/html/compilerlibref/index.html
	touch $@

# Latex rules

latex: $(ALL_LATEX)


build/latex/alldoc.pdf: $(ALL_LATEX) build/latex/alldoc.tex \
build/latex/stdlib_input.tex build/latex/compilerlibs_input.tex \
| build/latex
	cd build/latex && pdflatex alldoc.tex
	cd build/latex && pdflatex alldoc.tex

# We include children pages directly except for the root Stdlib module
NOT_STDLIB=$(filter-out libref/stdlib,$(ALL_PAGED_DOC))
$(NOT_STDLIB:%=build/%.tex.stamp):\
build/%.tex.stamp: build/%.odocl | build/
	$(odoc) latex-generate --with-children=true --output-dir build/latex $<
	touch $@

# Stdlib latex page: we manually integrate stdlib module
build/libref/stdlib.tex.stamp: build/libref/stdlib.odocl | build/libref
	$(odoc) latex-generate --with-children=false --output-dir build/latex $<
	touch $@

# Man pages
$(ALL_PAGED_DOC:%=build/%.3o.stamp):build/%.3o.stamp:build/%.odocl | build/
	$(odoc) man-generate --output-dir build/man  $<
	touch $@

# Man pages are the only installed documentation
.PHONY: install
install:
	$(MKDIR) "$(INSTALL_LIBRARIES_MAN_DIR)"
	if test -d build/man/libref ; then \
	  $(INSTALL_DATA) build/man/libref/* "$(INSTALL_LIBRARIES_MAN_DIR)"; \
	fi
	if test -d build/man/compilerlibref ; then \
	  $(INSTALL_DATA) build/man/libref/* "$(INSTALL_LIBRARIES_MAN_DIR)"; \
	fi

# Dependencies for stdlib modules.
# Use the same dependencies used for compiling .cmx files.
# The existing rules look like this:
#   stdlib__X.cmx: x.ml \
#       stdlib__Y.cmx \
#       stdlib__X.cmi
# We want:
#   build/libref/stdlib__X.odoc: \
#       build/libref/stdlib__Y.odoc \
#       stdlib__X.cmti
build/.depend: ../../stdlib/.depend | build/
	sed \
    -e ':l; /\\ *$$/ { N; bl }; # Read lines separated by \\' \
    -e '/^\S*\.cmx *:/! d; # Keep only rules to .cmx' \
    -e 's#\<\(\w*\)\.cmx\>#build/libref/\1.odoc#g; # .cmx -> .odoc' \
    -e 's/\.cmi\>/.cmti/g; # .cmi -> .cmti' \
    -e 's/\<\S*\.ml\>//g; # .ml -> removed' \
    $< > $@

include build/.depend
