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
ROOTDIR = ..
-include ../Makefile.build_config

odoc-%:
	$(MAKE) -C odoc $*

ocamldoc-%:
	$(MAKE) -C ocamldoc $*

ifeq ($(DOCUMENTATION_TOOL),odoc)
man: odoc-man
latex: odoc-latex
html: odoc-html
	@echo "documentation is in ./api_docgen/odoc/build/html/"
all: html latex man
install: odoc-install
else
man: ocamldoc-man
latex: ocamldoc-latex
html: ocamldoc-html
	@echo "documentation is in ./api_docgen/ocamldoc/build/html/"
texi: ocamldoc-texi
pdf: ocamldoc-pdf
all: html pdf man latex texi
install: ocamldoc-install
endif

clean:
	rm -rf build odoc/build ocamldoc/build

distclean: clean

.PHONY: html latex man clean distclean install texi pdf
