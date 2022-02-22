ROOTDIR = ../..
-include $(ROOTDIR)/Makefile.build_config

TEXQUOTE = $(ROOTDIR)/runtime/ocamlrun ../tools/texquote2

FILES = allfiles.tex biblio.tex foreword.tex version.tex cmds/warnings-help.etex ifocamldoc.tex

TEXINPUTS = ".:..:../refman:../refman/extensions:../library:../cmds:../tutorials:../../styles:"
RELEASE = $$HOME/release/$${RELEASENAME}
HEVEA = hevea
HACHA = hacha
# We suppress warnings in info and text mode (with -s) because hevea listings emit
# DIV blocks that the text modes do not know how to interpret.
INFO_FLAGS = -fix -exec xxdate.exe -info -w 79 -s
HTML_FLAGS = -fix -exec xxdate.exe -O
TEXT_FLAGS = -fix -exec xxdate.exe -text -w 79 -s

# Copy the documentation files from ROOTDIR/api_docgen
APIDOC=$(ROOTDIR)/api_docgen
.PHONY: html_files
.PHONY: latex_files
ifeq ($(DOCUMENTATION_TOOL),odoc)
latex_files:
	make -C $(APIDOC) latex
	cp $(APIDOC)/odoc/build/latex/*/*.tex library

html_files:
	make -C $(APIDOC) html
	cp -r $(APIDOC)/odoc/build/html/*  htmlman

ifocamldoc.tex: $(ROOTDIR)/Makefile.build_config
	$(MAKE) -C $(APIDOC)/odoc build/latex/ifocamldoc.tex
	cp $(APIDOC)/odoc/build/latex/ifocamldoc.tex $@
else
latex_files:
	$(MAKE) -C $(APIDOC) latex
	cp $(APIDOC)/ocamldoc/build/latex/*.tex library

html_files:
	$(MAKE) -C $(APIDOC) html
	mkdir -p htmlman/libref
	cp -r $(APIDOC)/ocamldoc/build/html/libref htmlman
	cp -r $(APIDOC)/ocamldoc/build/html/compilerlibref htmlman
	cp style.css htmlman/libref
	cp style.css htmlman/compilerlibref

ifocamldoc.tex: $(ROOTDIR)/Makefile.build_config
	$(MAKE) -C $(APIDOC)/ocamldoc build/latex/ifocamldoc.tex
	cp $(APIDOC)/ocamldoc/build/latex/ifocamldoc.tex $@
endif

manual: files latex_files
	cd texstuff \
	  && TEXINPUTS=$(TEXINPUTS) pdflatex manual.tex

index:
	cd texstuff \
	  && sh ../../tools/fix_index.sh manual.idx \
	  && makeindex manual.idx \
	  && makeindex manual.kwd.idx

# libref/style.css and comilerlibref/style.css are used as witness
# for the generation of the html stdlib and compilerlibs reference.
html: etex-files html_files
	cd htmlman \
	  && $(HEVEA) $(HTML_FLAGS) \
	    -I .. -I ../cmds -I ../library -I ../refman \
	    -I ../refman/extensions -I ../tutorials \
	    -I ../../styles -I ../texstuff \
	    manual.hva -e macros.tex ../manual.tex \
	  && $(HACHA) -tocter manual.html

info: files latex_files
	cd infoman \
	  && rm -f ocaml.info* \
	  && $(HEVEA) $(INFO_FLAGS) -o ocaml.info.body \
	    -I .. -I ../cmds -I ../library -I ../refman \
	    -I ../refman/extensions -I ../tutorials \
	    -I ../../styles -I ../texstuff \
	    ../manual.inf -e macros.tex ../manual.tex
	cat manual.info.header infoman/ocaml.info.body > infoman/ocaml.info
	cd infoman \
	  && rm -f ocaml.info.tmp ocaml.info.body \
	  && gzip -9 ocaml.info*

text: files latex_files
	cd textman \
	  && $(HEVEA) $(TEXT_FLAGS) \
	    -I .. -I ../cmds -I ../library -I ../refman \
	    -I ../refman/extensions -I ../tutorials \
	    -I ../../styles -I ../texstuff \
	    ../manual.inf -e macros.tex ../manual.tex


all:
	$(MAKE) html
	$(MAKE) text
	$(MAKE) info
	$(MAKE) manual
	$(MAKE) index
	$(MAKE) manual

release: all
	cp htmlman/manual.html $(RELEASE)refman.html
	rm -f htmlman/manual.{html,haux,hmanual*,htoc}
	tar zcf $(RELEASE)refman-html.tar.gz \
	  htmlman/*.* htmlman/libref htmlman/compilerlibref htmlman/fonts
	zip -8 $(RELEASE)refman-html.zip \
	  htmlman/*.* htmlman/libref/*.* htmlman/compilerlibref/*.* \
	  htmlman/fonts/*.*
	cp texstuff/manual.pdf $(RELEASE)refman.pdf
	cp textman/manual.txt $(RELEASE)refman.txt
	tar cf - infoman/ocaml.info* | gzip > $(RELEASE)refman.info.tar.gz

web: html
	$(MAKE) -C html_processing all

files: $(FILES)
	$(MAKE) -C cmds      all
	$(MAKE) -C library   all
	$(MAKE) -C refman    all
	$(MAKE) -C tutorials all

etex-files: $(FILES)
	$(MAKE) -C cmds      etex-files
	$(MAKE) -C library   etex-files
	$(MAKE) -C refman    etex-files
	$(MAKE) -C tutorials etex-files


%.tex: %.etex
	$(TEXQUOTE) < $< > $*.texquote_error.tex
	mv $*.texquote_error.tex $@

cmds/warnings-help.etex: $(ROOTDIR)/utils/warnings.ml $(ROOTDIR)/ocamlc
	(echo "% This file is generated from (ocamlc -warn-help)";\
	 echo "% according to a rule in manual/src/Makefile.";\
	 echo "% In particular, the reference to documentation sections";\
	 echo "% are inserted through the Makefile, which should be updated";\
	 echo "% when a new warning is documented.";\
	 echo "%";\
	 $(ROOTDIR)/boot/ocamlrun $(ROOTDIR)/ocamlc -warn-help \
	 | LC_ALL=C sed -e 's/^ *\([0-9][0-9]*\) *\[\([a-z][a-z-]*\)\]\(.*\)/\\item[\1 "\2"] \3/' \
	                -e 's/^ *\([0-9A-Z][0-9]*\) *\([^]].*\)/\\item[\1] \2/'\
	 | sed -e 's/@/\\@/g' \
	) >$@
#	sed --inplace is not portable, emulate
	for i in 52 57; do\
	  sed\
	    s'/\\item\[\('$$i'[^]]*\)\]/\\item\[\1 (see \\ref{ss:warn'$$i'})\]/'\
	    $@ > $@.tmp;\
	  mv $@.tmp $@;\
	done

.PHONY: clean
clean:
	rm -f $(filter-out version.tex,$(FILES)) *.texquote_error
	$(MAKE) -C cmds      clean
	$(MAKE) -C library   clean
	$(MAKE) -C refman    clean
	$(MAKE) -C tutorials clean
	$(MAKE) -C html_processing clean
	-rm -f texstuff/*
	cd htmlman; rm -rf libref compilerlibref *.htoc *.html *.haux *.hind *.svg \
	                   manual.hmanual manual.hmanual.kwd manual.css
	cd textman; rm -f manual.txt *.haux *.hind *.htoc
	cd infoman; rm -f ocaml.info ocaml.info-*  *.haux *.hind *.info*.gz

.PHONY: distclean
distclean: clean
	rm -f version.tex
	$(MAKE) -C html_processing distclean
