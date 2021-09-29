SRC = $(abspath ../..)

export LD_LIBRARY_PATH   ?= "$(SRC)/otherlibs/unix/:$(SRC)/otherlibs/str/"
export DYLD_LIBRARY_PATH ?= "$(SRC)/otherlibs/unix/:$(SRC)/otherlibs/str/"
SET_LD_PATH = CAML_LD_LIBRARY_PATH=$(LD_LIBRARY_PATH)

TEXQUOTE = $(SRC)/runtime/ocamlrun ../tools/texquote2

FILES = allfiles.tex biblio.tex foreword.tex version.tex warnings-help.etex

TEXINPUTS = ".:..:../refman:../refman/extensions:../library:../cmds:../tutorials:../../styles:"
RELEASE = $$HOME/release/$${RELEASENAME}
HEVEA = hevea
HACHA = hacha
# We suppress warnings in info and text mode (with -s) because hevea listings emit
# DIV blocks that the text modes do not know how to interpret.
INFO_FLAGS = -fix -exec xxdate.exe -info -w 79 -s
HTML_FLAGS = -fix -exec xxdate.exe -O
TEXT_FLAGS = -fix -exec xxdate.exe -text -w 79 -s

# Copy the documentation files from SRC/doc
.PHONY: html_files
.PHONY: latex_files
latex_files:
	$(MAKE) -C $(SRC)/api_docgen latex
	cp $(SRC)/api_docgen/latex/*.tex library
html_files:
	$(MAKE) -C $(SRC)/api_docgen html
	mkdir -p htmlman/libref
	cp -r $(SRC)/api_docgen/html/libref $(SRC)/api_docgen/html/compilerlibref htmlman
	cp style.css htmlman/libref
	cp style.css htmlman/compilerlibref


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
	$(MAKE) html text info manual
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

version.tex: $(SRC)/VERSION
	sed -n -e '1s/^\([0-9]*\.[0-9]*\).*$$/\\def\\ocamlversion{\1}/p' $< > $@

warnings-help.etex: $(SRC)/utils/warnings.ml $(SRC)/ocamlc
	(echo "% This file is generated from (ocamlc -warn-help)";\
	 echo "% according to a rule in manual/src/Makefile.";\
	 echo "% In particular, the reference to documentation sections";\
	 echo "% are inserted through the Makefile, which should be updated";\
	 echo "% when a new warning is documented.";\
	 echo "%";\
	 $(SET_LD_PATH) $(SRC)/boot/ocamlrun $(SRC)/ocamlc -warn-help \
	 | sed -e 's/^ *\([0-9][0-9]*\) *\[\([a-z][a-z-]*\)\]\(.*\)/\\item[\1 "\2"] \3/' \
               -e 's/^ *\([0-9A-Z][0-9]*\) *\([^]].*\)/\\item[\1] \2/'\
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
	rm -f $(FILES) *.texquote_error
	$(MAKE) -C cmds      clean
	$(MAKE) -C library   clean
	$(MAKE) -C refman    clean
	$(MAKE) -C tutorials clean
	$(MAKE) -C html_processing clean
	-rm -f texstuff/*
	cd htmlman; rm -rf libref compilerlibref index.html \
	manual*.html *.haux *.hind *.svg
	cd textman; rm -f manual.txt *.haux *.hind
	cd infoman; rm -f ocaml.info ocaml.info-*  *.haux *.hind
