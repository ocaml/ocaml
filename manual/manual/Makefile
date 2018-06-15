SRC = $(abspath ../..)

export LD_LIBRARY_PATH   ?= "$(SRC)/otherlibs/unix/:$(SRC)/otherlibs/str/"
export DYLD_LIBRARY_PATH ?= "$(SRC)/otherlibs/unix/:$(SRC)/otherlibs/str/"
SET_LD_PATH = CAML_LD_LIBRARY_PATH=$(LD_LIBRARY_PATH)

OCAMLDOC = $(if $(wildcard $(SRC)/ocamldoc/ocamldoc.opt),\
  $(SRC)/ocamldoc/ocamldoc.opt,\
  $(SET_LD_PATH) $(SRC)/byterun/ocamlrun $(SRC)/ocamldoc/ocamldoc)\
  -hide Pervasives -nostdlib -initially-opened-module Pervasives

# Copy and unprefix the standard library when needed
include $(SRC)/ocamldoc/Makefile.unprefix

TEXQUOTE = ../tools/texquote2


FILES = allfiles.tex biblio.tex foreword.tex version.tex warnings-help.etex

TEXINPUTS = ".:..:../refman:../library:../cmds:../tutorials:../../styles:"
RELEASE = $$HOME/release/$${RELEASENAME}
HEVEA = hevea
HACHA = hacha
INFO_FLAGS = -fix -exec xxdate.exe -info -w 79
HTML_FLAGS = -fix -exec xxdate.exe -O
TEXT_FLAGS = -fix -exec xxdate.exe -text -w 79


manual: files
	cd texstuff \
	  && TEXINPUTS=$(TEXINPUTS) pdflatex manual.tex

index:
	cd texstuff \
	  && sh ../../tools/fix_index.sh manual.idx \
	  && makeindex manual.idx \
	  && makeindex manual.kwd.idx


# libref/style.css is used as witness for the generation of the html stdlib
# reference.
html: htmlman/libref/style.css etex-files
	cd htmlman \
	  && $(HEVEA) $(HTML_FLAGS) \
	    -I .. -I ../cmds -I ../library -I ../refman -I ../tutorials \
	    -I ../../styles -I ../texstuff \
	    manual.hva -e macros.tex ../manual.tex \
	  && $(HACHA) -tocter manual.html

htmlman/libref/style.css: style.css $(STDLIB_MLIS) | unprefix_stdlib_for_ocamldoc
	mkdir -p htmlman/libref
	$(OCAMLDOC) -colorize-code -sort -html \
	  -d htmlman/libref \
	  -I $(STDLIB_UNPREFIXED) \
	  $(STDLIB_MLIS)
	cp style.css $@


info: files
	cd infoman \
	  && rm -f ocaml.info* \
	  && $(HEVEA) $(INFO_FLAGS) -o ocaml.info.body \
	    -I .. -I ../cmds -I ../library -I ../refman -I ../tutorials \
	    -I ../../styles -I ../texstuff \
	    ../manual.inf -e macros.tex ../manual.tex
	cat manual.info.header infoman/ocaml.info.body > infoman/ocaml.info
	cd infoman \
	  && rm -f ocaml.info.tmp ocaml.info.body \
	  && gzip -9 ocaml.info*

text: files
	cd textman \
	  && $(HEVEA) $(TEXT_FLAGS) \
	    -I .. -I ../cmds -I ../library -I ../refman -I ../tutorials \
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
	tar zcf $(RELEASE)refman-html.tar.gz htmlman/*.* htmlman/libref
	zip -8 $(RELEASE)refman-html.zip htmlman/*.* htmlman/libref/*.*
	cp texstuff/manual.pdf $(RELEASE)refman.pdf
	cp textman/manual.txt $(RELEASE)refman.txt
	tar cf - infoman/ocaml.info* | gzip > $(RELEASE)refman.info.tar.gz


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
	 echo "% according to a rule in manual/manual/Makefile.";\
	 echo "% In particular, the reference to documentation sections";\
	 echo "% are inserted through the Makefile, which should be updated";\
	 echo "% when a new warning is documented.";\
	 echo "%";\
	 $(SET_LD_PATH) $(SRC)/boot/ocamlrun $(SRC)/ocamlc -warn-help \
	 | sed -e 's/^ *\([0-9A-Z][0-9]*\)\(.*\)/\\item[\1] \2/'\
	) >$@
#	sed --inplace is not portable, emulate
	for i in 52 57; do\
	  sed\
	    s'/\\item\['$$i'\]/\\item\['$$i' (see \\ref{ss:warn'$$i'})\]/'\
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
	-rm -f texstuff/*
	cd htmlman; rm -rf libref index.html manual*.html *.haux *.hind
	cd textman; rm -f manual.txt *.haux *.hind
	cd infoman; rm -f ocaml.info ocaml.info-*  *.haux *.hind
