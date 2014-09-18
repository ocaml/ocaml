# $Id$

FILES=allfiles.tex biblio.tex foreword.tex version.tex warnings.etex
TEXINPUTS=.:..:../refman:../library:../cmds:../tutorials:../../styles:
TEXFONTS=../../styles:
RELEASE=$$HOME/release/$${RELEASENAME}
SRC=../../release
OCAMLDOC=$(SRC)/ocamldoc/ocamldoc -hide Pervasives
HEVEA=hevea
HACHA=hacha
INFO=-fix -exec xxdate.exe -info -w 79
HTML=-fix -exec xxdate.exe -O
TEXT=-fix -exec xxdate.exe -text -w 79

SRC2=../$(SRC)
MLIS=$(SRC2)/stdlib/*.mli \
	$(SRC2)/otherlibs/bigarray/bigarray.mli \
	$(SRC2)/otherlibs/dynlink/dynlink.mli \
	$(SRC2)/otherlibs/graph/graphics.mli \
	$(SRC2)/otherlibs/graph/graphicsX11.mli \
	$(SRC2)/otherlibs/num/num.mli \
	$(SRC2)/otherlibs/num/arith_status.mli \
	$(SRC2)/otherlibs/num/big_int.mli \
	$(SRC2)/otherlibs/num/ratio.mli \
	$(SRC2)/otherlibs/str/*.mli \
	$(SRC2)/otherlibs/systhreads/*.mli \
	$(SRC2)/otherlibs/unix/*.mli

manual: files
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) latex manual.tex

labltk: cmds/browser.tex library/liblabltk.tex library/tk.mli
	cd library; make Tk.tex
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) latex labltk.tex

index::
	cd texstuff && \
	  ../../tools/fix_index.sh manual.idx && \
	  makeindex manual.idx
	cd texstuff; makeindex manual.kwd.idx

pdfmanual: files
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) pdflatex pdfmanual.tex

index::
	cd texstuff && \
	  ../../tools/fix_index.sh pdfmanual.idx && \
	  makeindex pdfmanual.idx
	cd texstuff; makeindex pdfmanual.kwd.idx

html: files
	cd htmlman; \
	mkdir -p libref ; \
	../$(OCAMLDOC) -colorize-code -sort -html \
	-d libref \
	-I $(SRC2)/stdlib  \
	-I $(SRC2)/otherlibs/bigarray \
	-I $(SRC2)/otherlibs/dynlink \
	-I $(SRC2)/otherlibs/graph \
	-I $(SRC2)/otherlibs/num \
	-I $(SRC2)/otherlibs/str \
	-I $(SRC2)/otherlibs/systhreads \
	-I $(SRC2)/otherlibs/unix \
	$(MLIS) ; \
	cp -f ../style.css libref ; \
	${HEVEA} ${HTML} -I .. -I ../refman -I ../library -I ../cmds \
	         -I ../tutorials -I ../../styles -I ../texstuff manual.hva \
	         -e macros.tex ../manual.tex ; \
	${HACHA} -tocter manual.html ; \


info: files
	cd infoman; rm -f ocaml.info*; \
	${HEVEA} ${INFO} -o ocaml.info.body -I .. -I ../refman -I ../library \
	         -I ../cmds -I ../tutorials -I ../../styles -I ../texstuff \
	         ../manual.inf -e macros.tex ../manual.tex
	cat manual.info.header infoman/ocaml.info.body > infoman/ocaml.info
	cd infoman; rm -f ocaml.info.tmp; gzip -9 ocaml.info*

text: files
	cd textman; \
	${HEVEA} ${TEXT} -I .. -I ../refman -I ../library -I ../cmds \
	         -I ../tutorials -I ../../styles -I ../texstuff \
	         ../manual.inf -e macros.tex ../manual.tex

files: $(FILES)
	cd refman; $(MAKE) all
	cd library; $(MAKE) all
	cd cmds; $(MAKE) all
	cd tutorials; $(MAKE) all

all:
	$(MAKE) manual pdfmanual
	$(MAKE) manual pdfmanual
	$(MAKE) index
	$(MAKE) manual pdfmanual
	$(MAKE) html text info

clean:
	rm -f $(FILES)
	cd refman; $(MAKE) clean
	cd library; $(MAKE) clean
	cd cmds; $(MAKE) clean
	cd tutorials; $(MAKE) clean
	-rm -f texstuff/*
	cd htmlman; rm -rf libref index.html manual*.html *.haux *.hind
	cd textman; rm -f manual.txt *.haux *.hind
	cd infoman; rm -f ocaml.info ocaml.info-*  *.haux *.hind
	rm -f warnings.etex

release:
	gzip < texstuff/manual.dvi > $(RELEASE)refman.dvi.gz
	dvips -o '!gzip > $(RELEASE)refman.ps.gz' texstuff/manual.dvi
	cp htmlman/manual.html $(RELEASE)refman.html
	rm -f htmlman/manual.{html,haux,hmanual*,htoc}
	tar zcf $(RELEASE)refman-html.tar.gz htmlman/*.* htmlman/libref
	zip -8 $(RELEASE)refman-html.zip htmlman/*.* htmlman/libref/*.*
	cp texstuff/pdfmanual.pdf $(RELEASE)refman.pdf
	cp textman/manual.txt $(RELEASE)refman.txt
	tar cf - infoman/ocaml.info* | gzip > $(RELEASE)refman.info.tar.gz

.SUFFIXES:
.SUFFIXES: .tex .etex .htex

.etex.tex:
	../tools/texquote2 < $*.etex > $*.tex

version.tex: $(SRC)/VERSION
	sed -n -e '1s/^\([0-9]*\.[0-9]*\).*$$/\\def\\ocamlversion{\1}/p' \
	    $(SRC)/VERSION > version.tex

warnings.etex: $(SRC)/utils/warnings.ml $(SRC)/ocamlc
	$(SRC)/boot/ocamlrun $(SRC)/ocamlc -warn-help \
	| sed -e 's/^ *\([0-9A-Z][0-9]*\)\(.*\)/\\item[\1] \2/' > warnings.etex
