FILES=allfiles.tex biblio.tex foreword.tex version.tex foreword.htex
DVI2TXT=../tools/dvi2txt
TEXINPUTS=.:..:../refman:../library:../cmds:../tutorials:../../styles:
TEXFONTS=../../styles:
RELEASE=$$HOME/release/$${RELEASENAME}
CSLDIR=../../csl

manual: files
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) latex manual.tex

index::
	cd texstuff; makeindex manual.idx
	cd texstuff; makeindex manual.kwd.idx

pdfmanual: files
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) pdflatex pdfmanual.tex

index::
	cd texstuff; makeindex pdfmanual.idx
	cd texstuff; makeindex pdfmanual.kwd.idx

#plaintext: files
#	cd texstuff; \
#	TEXINPUTS=$(TEXINPUTS) TEXFONTS=$(TEXFONTS) latex plaintext.tex
#	$(DVI2TXT) texstuff/plaintext.dvi > texstuff/plaintext.txt
#	$(DVI2TXT) -p texstuff/plaintext.dvi > texstuff/plaintext.ipr
#
#index::
#	cd texstuff; makeindex plaintext.idx
#	cd texstuff; makeindex plaintext.kwd.idx

html: files
	cd htmlman; \
        TEXINPUTS=$(TEXINPUTS) ../../tools/texexpand ../manual.htex | \
        ../../tools/htmlgen > ../manual.html
	cd htmlman; ../../tools/htmlcut -6 -18 ../manual.html > index.html
	cd htmlman; ../../tools/htmlthread node*.html

files: $(FILES)
	cd refman; $(MAKE) all
	cd library; $(MAKE) all
	cd cmds; $(MAKE) all
	cd tutorials; $(MAKE) all

all:
	$(MAKE) manual pdfmanual # plaintext
	$(MAKE) manual pdfmanual # plaintext
	$(MAKE) index
	$(MAKE) manual pdfmanual # plaintext
	$(MAKE) html
clean:
	rm -f $(FILES)
	cd refman; $(MAKE) clean
	cd library; $(MAKE) clean
	cd cmds; $(MAKE) clean
	rm -f texstuff/*
	cd htmlman; rm -f index.html node*.html

release:
	gzip < texstuff/manual.dvi > $(RELEASE)refman.dvi.gz
	dvips -Php11rv -o '!gzip > $(RELEASE)refman.ps.gz' texstuff/manual.dvi
#	cp texstuff/plaintext.txt $(RELEASE)refman.txt
#	cp texstuff/plaintext.ipr $(RELEASE)refman.prn
	tar cf - htmlman/*.* | gzip > $(RELEASE)refman.html.tar.gz
	cp texstuff/pdfmanual.pdf $(RELEASE)refman.pdf

.SUFFIXES:
.SUFFIXES: .tex .etex .htex

.etex.tex:
	../tools/texquote2 < $*.etex > $*.tex
.etex.htex:
	cp $*.etex $*.htex

version.tex: $(CSLDIR)/utils/config.mlp
	sed -n -e 's/^let version = "\(.*\)".*$$/\\def\\ocamlversion{\1}/p' $(CSLDIR)/utils/config.mlp > version.tex
