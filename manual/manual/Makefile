FILES=allfiles.tex biblio.tex foreword.tex version.tex
TEXINPUTS=.:..:../refman:../library:../cmds:../tutorials:../../styles:
TEXFONTS=../../styles:
RELEASE=$$HOME/release/$${RELEASENAME}
CSLDIR=../../csl
HEVEA=hevea
HACHA=hacha
INFO=-info -w 79
HTML=
TEXT=-text -w 79

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

html: files
	cd htmlman; \
	${HEVEA} ${HTML} -I .. -I ../refman -I ../library -I ../cmds -I ../tutorials -I ../../styles -I ../texstuff manual.hva -e macros.tex ../manual.tex ;\
	${HACHA} manual.html

info: files
	cd infoman; rm -f ocaml.info*; \
	${HEVEA} ${INFO} -o ocaml.info -I .. -I ../refman -I ../library -I ../cmds -I ../tutorials -I ../../styles -I ../texstuff ../manual.inf -e macros.tex ../manual.tex
	cd infoman; gzip -9 ocaml.info*

text: files
	cd textman; \
	${HEVEA} ${TEXT} -I .. -I ../refman -I ../library -I ../cmds -I ../tutorials -I ../../styles -I ../texstuff ../manual.inf -e macros.tex ../manual.tex

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
	-rm -f texstuff/*
	cd htmlman; rm -f index.html manual*.html
	cd textman; rm -f manual.txt
	cd infoman; rm -f manual.info manual.info-*

release:
	gzip < texstuff/manual.dvi > $(RELEASE)refman.dvi.gz
	dvips -Php11rv -o '!gzip > $(RELEASE)refman.ps.gz' texstuff/manual.dvi
	rm -f htmlman/manual.html
	tar cf - htmlman/*.* | gzip > $(RELEASE)refman.html.tar.gz
	zip -8 $(RELEASE)refman.html.zip htmlman/*.*
	cp texstuff/pdfmanual.pdf $(RELEASE)refman.pdf
	cp textman/manual.txt $(RELEASE)refman.txt
	tar cf - infoman/ocaml* | gzip > $(RELEASE)refman.info.tar.gz

.SUFFIXES:
.SUFFIXES: .tex .etex .htex

.etex.tex:
	../tools/texquote2 < $*.etex > $*.tex

version.tex: $(CSLDIR)/utils/config.mlp
	sed -n -e 's/^let version = "\(.*\)".*$$/\\def\\ocamlversion{\1}/p' $(CSLDIR)/utils/config.mlp > version.tex
