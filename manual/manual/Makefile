FILES=allfiles.tex biblio.tex foreword.tex version.tex
TEXINPUTS=.:..:../refman:../library:../cmds:../tutorials:../../styles:
TEXFONTS=../../styles:
RELEASE=$$HOME/release/$${RELEASENAME}
CSLDIR=../../csl
HEVEA=hevea -v -fix -exec xxdate.exe
HACHA=hacha
INFO=-info -w 79
HTML=
TEXT=-text -w 79

CSLDIR2=../$(CSLDIR)
MLIS=$(CSLDIR2)/stdlib/*.mli \
	$(CSLDIR2)/otherlibs/bigarray/bigarray.mli \
	$(CSLDIR2)/otherlibs/dbm/dbm.mli \
	$(CSLDIR2)/otherlibs/dynlink/dynlink.mli \
	$(CSLDIR2)/otherlibs/graph/graphics.mli \
	$(CSLDIR2)/otherlibs/graph/graphicsX11.mli \
	$(CSLDIR2)/otherlibs/num/num.mli \
	$(CSLDIR2)/otherlibs/num/arith_status.mli \
	$(CSLDIR2)/otherlibs/num/big_int.mli \
	$(CSLDIR2)/otherlibs/str/*.mli \
	$(CSLDIR2)/otherlibs/systhreads/*.mli \
	$(CSLDIR2)/otherlibs/unix/*.mli \
	../library/tk.mli

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
	mkdir -p libref ; \
	ocamldoc -colorize-code -sort -html \
	-d libref \
	-I $(CSLDIR2)/stdlib  \
	-I $(CSLDIR2)/otherlibs/bigarray \
	-I $(CSLDIR2)/otherlibs/dbm \
	-I $(CSLDIR2)/otherlibs/dynlink \
	-I $(CSLDIR2)/otherlibs/graph \
	-I $(CSLDIR2)/otherlibs/num \
	-I $(CSLDIR2)/otherlibs/str \
	-I $(CSLDIR2)/otherlibs/systhreads \
	-I $(CSLDIR2)/otherlibs/unix \
	-I +labltk \
	$(MLIS) ; \
	${HEVEA} ${HTML} -I .. -I ../refman -I ../library -I ../cmds -I ../tutorials -I ../../styles -I ../texstuff manual.hva -e macros.tex ../manual.tex ;\
	${HACHA} manual.html ;\


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
	cd htmlman; rm -f index.html manual*.html *.haux *.hind
	cd textman; rm -f manual.txt *.haux *.hind
	cd infoman; rm -f ocaml.info ocaml.info-*  *.haux *.hind

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
