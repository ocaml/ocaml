FILES=allfiles.tex biblio.tex foreword.tex version.tex
DVI2TXT=../tools/dvi2txt
TEXINPUTS=.:..:../refman:../library:../cmds:../../styles:/usr/local/lib/tex/inputs
TEXFONTS=../../styles:/usr/local/lib/tex/fonts
RELEASE=$$HOME/release/$${RELEASENAME}
CSLDIR=../../csl

manual: files
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) latex manual.tex

index::
	cd texstuff; makeindex manual.idx
	cd texstuff; makeindex manual.kwd.idx

#plaintext: files
#	cd texstuff; \
#	TEXINPUTS=$(TEXINPUTS) TEXFONTS=$(TEXFONTS) latex plaintext.tex
#	$(DVI2TXT) texstuff/plaintext.dvi > texstuff/plaintext.txt
#	$(DVI2TXT) -p texstuff/plaintext.dvi > texstuff/plaintext.ipr

#index::
#	cd texstuff; makeindex plaintext.idx
#	cd texstuff; makeindex plaintext.kwd.idx

files: $(FILES)
	cd refman; make all
	cd library; make all
	cd cmds; make all

all:
	make manual
	make manual
	make index
	make manual

clean:
	rm $(FILES)
	cd refman; make clean
	cd library; make clean
	cd cmds; make clean
	rm -f texstuff/*

release:
	gzip < texstuff/manual.dvi > $(RELEASE)refman.dvi.gz
	dvips -Pps -o '!gzip > $(RELEASE)refman.ps.gz' texstuff/manual.dvi
#	cp texstuff/plaintext.txt $(RELEASE)refman.txt
#	cp texstuff/plaintext.ipr $(RELEASE)refman.prn

.SUFFIXES:
.SUFFIXES: .tex .etex

.etex.tex:
	../tools/texquote2 < $*.etex > $*.tex

version.tex: $(CSLDIR)/utils/config.mlp
	sed -n -e 's/^let version = "\(.*\)".*$$/\\def\\cslversion{\1}/p' $(CSLDIR)/utils/config.mlp > version.tex
