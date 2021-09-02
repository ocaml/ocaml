ROOTDIR = ../../..

CSLDIR = $(ROOTDIR)

TEXQUOTE = $(ROOTDIR)/runtime/ocamlrun ../../tools/texquote2

FILES = core.tex builtin.tex stdlib-blurb.tex compilerlibs.tex \
  libunix.tex libstr.tex old.tex libthreads.tex libdynlink.tex

etex-files: $(FILES)
all: etex-files

%.tex: %.etex
	$(TEXQUOTE) < $< > $*.texquote_error.tex
	mv $*.texquote_error.tex $@


.PHONY: clean
clean:
	rm -f *.tex ocamldoc.out ocamldoc.sty
