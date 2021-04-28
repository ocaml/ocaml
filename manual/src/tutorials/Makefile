ROOTDIR = ../../..
include $(ROOTDIR)/Makefile.common

LD_PATH = "$(ROOTDIR)/otherlibs/str:$(ROOTDIR)/otherlibs/unix"

TOOLS = ../../tools
CAMLLATEX = $(SET_LD_PATH) \
  $(OCAMLRUN) $(ROOTDIR)/tools/caml-tex \
  -repo-root $(ROOTDIR) -n 80 -v false
TEXQUOTE = $(OCAMLRUN) $(TOOLS)/texquote2
TRANSF = $(SET_LD_PATH) $(OCAMLRUN) $(TOOLS)/transf


FILES = coreexamples.tex lablexamples.tex objectexamples.tex \
  moduleexamples.tex advexamples.tex polymorphism.tex


etex-files: $(FILES)
all: $(FILES)


%.gen.tex: %.etex
	$(CAMLLATEX) $< -o $@

%.tex: %.gen.tex
	$(TEXQUOTE) < $< > $*.texquote_error.tex
	mv $*.texquote_error.tex $@


.PHONY: clean
clean:
	rm -f *.tex
