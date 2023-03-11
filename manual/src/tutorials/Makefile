ROOTDIR = ../../..
include $(ROOTDIR)/Makefile.common

LD_PATH = $(ROOTDIR)/otherlibs/str $(ROOTDIR)/otherlibs/unix

TOOLS = ../../tools
CAMLLATEX = $(OCAMLRUN) $(addprefix -I ,$(LD_PATH)) \
  $(ROOTDIR)/tools/ocamltex -repo-root $(ROOTDIR) -n 80 -v false
TEXQUOTE = $(OCAMLRUN) $(TOOLS)/texquote2
TRANSF = $(OCAMLRUN) $(TOOLS)/transf


FILES = coreexamples.tex lablexamples.tex polyvariant.tex objectexamples.tex \
        gadtexamples.tex moduleexamples.tex advexamples.tex polymorphism.tex \
        parallelism.tex memorymodel.tex


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

.PHONY: distclean
distclean: clean
