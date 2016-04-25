FILES= coreexamples.tex lablexamples.tex objectexamples.tex moduleexamples.tex advexamples.tex

TOPDIR=../../..
include $(TOPDIR)/Makefile.tools

CAMLLATEX= $(OCAMLRUN) ../../tools/caml-tex2
TEXQUOTE=../../tools/texquote2

ALLFILES=$(FILES)

all: $(ALLFILES)

clean:
	rm -f $(ALLFILES)

.SUFFIXES:
.SUFFIXES: .etex .tex

.etex.tex:
	$(CAMLLATEX) -caml "TERM=norepeat $(OCAML)" -n 80 -o - $*.etex | $(TEXQUOTE) > $*.tex

$(ALLFILES): $(CAMLLATEX) $(TEXQUOTE)
