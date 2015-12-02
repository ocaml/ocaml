FILES= coreexamples.tex lablexamples.tex objectexamples.tex moduleexamples.tex advexamples.tex

CAMLLATEX=../../tools/caml-tex2
TEXQUOTE=../../tools/texquote2

ALLFILES=$(FILES)

all: $(ALLFILES)

clean:
	rm -f $(ALLFILES)

.SUFFIXES:
.SUFFIXES: .etex .tex

.etex.tex:
	$(CAMLLATEX) -n 80 -o - $*.etex | $(TEXQUOTE) > $*.tex

$(ALLFILES): $(CAMLLATEX) $(TEXQUOTE)
