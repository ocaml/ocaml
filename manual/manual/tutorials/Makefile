FILES= coreexamples.tex objectexamples.tex moduleexamples.tex

CAMLLATEX=../../tools/caml-tex
TEXQUOTE=../../tools/texquote2

ALLFILES=$(FILES) $(FILES:.tex=.htex)

all: $(ALLFILES)

clean:
	rm -f $(ALLFILES)

.SUFFIXES:
.SUFFIXES: .etex .tex .htex

.etex.tex:
	$(CAMLLATEX) -o - $*.etex | $(TEXQUOTE) > $*.tex

.etex.htex:
	$(CAMLLATEX) -o $*.htex $*.etex

$(ALLFILES): $(CAMLLATEX) $(TEXQUOTE)
