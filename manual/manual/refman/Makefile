FILES= refman.tex lex.tex names.tex values.tex const.tex types.tex \
  patterns.tex expr.tex typedecl.tex modtypes.tex modules.tex compunit.tex \
  exten.tex objects.tex

TRANSF=../../tools/transf
TEXQUOTE=../../tools/texquote2

ALLFILES=$(FILES) $(FILES:.tex=.htex)

all: $(ALLFILES)

clean:
	rm -f $(ALLFILES)

.SUFFIXES:
.SUFFIXES: .etex .tex .htex

.etex.tex:
	$(TRANSF) < $*.etex | $(TEXQUOTE) > $*.tex

.etex.htex:
	$(TRANSF) -html < $*.etex > $*.htex

$(ALLFILES): $(TRANSF) $(TEXQUOTE)
