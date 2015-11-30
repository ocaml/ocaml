FILES= refman.tex lex.tex names.tex values.tex const.tex types.tex \
  patterns.tex expr.tex typedecl.tex modtypes.tex modules.tex compunit.tex \
  exten.tex classes.tex

TRANSF=../../tools/transf
TEXQUOTE=../../tools/texquote2

ALLFILES=$(FILES)

all: $(ALLFILES)

clean:
	rm -f $(ALLFILES)

.SUFFIXES:
.SUFFIXES: .etex .tex

.etex.tex:
	$(TRANSF) < $*.etex | $(TEXQUOTE) > $*.tex

$(ALLFILES): $(TRANSF) $(TEXQUOTE)
