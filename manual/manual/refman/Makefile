FILES= refman.tex lex.tex names.tex values.tex const.tex types.tex \
  patterns.tex expr.tex typedecl.tex modtypes.tex modules.tex compunit.tex
# directive.tex intf.tex impl.tex exten.tex

TRANSF=../../tools/transf
TEXQUOTE=../../tools/texquote2

all: $(FILES)

clean:
	rm -f $(FILES)

.SUFFIXES:
.SUFFIXES: .etex .tex

.etex.tex:
	$(TRANSF) < $*.etex | $(TEXQUOTE) > $*.tex

$(FILES): $(TRANSF) $(TEXQUOTE)
