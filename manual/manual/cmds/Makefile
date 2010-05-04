FILES=comp.tex top.tex runtime.tex native.tex lexyacc.tex intf-c.tex \
  depend.tex profil.tex debugger.tex browser.tex ocamldoc.tex warnings.tex

TRANSF=../../tools/transf
TEXQUOTE=../../tools/texquote2
FORMAT=../../tools/format-intf

all: $(FILES)

clean:
	rm -f $(FILES)
	rm -f *~ #*#

.SUFFIXES:
.SUFFIXES: .tex .etex

.etex.tex:
	$(TEXQUOTE) < $*.etex > $*.tex

top.tex: top.etex $(TRANSF)
	$(TRANSF) < top.etex | $(TEXQUOTE) > top.tex

lexyacc.tex: lexyacc.etex $(TRANSF)
	$(TRANSF) < lexyacc.etex | $(TEXQUOTE) > lexyacc.tex

debugger.tex: debugger.etex $(TRANSF)
	$(TRANSF) < debugger.etex | $(TEXQUOTE) > debugger.tex

warnings.tex: ../warnings.tex
	cp ../warnings.tex .
