FILES=comp.tex top.tex runtime.tex native.tex lexyacc.tex intf-c.tex \
  depend.tex profil.tex

TRANSF=../../tools/transf
TEXQUOTE=../../tools/texquote2
FORMAT=../../tools/format-intf

all: $(FILES) $(FILES:.tex=.htex)

clean:
	rm -f $(FILES) $(FILES:.tex=.htex)
	rm -f *~ #*#

.SUFFIXES:
.SUFFIXES: .tex .etex .htex

.etex.tex:
	$(TEXQUOTE) < $*.etex > $*.tex

.etex.htex:
	cp $*.etex $*.htex

top.tex: top.etex $(TRANSF)
	$(TRANSF) < top.etex | $(TEXQUOTE) > top.tex

lexyacc.tex: lexyacc.etex $(TRANSF)
	$(TRANSF) < lexyacc.etex | $(TEXQUOTE) > lexyacc.tex

top.htex: top.etex $(TRANSF)
	$(TRANSF) -html < top.etex > top.htex

lexyacc.htex: lexyacc.etex $(TRANSF)
	$(TRANSF) -html < lexyacc.etex > lexyacc.htex
