FILES= coreexamples.tex lablexamples.tex objectexamples.tex moduleexamples.tex\
advexamples.tex polymorphism.tex

TOPDIR=../../..
include $(TOPDIR)/Makefile.tools

LD_PATH="$(TOPDIR)/otherlibs/str:$(TOPDIR)/otherlibs/unix"

CAMLLATEX=$(SET_LD_PATH) $(OCAMLRUN) ../../tools/caml-tex2
TEXQUOTE=../../tools/texquote2

ALLFILES=$(FILES)

etex-files: $(ALLFILES)
all: $(ALLFILES)

clean:
	rm -f $(ALLFILES)

.SUFFIXES:
.SUFFIXES: .etex .tex

.etex.tex:
	@$(CAMLLATEX) -caml "TERM=norepeat $(OCAML)" -n 80 -v false\
                       -o $*.caml_tex_error.tex $*.etex\
	&& mv $*.caml_tex_error.tex $*.gen.tex\
	&& $(TEXQUOTE) < $*.gen.tex > $*.texquote_error.tex\
	&& mv $*.texquote_error.tex $*.tex\
	|| printf "Failure when generating %s\n" $*.tex

$(ALLFILES): ../../tools/caml-tex2 $(TEXQUOTE)
