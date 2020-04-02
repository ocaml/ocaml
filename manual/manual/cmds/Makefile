FILES=comp.tex top.tex runtime.tex native.tex lexyacc.tex intf-c.tex \
  depend.tex profil.tex debugger.tex browser.tex ocamldoc.tex \
  warnings-help.tex ocamlbuild.tex flambda.tex spacetime.tex \
  afl-fuzz.tex plugins.tex unified-options.tex

TOPDIR=../../..
include $(TOPDIR)/Makefile.tools

LD_PATH="$(TOPDIR)/otherlibs/str:$(TOPDIR)/otherlibs/unix"

TRANSF=$(SET_LD_PATH) $(OCAMLRUN) ../../tools/transf
TEXQUOTE=../../tools/texquote2
FORMAT=../../tools/format-intf

CAMLLATEX=$(SET_LD_PATH) $(OCAMLRUN) ../../tools/caml-tex2 \
-caml "TERM=norepeat $(OCAML)" -n 80 -v false

WITH_TRANSF= top.tex intf-c.tex flambda.tex spacetime.tex \
  afl-fuzz.tex lexyacc.tex debugger.tex

WITH_CAMLEXAMPLE = ocamldoc.tex

etex-files: $(FILES)

all: $(FILES)

clean::
	rm -f $(FILES)
	rm -f *~ #*#

.SUFFIXES:
.SUFFIXES: .tex .etex

.etex.tex:
	@$(TEXQUOTE) < $*.etex > $*.texquote_error.tex\
	&& mv $*.texquote_error.tex $*.tex\
	|| printf "Failure when generating %s\n" $*.tex

$(WITH_TRANSF):%.tex:%.etex
	@$(TRANSF) < $*.etex > $*.transf_error.tex \
	&& mv $*.transf_error.tex $*.transf_gen.tex \
	&& $(TEXQUOTE) < $*.transf_gen.tex > $*.texquote_error.tex \
	&& mv $*.texquote_error.tex $*.tex \
	|| printf "Failure when generating %s\n" $*.tex


$(WITH_CAMLEXAMPLE):%.tex:%.etex
	@$(CAMLLATEX) -o $*.caml_tex_error.tex $*.etex \
	&& mv $*.caml_tex_error.tex $*.gen.tex \
	&& $(TRANSF) < $*.gen.tex > $*.transf_error.tex \
	&& mv $*.transf_error.tex $*.gen.tex\
	&& $(TEXQUOTE) < $*.gen.tex  > $*.texquote_error.tex\
	&& mv $*.texquote_error.tex $*.tex\
	|| printf "Failure when generating %s\n" $*.tex

warnings-help.etex: ../warnings-help.etex
	cp ../warnings-help.etex .

clean::
	rm -f warnings-help.etex
