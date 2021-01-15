TOPDIR = ../../..
include $(TOPDIR)/Makefile.tools

LD_PATH = "$(TOPDIR)/otherlibs/str:$(TOPDIR)/otherlibs/unix"

TOOLS = ../../tools
CAMLLATEX = $(SET_LD_PATH) \
  $(OCAMLRUN) $(TOPDIR)/tools/caml-tex \
  -repo-root $(TOPDIR) -n 80 -v false
TEXQUOTE = $(OCAMLRUN) $(TOOLS)/texquote2
TRANSF = $(SET_LD_PATH) $(OCAMLRUN) $(TOOLS)/transf

FILES = comp.tex top.tex runtime.tex native.tex lexyacc.tex intf-c.tex \
  ocamldep.tex profil.tex debugger.tex ocamldoc.tex \
  warnings-help.tex flambda.tex \
  afl-fuzz.tex instrumented-runtime.tex unified-options.tex

WITH_TRANSF = top.tex intf-c.tex flambda.tex \
  afl-fuzz.tex lexyacc.tex debugger.tex

WITH_CAMLEXAMPLE = instrumented-runtime.tex ocamldoc.tex


etex-files: $(FILES)
all: $(FILES)


%.tex: %.etex
	$(TEXQUOTE) < $< > $*.texquote_error.tex
	mv $*.texquote_error.tex $@

$(WITH_TRANSF): %.tex: %.etex
	$(TRANSF) < $< > $*.transf_error.tex
	mv $*.transf_error.tex $*.transf_gen.tex
	$(TEXQUOTE) < $*.transf_gen.tex > $*.texquote_error.tex
	mv $*.texquote_error.tex $@

$(WITH_CAMLEXAMPLE): %.tex: %.etex
	$(CAMLLATEX) $< -o $*.gen.tex
	$(TRANSF) < $*.gen.tex > $*.transf_error.tex
	mv $*.transf_error.tex $*.transf_gen.tex
	$(TEXQUOTE) < $*.transf_gen.tex > $*.texquote_error.tex
	mv $*.texquote_error.tex $@

warnings-help.etex: ../warnings-help.etex
	cp $< $@


.PHONY: clean
clean:
	rm -f *.tex
	rm -f warnings-help.etex
