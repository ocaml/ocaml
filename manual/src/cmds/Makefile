ROOTDIR = ../../..
include $(ROOTDIR)/Makefile.common

LD_PATH = $(ROOTDIR)/otherlibs/str $(ROOTDIR)/otherlibs/unix

TOOLS = ../../tools
CAMLLATEX = $(OCAMLRUN) $(addprefix -I ,$(LD_PATH)) \
  $(ROOTDIR)/tools/caml-tex -repo-root $(ROOTDIR) -n 80 -v false
TEXQUOTE = $(OCAMLRUN) $(TOOLS)/texquote2
TRANSF = $(OCAMLRUN) $(TOOLS)/transf

FILES = comp.tex top.tex runtime.tex native.tex lexyacc.tex intf-c.tex \
  ocamldep.tex profil.tex debugger.tex ocamldoc.tex \
  warnings-help.tex flambda.tex tail-mod-cons.tex \
  afl-fuzz.tex runtime-tracing.tex unified-options.tex

etex-files: $(FILES)
all: $(FILES)

%.gen.tex: %.etex
	$(CAMLLATEX) $< -o $*_camltex.tex
	$(TRANSF) < $*_camltex.tex > $*.transf_error.tex
	mv $*.transf_error.tex $@

%.tex: %.gen.tex
	$(TEXQUOTE) < $< > $*.texquote_error.tex
	mv $*.texquote_error.tex $@

.PHONY: clean
clean:
	rm -f *.tex

.PHONY: distclean
distclean: clean
