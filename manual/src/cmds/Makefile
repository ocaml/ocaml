ROOTDIR = ../../..
include $(ROOTDIR)/Makefile.common

LD_PATH = $(ROOTDIR)/otherlibs/str $(ROOTDIR)/otherlibs/unix

TOOLS = ../../tools
CAMLLATEX = $(OCAMLRUN) $(addprefix -I ,$(LD_PATH)) \
  $(ROOTDIR)/tools/ocamltex -repo-root $(ROOTDIR) -n 80 -v false
TEXQUOTE = $(OCAMLRUN) $(TOOLS)/texquote2
TRANSF = $(OCAMLRUN) $(TOOLS)/transf

FILES = afl-fuzz.tex comp.tex debugger.tex flambda.tex intf-c.tex  \
        lexyacc.tex native.tex native-debugger.tex ocamldep.tex ocamldoc.tex \
	profil.tex runtime-tracing.tex runtime.tex tail-mod-cons.tex \
	top.tex tsan.tex unified-options.tex warnings-help.tex

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
