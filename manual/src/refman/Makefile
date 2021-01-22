TOPDIR = ../../..
include $(TOPDIR)/Makefile.tools

LD_PATH = "$(TOPDIR)/otherlibs/str:$(TOPDIR)/otherlibs/unix"

TOOLS = ../../tools
CAMLLATEX = $(SET_LD_PATH) \
  $(OCAMLRUN) $(TOPDIR)/tools/caml-tex \
  -repo-root $(TOPDIR) -n 80 -v false
TEXQUOTE = $(OCAMLRUN) $(TOOLS)/texquote2
TRANSF = $(SET_LD_PATH) $(OCAMLRUN) $(TOOLS)/transf

EXTENSION_FILES = letrecvalues.tex recursivemodules.tex locallyabstract.tex \
  firstclassmodules.tex moduletypeof.tex signaturesubstitution.tex \
  modulealias.tex overridingopen.tex gadts.tex bigarray.tex \
  attributes.tex extensionnodes.etex extensiblevariants.tex \
  generativefunctors.tex extensionsyntax.tex inlinerecords.tex \
  doccomments.tex indexops.tex emptyvariants.tex alerts.tex \
  generalizedopens.tex bindingops.tex extensionnodes.tex

FILES =  $(addprefix extensions/,$(EXTENSION_FILES)) \
  refman.tex lex.tex names.tex values.tex const.tex types.tex \
  patterns.tex expr.tex typedecl.tex modtypes.tex modules.tex compunit.tex \
  exten.tex classes.tex


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
	rm -f extensions/*.tex
