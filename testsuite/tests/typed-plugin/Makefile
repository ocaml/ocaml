# Ensure that calling an external C primite forces linking
# the module that defines it

MAIN_MODULE = main
#LIBRARIES = for_rewrite

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common

main.cmo: pptx_example.cmo for_rewrite.cmo
#main.cmx: lib.cmxa

pptx_example.cmo: pptx_example.ml
	$(OCAMLC) -g -o pptx_example.cmxs -I $(TOPDIR)/driver -I $(TOPDIR)/parsing \
	-I $(TOPDIR)/typing -I $(TOPDIR)/utils -I $(TOPDIR)/compilerlibs \
		ocamlcommon.cma $<

for_rewrite.cmo: for_rewrite.ml
	@$(OCAMLC) -c -plugin pptx_example.cmo -dsource -o $@ $<

#lib.cmxa: file.cmx
#	@$(OCAMLOPT) -a -o $@ $<
