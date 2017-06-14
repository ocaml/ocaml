TOPDIR=$(abspath ../..)
include $(TOPDIR)/Makefile.tools

.SUFFIXES:

OCAMLDEP= $(OCAMLRUN) $(TOPDIR)/tools/ocamldep -slash
OCAMLC_SNP= $(OCAMLRUN) $(TOPDIR)/ocamlc -nostdlib -nopervasives -I $(HERE)

pervasives.cmi: pervasives.mli camlinternalFormatBasics.cmi
	$(OCAMLC_SNP) -c $<

camlinternalFormatBasics.cmi: \
camlinternalFormatBasics.mli
	$(OCAMLC_SNP) -c $<

%.cmi: %.mli pervasives.cmi
	$(OCAMLC_SNP) -c -open Pervasives $<

depend:
	$(OCAMLDEP) *.mli > .depend

include .depend

clean:
	rm *.mli *.cmi
