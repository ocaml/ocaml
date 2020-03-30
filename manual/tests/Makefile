TOPDIR=$(abspath ../..)
include $(TOPDIR)/Makefile.tools
MANUAL=$(TOPDIR)/manual/manual

.PHONY: all
all: check-cross-references check-stdlib

cross-reference-checker: cross_reference_checker.ml
	$(OCAMLC) $(TOPDIR)/compilerlibs/ocamlcommon.cma -I $(TOPDIR)/parsing \
         -I $(TOPDIR)/driver \
        cross_reference_checker.ml -o cross-reference-checker

check-cross-references: cross-reference-checker
	$(OCAMLRUN) ./cross-reference-checker \
	-auxfile $(MANUAL)/texstuff/manual.aux \
        $(TOPDIR)/utils/warnings.ml \
	$(TOPDIR)/bytecomp/translmod.ml

check-stdlib:
	./check-stdlib-modules $(TOPDIR)
