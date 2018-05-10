TOPDIR=$(abspath ../..)
include $(TOPDIR)/Makefile.tools
MANUAL=$(TOPDIR)/manual/manual

all: check-cross-references check-stdlib


cross-reference-checker: cross_reference_checker.ml
	$(OCAMLC) $(TOPDIR)/compilerlibs/ocamlcommon.cma \
	  -I $(TOPDIR)/parsing -I $(TOPDIR)/driver \
	  $< -o $@

.PHONY: check-cross-references
check-cross-references: cross-reference-checker
	$(OCAMLRUN) ./cross-reference-checker \
	  -auxfile $(MANUAL)/texstuff/manual.aux \
	  $(TOPDIR)/utils/warnings.ml \
	  $(TOPDIR)/bytecomp/translmod.ml

.PHONY: check-stdlib
check-stdlib:
	./check-stdlib-modules $(TOPDIR)


.PHONY: clean
clean:
	rm -f *.cm? *.cmx? cross-reference-checker
