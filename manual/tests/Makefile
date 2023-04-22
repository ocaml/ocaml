ROOTDIR = ../..
include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/api_docgen/Makefile.docfiles
include $(ROOTDIR)/stdlib/StdlibModules
include $(ROOTDIR)/Makefile.best_binaries
OCAMLC ?= $(BEST_OCAMLC) $(STDLIBFLAGS)

MANUAL=$(ROOTDIR)/manual/src

.PHONY: all
all: check-cross-references check-stdlib check-case-collision

.PHONY: tools
tools: cross-reference-checker

cross_reference_checker.cmo: cross_reference_checker.ml
	$(OCAMLC) -I $(ROOTDIR)/parsing -I $(ROOTDIR)/driver -o $@ -c $<

cross-reference-checker: cross_reference_checker.cmo
	$(OCAMLC) $(ROOTDIR)/compilerlibs/ocamlcommon.cma -o $@ $<

# check cross-references between the manual and error messages
.PHONY: check-cross-references
check-cross-references: cross-reference-checker
	$(OCAMLRUN) ./cross-reference-checker \
	  -auxfile $(MANUAL)/texstuff/manual.aux \
	  $(TOPSRCDIR)/utils/warnings.ml \
	  $(TOPSRCDIR)/driver/main_args.ml \
	  $(TOPSRCDIR)/lambda/translmod.ml \
	  $(TOPSRCDIR)/typing/typemod.ml \
	  $(TOPSRCDIR)/typing/typeclass.ml

# check that all standard library modules are referenced by the
# standard library chapter of the manual
.PHONY: check-stdlib
check-stdlib:
	$(SRCDIR)/check-stdlib-modules $(TOPSRCDIR)


# check name collision between latex source file and module documentation
# on case-insensitive file systems
normalize = $(shell echo $(basename $(notdir $(1) )) | tr A-Z a-z)
LOWER_MLIS= $(call normalize,$(ALL_DOC:%=%.mli))
LOWER_ETEX= $(call normalize,$(wildcard $(MANUAL)/*/*.etex) $(wildcard *.etex))
INTER = $(filter $(LOWER_ETEX), $(LOWER_MLIS))

.PHONY: check-case-collision
check-case-collision:
ifeq ($(INTER),)
	@echo "No collisions detected between OCaml modules and LaTeX sources."
else
	@echo "The following names"
	@echo "  $(INTER)"
	@echo "are used by both an OCaml module and a latex source file."
	@echo "This creates a conflict on case-insensitive file systems."
	@false
endif


.PHONY: clean
clean:
	rm -f *.cm? *.cmx? cross-reference-checker

.PHONY: distclean
distclean: clean
