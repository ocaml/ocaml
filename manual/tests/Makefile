ROOTDIR = ../..
include $(ROOTDIR)/api_docgen/Makefile.docfiles
include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/stdlib/StdlibModules
include $(ROOTDIR)/Makefile.best_binaries
STDLIBFLAGS = -nostdlib -I $(ROOTDIR)/stdlib
OCAMLC ?= $(BEST_OCAMLC) $(STDLIBFLAGS)

MANUAL=$(ROOTDIR)/manual/src

.PHONY: all
all: check-cross-references check-stdlib check-case-collision

.PHONY: tools
tools: cross-reference-checker

cross-reference-checker: cross_reference_checker.ml
	$(OCAMLC) $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
	  -I $(ROOTDIR)/parsing -I $(ROOTDIR)/driver \
	  $< -o $@

# check cross-references between the manual and error messages
.PHONY: check-cross-references
check-cross-references: cross-reference-checker
	$(OCAMLRUN) ./cross-reference-checker \
	  -auxfile $(MANUAL)/texstuff/manual.aux \
	  $(ROOTDIR)/utils/warnings.ml \
	  $(ROOTDIR)/driver/main_args.ml \
	  $(ROOTDIR)/lambda/translmod.ml

# check that all standard library modules are referenced by the
# standard library chapter of the manual
.PHONY: check-stdlib
check-stdlib:
	./check-stdlib-modules $(ROOTDIR)


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
