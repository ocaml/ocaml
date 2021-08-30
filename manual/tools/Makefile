ROOTDIR = ../..
include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

STDLIBFLAGS = -nostdlib -I $(ROOTDIR)/stdlib
OCAMLC ?= $(BEST_OCAMLC) $(STDLIBFLAGS)

all: texquote2 transf


transf: transf.cmo transfmain.cmo
	$(OCAMLC) $(COMPFLAGS) -o $@ -g $^

transfmain.cmo: transf.cmo

texquote2: texquote2.ml
	$(OCAMLC) $(COMPFLAGS) -o $@ $<

%.cmo: %.ml
	$(OCAMLC) $(COMPFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(COMPFLAGS) -c $<
.PHONY: clean
clean:
	rm -f *.o *.cm? *.cmx?
	rm -f transf.ml
	rm -f texquote2 transf
