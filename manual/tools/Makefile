TOPDIR=../..
COMPFLAGS=-I $(OTOPDIR)/otherlibs/str -I $(OTOPDIR)/otherlibs/unix
include $(TOPDIR)/Makefile.tools

all: texquote2 transf caml-tex2


transf: transf.cmo htmltransf.cmo transfmain.cmo
	$(OCAMLC) -o $@ -g $^

transfmain.cmo: transf.cmo htmltransf.cmo


caml-tex2: caml_tex2.ml
	$(OCAMLC) $(TOPDIR)/compilerlibs/ocamlcommon.cma -I $(TOPDIR)/parsing \
	  -o $@ str.cma unix.cma $<

texquote2: texquote2.ml
	$(OCAMLC) -o $@ $<


%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

%.ml: %.mll
	$(OCAMLLEX) $<


.PHONY: clean
clean:
	rm -f *.o *.cm? *.cmx?
	rm -f transf.ml htmltransf.ml
	rm -f texquote2 transf caml-tex2
