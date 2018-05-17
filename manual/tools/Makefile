TOPDIR=../..
COMPFLAGS=-I $(OTOPDIR)/otherlibs/str -I $(OTOPDIR)/otherlibs/unix
CAMLTEX2_FLAGS= -I $(OTOPDIR)/parsing -I $(OTOPDIR)/utils \
          -I $(OTOPDIR)/driver -I $(OTOPDIR)/toplevel

CAMLTEX2_LIBS = $(TOPDIR)/compilerlibs/ocamlcommon.cma \
 $(TOPDIR)/compilerlibs/ocamlbytecomp.cma \
 $(TOPDIR)/compilerlibs/ocamltoplevel.cma


include $(TOPDIR)/Makefile.tools

all: texquote2 transf caml-tex2


transf: transf.cmo htmltransf.cmo transfmain.cmo
	$(OCAMLC) -o $@ -g $^

transfmain.cmo: transf.cmo htmltransf.cmo

COMPILER_CMAS=common bytecomp toplevel
COMPILER_LIBS=$(addprefix $(TOPDIR)/compilerlibs/ocaml, $(COMPILER_CMAS) )
COMPILER_CMIS = $(addprefix  -I $(TOPDIR)/, utils parsing driver toplevel)

caml-tex2: caml_tex2.ml
	$(OCAMLC) $(addsuffix .cma, $(COMPILER_LIBS)) $(COMPILER_CMIS) \
	  -o $@ str.cma unix.cma -linkall $<

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
