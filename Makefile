include Makefile.ocaml

ASMTOP=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) \
 driver/errors.cmo driver/compile.cmo toplevel/genprintval.cmo

toplevellib.cmxa: $(ASMTOP:.cmo=.cmx)
	$(CAMLOPT) -a -o toplevellib.cmxa $(ASMTOP:.cmo=.cmx)
	cp toplevellib.cmxa toplevellib.a $(LIBDIR)/

INTERFACES=parsing/asttypes.cmi

install_top:
	mkdir -p $(LIBDIR)/ocamlsrc
	cp -f $(ASMTOP:.cmo=.cmi) $(COMPOBJS:.cmo=.cmi) \
               $(INTERFACES) $(LIBDIR)/ocamlsrc