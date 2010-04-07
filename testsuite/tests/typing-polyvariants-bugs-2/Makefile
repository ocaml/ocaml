default:
	@printf " ... testing 'pr3918':"
	@($(OCAMLC) -c pr3918a.mli && $(OCAMLC) -c pr3918b.mli && $(OCAMLC) -c pr3918c.ml && echo " => passed") || echo " => failed"

clean: defaultclean

include ../../makefiles/Makefile.common
