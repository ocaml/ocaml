default:
	@$(OCAMLC) -o program.byte alloc.ml
	@./program.byte
	@$(OCAMLOPT) -o program.native alloc.ml
	@./program.native

clean: defaultclean
	@rm -fr program.*

include ../../makefiles/Makefile.common
