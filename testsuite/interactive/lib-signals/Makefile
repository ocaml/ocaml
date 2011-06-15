BASEDIR=../..

default:
	@$(OCAMLC) -o program.byte signals.ml
	@./program.byte
	@$(OCAMLOPT) -o program.native signals.ml
	@./program.native

clean: defaultclean
	@rm -fr program.*

include $(BASEDIR)/makefiles/Makefile.common
