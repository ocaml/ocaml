default: compile run

compile: tscanf2_io.cmo tscanf2_io.cmx
	@$(OCAMLC) unix.cma tscanf2_io.cmo -o master.bytecode tscanf2_master.ml
	@$(OCAMLC) tscanf2_io.cmo -o slave.bytecode tscanf2_slave.ml
	@$(OCAMLOPT) unix.cmxa tscanf2_io.cmx -o master.native tscanf2_master.ml
	@$(OCAMLOPT) tscanf2_io.cmx -o slave.native tscanf2_slave.ml

run:
	@echo -n " ... testing with ocamlc"
	@./master.bytecode ./slave.bytecode > result.bytecode 2>&1
	@diff -q reference result.bytecode > /dev/null || (echo " => failed" && exit 1)
	@echo -n " ocamlopt"
	@./master.native ./slave.native > result.native 2>&1
	@diff -q reference result.native > /dev/null || (echo " => failed" && exit 1)
	@echo " => passed"

clean: defaultclean
	@rm -f master.* slave.* result.*

include ../../makefiles/Makefile.common
