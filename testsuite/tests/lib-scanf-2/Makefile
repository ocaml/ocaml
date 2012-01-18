BASEDIR=../..

default: compile run

compile: tscanf2_io.cmo tscanf2_io.cmx
	@$(OCAMLC) unix.cma tscanf2_io.cmo -o master.byte tscanf2_master.ml
	@$(OCAMLC) tscanf2_io.cmo -o slave.byte tscanf2_slave.ml
	@$(OCAMLOPT) unix.cmxa tscanf2_io.cmx -o master.native tscanf2_master.ml
	@$(OCAMLOPT) tscanf2_io.cmx -o slave.native tscanf2_slave.ml

run:
	@printf " ... testing with ocamlc"
	@./master.byte ./slave.byte > result.byte 2>&1
	@$(DIFF) reference result.byte > /dev/null || (echo " => failed" && exit 1)
	@printf " ocamlopt"
	@./master.native ./slave.native > result.native 2>&1
	@$(DIFF) reference result.native > /dev/null || (echo " => failed" && exit 1)
	@echo " => passed"

promote:
	@cp result.byte reference

clean: defaultclean
	@rm -f master.* slave.* result.*

include $(BASEDIR)/makefiles/Makefile.common
