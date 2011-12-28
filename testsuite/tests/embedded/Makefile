BASEDIR=../..

default: compile run

compile:
	@$(OCAMLC) -ccopt -I -ccopt $(TOPDIR)/byterun cmstub.c
	@$(OCAMLC) -ccopt -I -ccopt $(TOPDIR)/byterun cmmain.c
	@$(OCAMLC) -c cmcaml.ml
	@$(OCAMLC) -custom -o program cmstub.o cmcaml.cmo cmmain.o

run:
	@printf " ... testing 'cmmain':"
	@./program > program.result
	@$(DIFF) program.reference program.result > /dev/null || (echo " => failed" && exit 1)
	@echo " => passed"
	
promote: defaultpromote

clean: defaultclean
	@rm -f *.result ./program

include $(BASEDIR)/makefiles/Makefile.common
