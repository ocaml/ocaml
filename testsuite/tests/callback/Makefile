BASEDIR=../..

CC=$(NATIVECC) -I $(TOPDIR)/byterun

default: run-byte run-opt

common:
	@$(CC) -c callbackprim.c

run-byte: common
	@printf " ... testing 'bytecode':"
	@$(OCAMLC) -c tcallback.ml
	@$(OCAMLC) -o ./program -custom unix.cma callbackprim.$(O) tcallback.cmo
	@./program > bytecode.result
	@$(DIFF) reference bytecode.result || (echo " => failed" && exit 1) 
	@echo " => passed"

run-opt: common
	@printf " ... testing 'native':"
	@$(OCAMLOPT) -c tcallback.ml
	@$(OCAMLOPT) -o ./program unix.cmxa callbackprim.$(O) tcallback.cmx
	@./program > native.result
	@$(DIFF) reference native.result || (echo " => failed" && exit 1) 
	@echo " => passed"
	
promote: defaultpromote

clean: defaultclean
	@rm -f *.result ./program

include $(BASEDIR)/makefiles/Makefile.common
