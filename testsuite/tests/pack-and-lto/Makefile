default: native warning

native:
	printf " ... testing native 'test.reference':"
	@$(OCAMLOPT) -lto -c -for-pack Pack package/a.ml
	@$(OCAMLOPT) -lto -c -for-pack Pack package/b.ml
	@$(OCAMLOPT) -lto -pack package/a.cmx package/b.cmx -o pack.cmx
	@$(OCAMLOPT) -lto -c test.ml
	@$(OCAMLOPT) -lto pack.cmx test.cmx -o test.native
	@./test.native > test.result
	@$(DIFF) test.result test.reference >/dev/null \
	    && echo " => passed" || echo " => failed"

warning:
	printf " ... testing warning with lto 'warning.reference':"
	@$(OCAMLOPT) -c no_lto.ml # no lto
	@$(OCAMLOPT) -lto -c main_warning.ml 2> warning.result
	@$(DIFF) warning.result warning.reference >/dev/null \
	    && echo " => passed" || echo " => failed"


promote: defaultpromote

clean: defaultclean
	@rm -f *.result
	@rm -f test.native

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
COMPFLAGS =
