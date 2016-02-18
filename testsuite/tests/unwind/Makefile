# The -keep_dwarf_unwind option of ld was introduced in ld version 224.1.
# (The last released version where it is not supported is version 136.)
default:
	@printf " ... testing 'unwind_test':"
	@if [ $(SYSTEM) = macosx ] && ! $(BYTECODE_ONLY) && [[ `ld -v 2>&1` =~ ld64-([0-9]*) ]] && [[ 224 -le $${BASH_REMATCH[1]} ]]; then \
	  $(MAKE) native_macosx_tests; \
	else \
	  echo " => skipped"; \
	fi

native_macosx_tests:
	@$(MAKE) clean ; $(MAKE) unwind_test && \
	./unwind_test >/dev/null 2>&1 && echo " => passed" || echo " => failed"

unwind_test:
	@$(OCAMLOPT) -c -opaque mylib.mli
	@$(OCAMLOPT) -c driver.ml
	@$(OCAMLOPT) -c mylib.ml
	@$(OCAMLOPT) -c stack_walker.c
	@$(OCAMLOPT) -cclib -Wl,-keep_dwarf_unwind -o unwind_test mylib.cmx \
	             driver.cmx stack_walker.o

clean:
	@rm -f *.cm* *.o unwind_test

include $(BASEDIR)/makefiles/Makefile.common
