default:
	@if $(BYTECODE_ONLY); then : ; elif [ $(SYSTEM) = macosx ]; then $(MAKE) native_macosx_tests; fi

native_macosx_tests:
	@printf " ... testing 'unwind_test':"; \
        $(MAKE) clean ; $(MAKE) unwind_test && \
	./unwind_test >/dev/null 2>&1 && echo " => passed" || echo " => failed"

unwind_test:
	@ocamlopt -c -opaque mylib.mli
	@ocamlopt -c driver.ml
	@ocamlopt -c mylib.ml
	@ocamlopt -c stack_walker.c
	@ocamlopt -cclib -Wl,-keep_dwarf_unwind -o unwind_test mylib.cmx driver.cmx stack_walker.o

clean:
	@rm -f *.cm* *.o unwind_test

include $(BASEDIR)/makefiles/Makefile.common
