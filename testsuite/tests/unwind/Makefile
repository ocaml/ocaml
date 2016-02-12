default:
	@printf " ... testing 'unwind_test':"
	@if [ $(SYSTEM) = macosx ] && ! $(BYTECODE_ONLY); then \
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
	@$(OCAMLOPT) -cclib -Wl,-keep_dwarf_unwind -o unwind_test mylib.cmx driver.cmx stack_walker.o

clean:
	@rm -f *.cm* *.o unwind_test

include $(BASEDIR)/makefiles/Makefile.common
