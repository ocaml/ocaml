BASEDIR=../..



# The -keep_dwarf_unwind option of ld was introduced in ld version 224.1.
# (The last released version where it is not supported is version 136.)
default:
	@printf " ... testing 'unwind_test':"
	@if $(BYTECODE_ONLY); then \
	  echo " => skipped (bytecode only)"; \
	elif [ $(SYSTEM) = macosx ]; then \
	  LDFULL="`ld -v 2>&1`"; \
	  LD="`echo $$LDFULL | grep -o \"ld64-[0-9]*\"`"; \
	  LDVER="`echo $$LD | sed \"s/ld64-//\"`"; \
	  if [[ -z "$$LD" ]]; then \
	    echo " => skipped (unknown linker: pattern ld64-[0-9]* not found" \
	    echo "    in 'ld -v' output)"; \
	  elif [[ $$LDVER -lt 224 ]]; then \
	   echo " => skipped (ld version is $$LDVER, only 224 or above " \
	   echo "    are supported)"; \
	  else \
	    $(MAKE) run_tests; \
	  fi; \
	else \
	  if ! pkg-config --exists libunwind 2>/dev/null; then \
	    echo " => skipped (libunwind package not found)";  \
          else \
	    $(MAKE) run_tests ; \
	  fi ; \
	fi


include $(BASEDIR)/makefiles/Makefile.common

ifeq ($(SYSTEM),macosx)
LIBUNWIND_OPTS = -cclib -Wl,-keep_dwarf_unwind
else
LIBUNWIND_OPTS = $(patsubst %, -cclib %, $(shell pkg-config --libs libunwind))
endif

run_tests: unwind_test
	@./unwind_test >/dev/null 2>&1 && echo " => passed" || echo " => failed"

unwind_test: mylib.mli driver.ml mylib.ml stack_walker.c
	@$(OCAMLOPT) -g -c -opaque mylib.mli
	@$(OCAMLOPT) -g -c driver.ml
	@$(OCAMLOPT) -g -c mylib.ml
	@$(OCAMLOPT) -g -ccopt -I -ccopt $(CTOPDIR)/byterun -c stack_walker.c
	@$(OCAMLOPT) -g -o unwind_test mylib.cmx \
	             driver.cmx stack_walker.o $(LIBUNWIND_OPTS)

clean:
	@rm -f *.cm* *.o unwind_test

