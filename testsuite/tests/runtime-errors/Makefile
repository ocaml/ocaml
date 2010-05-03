default: compile run

compile:
	@for f in *.ml; do \
	  $(OCAMLC) -w a -o `basename $$f ml`bytecode $$f; \
	  $(OCAMLOPT) -w a -o `basename $$f ml`native $$f; \
	done
	@if [ ! `grep -c HAS_STACK_OVERFLOW_DETECTION ../../../config/s.h` ]; then \
	  rm -f stackoverflow.byte stackoverflow.native; \
	fi

run:
	@for f in *.bytecode; do \
	  printf " ... testing '$$f':"; \
	  (./$$f > $$f.result 2>&1; true); \
	  diff -q $$f.reference $$f.result > /dev/null || (echo " => failed" && exit 1) && echo " => passed"; \
	  printf " ... testing '`basename $$f bytecode`native':"; \
	  (./`basename $$f bytecode`native > `basename $$f bytecode`native.result 2>&1; true); \
	  diff -q `basename $$f bytecode`native.reference `basename $$f bytecode`native.result > /dev/null || (echo " => failed" && exit 1) && echo " => passed"; \
	done

clean: defaultclean
	@rm -f *.bytecode *.native *.result

include ../../makefiles/Makefile.common
