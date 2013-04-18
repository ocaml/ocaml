BASEDIR=../..

default: compile run

compile:
	@for f in *.ml; do \
	  $(OCAMLC) -w a -o `basename $$f ml`bytecode $$f; \
	  test -z "$(BYTECODE_ONLY)" && $(OCAMLOPT) -w a -o `basename $$f ml`native $$f || true; \
	done
	@if [ `grep -c HAS_STACK_OVERFLOW_DETECTION ../../../config/s.h` -eq 0 ]; then \
	  rm -f stackoverflow.native || true; \
	fi

run:
	@ulimit -s 1024; \
	 for f in *.bytecode; do \
	  printf " ... testing '$$f':"; \
	  (./$$f > $$f.result 2>&1 || true); \
	  $(DIFF) $$f.reference $$f.result > /dev/null || (echo " => failed" && exit 1) && echo " => passed"; \
	  fn=`basename $$f bytecode`native; \
	  if [ -z "$(BYTECODE_ONLY)" -a -f "$$fn" ]; then \
	    printf " ... testing '$$fn':"; \
	    (./$$fn > $$fn.result 2>&1 || true); \
	    $(DIFF) $$fn.reference $$fn.result > /dev/null || (echo " => failed" && exit 1) && echo " => passed"; \
	  fi; \
	done

promote: defaultpromote

clean: defaultclean
	@rm -f *.bytecode *.native *.result

include $(BASEDIR)/makefiles/Makefile.common
