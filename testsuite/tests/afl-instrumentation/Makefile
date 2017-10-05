BASEDIR=../..

default:
	@printf " ... testing 'afl_instrumentation':"
	@if ! which afl-showmap > /dev/null; then \
	  echo " => skipped (afl-showmap unavailable)"; \
	else \
	  if OCAMLOPT='$(OCAMLOPT)' ./test.sh > /dev/null; then \
	    echo " => passed"; \
	  else \
	    echo " => failed"; \
	  fi \
	fi

.PHONY: clean
clean:

include $(BASEDIR)/makefiles/Makefile.common
