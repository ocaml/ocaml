compile: compile-targets

promote: defaultpromote

clean: defaultclean

include ../makefiles/Makefile.common

compile-targets: testing.cmi testing.cmo
	@if [ -z "$(BYTECODE_ONLY)" ]; then \
	  $(MAKE) testing.cmx; \
	fi
