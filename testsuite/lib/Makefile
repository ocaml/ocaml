# $Id$

compile: compile-targets

promote: defaultpromote

clean: defaultclean

include ../makefiles/Makefile.common

compile-targets: testing.cmi testing.cmo $(if $(BYTECODE_ONLY),,testing.cmx)
