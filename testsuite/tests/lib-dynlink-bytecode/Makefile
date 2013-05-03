#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#                 Xavier Clerc, SED, INRIA Rocquencourt                 #
#                                                                       #
#   Copyright 2010 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

BASEDIR=../..

default: compile run

compile:
	@$(OCAMLC) -c registry.ml
	@for file in stub*.c; do \
	  $(OCAMLC) -c $$file; \
	  $(OCAMLMKLIB) -o `echo $$file | sed -e 's/stub/plug/' -e 's/\.c//'` \
	                `basename $$file c`o; \
	done
	@for file in plug*.ml; do \
	  $(OCAMLC) -c $$file; \
	  $(OCAMLMKLIB) -o `basename $$file .ml` `basename $$file ml`cmo; \
	done
	@$(OCAMLC) -c main.ml
	@$(OCAMLC) -o main dynlink.cma registry.cmo main.cmo
	@$(OCAMLC) -o static -linkall registry.cmo plug1.cma plug2.cma \
	           -use-runtime $(PREFIX)/bin/ocamlrun
	@$(OCAMLC) -o custom -custom -linkall registry.cmo plug2.cma plug1.cma \
	           -I .

run:
	@printf " ... testing 'main'"
	@LD_LIBRARY_PATH="`pwd`" ./main plug1.cma plug2.cma >main.result
	@$(DIFF) main.reference main.result >/dev/null \
	&& echo " => passed" || echo " => failed"

	@printf " ... testing 'static'"
	@LD_LIBRARY_PATH="`pwd`" ./static >static.result
	@$(DIFF) static.reference static.result >/dev/null \
	&& echo " => passed" || echo " => failed"

	@printf " ... testing 'custom'"
	@LD_LIBRARY_PATH="`pwd`" ./custom > custom.result
	@$(DIFF) custom.reference custom.result >/dev/null \
	&& echo " => passed" || echo " => failed"

promote: defaultpromote

clean: defaultclean
	@rm -f ./main ./static ./custom *.result marshal.data

include $(BASEDIR)/makefiles/Makefile.common
