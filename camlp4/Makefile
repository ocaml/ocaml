#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#        Nicolas Pouillard, projet Gallium, INRIA Rocquencourt          #
#                                                                       #
#   Copyright 2006 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################



# RELEASE NOTE:
#   Do not forget to call make genclean to update Makefile.clean before a
#   release.

OCAMLC=../boot/ocamlrun ../ocamlc -nostdlib \
  -I ../stdlib -I ../otherlibs/unix -I ../otherlibs/win32unix -I build -g
OCAMLRUN=../boot/ocamlrun -I ../otherlibs/unix -I ../otherlibs/win32unix
YAM=$(OCAMLRUN) ./yam
YAM_OPTIONS=-verbosity '$(VERBOSE)'

default: all

opt install doc all pack just_doc: yam
	@echo 'YAM $@ (use "make $@ VERBOSE=1" for a verbose make)'
	@$(YAM) $(YAM_OPTIONS) $@

smartclean::
	if test -x ../boot/ocamlrun; then \
	  if test -x ../ocaml; then \
	    $(OCAML) build/build.ml -clean; \
	  else \
	    if test -x ./yam; then \
	      $(YAM) $(YAM_OPTIONS) -clean; \
	    else \
	      $(MAKE) staticclean; \
	    fi; \
	  fi; \
	else \
	  rm -f $(CLEANFILES); \
	fi

genclean: yam
	$(YAM) -genclean Makefile.clean
	(echo /Camlp4Ast.ml/d; echo w; echo q) | ed Makefile.clean

clean::
	rm -f yam .cache-status
	rm -f $(CLEANFILES)
	rm -f *.cm[io] build/*.cm[io]

YAM_OBJS=build/YaM.cmo build/camlp4_config.cmo Makefile.cmo

yam: $(YAM_OBJS)
	$(OCAMLC) -o yam unix.cma $(YAM_OBJS)

.SUFFIXES: .mli .ml .cmi .cmo

.mli.cmi:
	$(OCAMLC) -c $*.mli
.ml.cmo:
	$(OCAMLC) -c $*.ml

build/YaM.cmo: build/YaM.cmi
Makefile.cmo: build/YaM.cmi build/camlp4_config.cmo

uninstall:
	rm -rf "$(LIBDIR)/camlp4"
	cd "$(BINDIR)"; rm -f *camlp4*

depend:

# Normal bootstrap

bootstrap: backup promote clean all compare
bootstrap-debug: backup promote-debug clean all compare

backup restore boot-clean::
	cd boot && $(MAKE) $@

promote-debug:
	cp camlp4boot-debug.run boot/camlp4boot

promote:
	cp camlp4boot.run boot/camlp4boot

compare:
	@if (cmp camlp4boot.run boot/camlp4boot); \
	then echo "Fixpoint reached, bootstrap succeeded."; \
	else echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	fi

.PHONY: clean install all uninstall backup restore boot-clean promote-debug \
	promote compare opt doc smartclean depend

include Makefile.clean
