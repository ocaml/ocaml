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

# $Id$

OCAMLRUN=CAML_LD_LIBRARY_PATH=../otherlibs/unix ../boot/ocamlrun
OCAML=$(OCAMLRUN) ../ocaml -I ../stdlib -I ../otherlibs/unix
YAM=$(OCAMLRUN) ./yam
YAM_OPTIONS=-verbosity '$(VERBOSE)'

default: all

opt install doc all pack just_doc: yam
	@echo 'YAM $@ (use "make $@ VERBOSE=1" for a verbose make)'
	@$(YAM) $(YAM_OPTIONS) $@

genclean: yam
	$(YAM) -genclean Makefile.clean

clean:
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
	  $(MAKE) staticclean; \
	fi

cleanall:
	$(OCAML) build/build.ml -cleanall

staticclean:
	rm -f $(CLEANFILES)

yam: build/build.ml
	$(OCAML) build/build.ml

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
	promote compare opt doc cleanall all-local clean-local depend

-include Makefile.clean
