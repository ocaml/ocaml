# $Id$

OCAMLRUN=CAML_LD_LIBRARY_PATH=../otherlibs/unix ../boot/ocamlrun
OCAML=$(OCAMLRUN) ../ocaml -I ../stdlib -I ../otherlibs/unix
YAM=$(OCAMLRUN) ./yam
YAM_OPTIONS=-verbosity '$(VERBOSE)'

default: all

opt install doc all pack just_doc: yam
	@echo 'YAM $@ (use "make $@ VERBOSE=1" for a verbose make)'
	@$(YAM) $(YAM_OPTIONS) $@

clean:
	if test -x ../ocaml; then \
	$(OCAML) build/build.ml -clean; \
	else \
	if test -x $(YAM); then \
	$(YAM) $(YAM_OPTIONS) -clean; \
	else : ; \
	fi; \
	fi

cleanall:
	$(OCAML) build/build.ml -cleanall

yam: build/build.ml
	$(OCAML) build/build.ml

uninstall:
	rm -rf "$(LIBDIR)/camlp4"
	cd "$(BINDIR)"; rm -f *camlp4*

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
	promote compare opt doc cleanall all-local clean-local

