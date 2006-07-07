# $Id$

OCAMLRUN=CAML_LD_LIBRARY_PATH=../otherlibs/unix ../boot/ocamlrun
OCAML=$(OCAMLRUN) ../ocaml -I ../stdlib -I ../otherlibs/unix
YAM=$(OCAMLRUN) ./yam

default all-local: yam
	$(YAM) -v all

opt: yam
	$(YAM) -v opt

clean-local:
	if test -x ../ocaml; then \
	$(OCAML) build/build.ml -clean; \
	else \
	if test -x $(YAM); then \
	$(YAM) -v -clean; \
	else : ; \
	fi; \
	fi

cleanall:
	$(OCAML) build/build.ml -cleanall

install: yam
	$(YAM) install

doc: yam
	$(YAM) -v doc

yam: build/build.ml
	$(OCAML) build/build.ml

DIRS=boot man
#DIRS=boot odyl lib camlp4 meta etc top ocpp man compile

all clean:
	set -e; for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@) || eval 'exit 1'; done
	$(MAKE) $@-local
	set -e; for i in $(SUBDIRS_LAST); do (cd $$i && $(MAKE) $@) || eval 'exit 1'; done

uninstall:
	rm -rf "$(LIBDIR)/camlp4"
	cd "$(BINDIR)"; rm -f *camlp4* odyl ocpp

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

