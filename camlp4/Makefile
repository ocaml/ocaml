# $Id$

include config/Makefile

DIRS=odyl camlp4 meta etc top ocpp lib man
FDIRS=odyl camlp4 meta lib
OPTDIRS=lib odyl camlp4 meta etc
SHELL=/bin/sh

all: boot/camlp4$(EXE)
	set -e; for i in $(DIRS); do cd $$i; $(MAKE) all; cd ..; done

opt:
	cd lib; $(MAKE) opt

bullshit:
	set -e; for i in $(OPTDIRS); do cd $$i; $(MAKE) opt; cd ..; done

boot/camlp4$(EXE):
	$(MAKE) clean_cold library_cold compile_cold
	$(MAKE) promote_cold
	$(MAKE) clean_cold clean_hot library

clean_hot:
	for i in $(DIRS); do (cd $$i; $(MAKE) clean); done

depend:
	for i in $(DIRS); do (cd $$i; $(MAKE) depend); done

install:
	for i in $(DIRS); do (cd $$i; $(MAKE) install BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) MANDIR=$(MANDIR)); done

clean::
	$(MAKE) clean_hot clean_cold
	rm -f boot/*.cm[oi] boot/camlp4*
	rm -rf boot/SAVED

scratch: clean

always:

# Normal bootstrap

bootstrap: backup promote clean_hot all compare

backup:
	mkdir boot.new
	make mv_cvs FROM=boot TO=boot.new
	mv boot boot.new/SAVED
	mv boot.new boot

restore:
	mv boot/SAVED boot.new
	make mv_cvs FROM=boot TO=boot.new
	rm -rf boot
	mv boot.new boot

promote:
	for i in $(FDIRS); do (cd $$i; $(MAKE) promote); done

compare:
	@if (for i in $(FDIRS); do \
		if (cd $$i; $(MAKE) compare 2>/dev/null); then :; \
		else exit 1; fi; \
	     done); \
	then echo "Fixpoint reached, bootstrap succeeded."; \
	else echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	fi

cleanboot:
	rm -rf boot/SAVED/SAVED


# Fast bootstrap

bootstrap_fast: backup promote clean_hot fast compare

fast: boot/camlp4$(EXE)
	set -e; for i in $(FDIRS); do cd $$i; $(MAKE) all; cd ..; done

clean_fast:
	for i in $(FDIRS); do (cd $$i; $(MAKE) clean); done


# The very beginning

world:
	$(MAKE) clean_cold library_cold compile_cold
	$(MAKE) promote_cold
	$(MAKE) clean_cold clean_hot library all

library:
	cd lib; $(MAKE) all promote

# Cold start using pure Objective Caml sources

library_cold:
	cd ocaml_src/lib; $(MAKE) all promote OTOP=../$(OTOP)

compile_cold:
	cd ocaml_src; set -e; \
	for i in $(FDIRS); do \
	  cd $$i; $(MAKE) all OTOP=../$(OTOP); cd ..; \
	done

promote_cold:
	for i in $(FDIRS); do \
		(cd ocaml_src/$$i; $(MAKE) promote); \
	done

clean_cold:
	for i in $(FDIRS); do \
		(cd ocaml_src/$$i; $(MAKE) clean); \
	done

# Bootstrap the sources

bootstrap_sources:
	cd etc; make pr_o.cmo
	mkdir ocaml_src.new
	@-for i in $(FDIRS); do \
		(mkdir ocaml_src.new/$$i; cd ocaml_src.new/$$i; \
		 sed 's/# $$Id.*\$$/# Id/' ../../$$i/Makefile | \
		 sed 's-include ../config-include ../../config-g' | \
		 sed 's-../boot-../../boot-g' > Makefile; \
		 cp ../../$$i/.depend .; \
	 done
	@-for i in $(FDIRS); do \
		for j in $$i/*.ml*; do \
			echo ============================================; \
			echo ocaml_src.new/$$j; \
			OTOP=.. ./tools/conv.sh $$j | \
			sed 's/$$Id.*\$$/Id/' > ocaml_src.new/$$j; \
		done; \
	done

promote_sources:
	make mv_cvs FROM=ocaml_src TO=ocaml_src.new
	for i in $(FDIRS); do \
		make mv_cvs FROM=ocaml_src/$$i TO=ocaml_src.new/$$i; \
	done
	mv ocaml_src/tools ocaml_src.new/.
	mv ocaml_src ocaml_src.new/SAVED
	mv ocaml_src.new ocaml_src

unpromote_sources:
	mv ocaml_src ocaml_src.new
	mv ocaml_src.new/SAVED ocaml_src
	mv ocaml_src.new/tools ocaml_src/.
	for i in $(FDIRS); do \
		make mv_cvs FROM=ocaml_src.new/$$i TO=ocaml_src/$$i; \
	done
	make mv_cvs FROM=ocaml_src.new TO=ocaml_src

clean_sources:
	rm -rf ocaml_src/SAVED/SAVED

# Utility

mv_cvs:
	test ! -d $(FROM)/CVS || mv $(FROM)/CVS $(TO)/.
	test ! -f $(FROM)/.cvsignore || mv $(FROM)/.cvsignore $(TO)/.
