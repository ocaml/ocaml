# $Id$

include ../config/Makefile

DIRS=utils parsing otherlibs

all:
	if test "$(OTOP)" = "../ocaml_stuff"; then \
	  set -e; \
	  for i in $(DIRS); do cd $$i; $(MAKE) all EXE=$(EXE); cd ..; done; \
	fi

clean:
	if test "$(OTOP)" = "../ocaml_stuff"; then \
	  set -e; \
	  for i in $(DIRS); do cd $$i; $(MAKE) clean EXE=$(EXE); cd ..; done; \
	fi

depend:
	set -e; for i in $(DIRS); do cd $$i; $(MAKE) depend; cd ..; done

steal:
	@if test "$(OCAML_SRC)" = ""; then \
		echo "usage: make steal OCAML_SRC=..."; exit 1; \
	fi
	$(MAKE) copy_steal FILE=utils/config.mli
	sed -e 's/\$$Id.*\$$/Id/' -e 's|"/.*"|""|' \
	$(OCAML_SRC)/utils/config.ml > utils/config.ml
	$(MAKE) copy_steal FILE=utils/warnings.mli
	$(MAKE) copy_steal FILE=parsing/asttypes.mli
	$(MAKE) copy_steal FILE=parsing/location.mli
	$(MAKE) copy_steal FILE=parsing/longident.mli
	$(MAKE) copy_steal FILE=parsing/parsetree.mli
	$(MAKE) copy_steal FILE=otherlibs/dynlink/extract_crc.ml

copy_steal:
	sed 's/\$$Id.*\$$/Id/' $(OCAML_SRC)/$(FILE) > $(FILE)

compare_stolen:
	@if test "$(OCAML_SRC)" = ""; then \
		echo "usage: make compare_stolen OCAML_SRC=..."; exit 1; \
	fi
	@echo =================================================
	@echo warnings.ml
	@sed 's/\$$Id.*\$$/Id/' $(OCAML_SRC)/utils/warnings.mli | diff utils/warnings.mli -; exit 0
	@echo =================================================
	@echo config.mli
	@sed 's/\$$Id.*\$$/Id/' $(OCAML_SRC)/utils/config.mli | diff utils/config.mli -; exit 0
	@echo =================================================
	@echo config.ml
	@sed 's|"/.*"|""|' $(OCAML_SRC)/utils/config.ml | sed 's/\$$Id.*\$$/Id/' | diff utils/config.ml -; exit 0
	@echo =================================================
	@echo asttypes.mli
	@sed 's/\$$Id.*\$$/Id/' $(OCAML_SRC)/parsing/asttypes.mli | diff parsing/asttypes.mli -; exit 0
	@echo =================================================
	@echo location.mli
	@sed 's/\$$Id.*\$$/Id/' $(OCAML_SRC)/parsing/location.mli | diff parsing/location.mli -; exit 0
	@echo =================================================
	@echo longident.mli
	@sed 's/\$$Id.*\$$/Id/' $(OCAML_SRC)/parsing/longident.mli | diff parsing/longident.mli -; exit 0
	@echo =================================================
	@echo parsetree.mli
	@sed 's/\$$Id.*\$$/Id/' $(OCAML_SRC)/parsing/parsetree.mli | diff parsing/parsetree.mli -; exit 0
	@echo =================================================
	@echo extract_crc.ml
	@sed 's/\$$Id.*\$$/Id/' $(OCAML_SRC)/otherlibs/dynlink/extract_crc.ml | diff otherlibs/dynlink/extract_crc.ml -; exit 0
