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

BASEDIR := $(shell pwd)
NO_PRINT=`$(MAKE) empty --no-print-directory >/dev/null 2>&1 \
	  && echo --no-print-directory`

FIND=find
include ../config/Makefile

.PHONY: default
default:
	@echo "Available targets:"
	@echo "  all             launch all tests"
	@echo "  all-foo         launch all tests beginning with foo"
	@echo "  list FILE=f     launch the tests listed in f (one per line)"
	@echo "  one DIR=p       launch the tests located in path p"
	@echo "  promote DIR=p   promote the reference files for the tests in p"
	@echo "  lib             build library modules"
	@echo "  clean           delete generated files"
	@echo "  report          print the report for the last execution"
	@echo
	@echo "all* and list can automatically re-run failed test directories if"
	@echo "MAX_TESTSUITE_DIR_RETRIES permits (default value = $(MAX_TESTSUITE_DIR_RETRIES))"

.PHONY: all
all: lib
	@for dir in tests/*; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$dir; \
	done 2>&1 | tee _log
	@$(MAKE) $(NO_PRINT) retries
	@$(MAKE) report

.PHONY: all-%
all-%: lib
	@for dir in tests/$**; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$dir; \
	done 2>&1 | tee _log
	@$(MAKE) $(NO_PRINT) retries
	@$(MAKE) report

.PHONY: list
list: lib
	@if [ -z "$(FILE)" ]; \
	  then echo "No value set for variable 'FILE'."; \
	  exit 1; \
	fi
	@while read LINE; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$LINE; \
	done <$(FILE) 2>&1 | tee _log
	@$(MAKE) $(NO_PRINT) retries
	@$(MAKE) report

.PHONY: one
one: lib
	@if [ -z "$(DIR)" ]; then \
	  echo "No value set for variable 'DIR'."; \
	  exit 1; \
	fi
	@if [ ! -d $(DIR) ]; then \
	  echo "Directory '$(DIR)' does not exist."; \
	  exit 1; \
	fi
	@$(MAKE) $(NO_PRINT) exec-one DIR=$(DIR)

.PHONY: exec-one
exec-one:
	@if [ ! -f $(DIR)/Makefile ]; then \
	  for dir in $(DIR)/*; do \
	    if [ -d $$dir ]; then \
	      $(MAKE) exec-one DIR=$$dir; \
	    fi; \
	  done; \
	else \
	  echo "Running tests from '$$DIR' ..."; \
	  cd $(DIR) && \
	  $(MAKE) TERM=dumb BASEDIR=$(BASEDIR) || echo '=> unexpected error'; \
	fi

.PHONY: clean-one
clean-one:
	@if [ ! -f $(DIR)/Makefile ]; then \
	  for dir in $(DIR)/*; do \
	    if [ -d $$dir ]; then \
	      $(MAKE) clean-one DIR=$$dir; \
	    fi; \
	  done; \
	else \
	  cd $(DIR) && $(MAKE) TERM=dumb BASEDIR=$(BASEDIR) clean; \
	fi

.PHONY: promote
promote:
	@if [ -z "$(DIR)" ]; then \
	  echo "No value set for variable 'DIR'."; \
	  exit 1; \
	fi
	@if [ ! -d $(DIR) ]; then \
	  echo "Directory '$(DIR)' does not exist."; \
	  exit 1; \
	fi
	@cd $(DIR) && $(MAKE) TERM=dumb BASEDIR=$(BASEDIR) promote

.PHONY: lib
lib:
	@cd lib && $(MAKE) -s BASEDIR=$(BASEDIR)

.PHONY: clean
clean:
	@cd lib && $(MAKE) BASEDIR=$(BASEDIR) clean
	@for file in `$(FIND) interactive tests -name Makefile`; do \
	  (cd `dirname $$file` && $(MAKE) BASEDIR=$(BASEDIR) clean); \
	done

.PHONY: report
report:
	@if [ ! -f _log ]; then echo "No '_log' file."; exit 1; fi
	@awk -f makefiles/summarize.awk <_log

retry-list:
	@while read LINE; do \
	  if [ -n "$$LINE" ] ; then \
	    echo re-ran $$LINE>>_log; \
	    $(MAKE) $(NO_PRINT) clean-one DIR=$$LINE; \
	    $(MAKE) $(NO_PRINT) exec-one DIR=$$LINE 2>&1 | tee -a _log ; \
	  fi \
	done <_retries;
	@$(MAKE) $(NO_PRINT) retries

retries:
	@awk -v retries=1 -v max_retries=$(MAX_TESTSUITE_DIR_RETRIES) -f makefiles/summarize.awk <_log >_retries
	@test `cat _retries | wc -l` -eq 0 || $(MAKE) $(NO_PRINT) retry-list
	@rm -f _retries

.PHONY: empty
empty:
