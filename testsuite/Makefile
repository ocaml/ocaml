# $Id$

BASEDIR=${PWD}
NO_PRINT=`($(MAKE) empty --no-print-directory > /dev/null 2>&1) && echo '--no-print-directory' || echo ''`

default:
	@echo "Available targets:"
	@echo "  all           launches all tests"
	@echo "  list FILE=f   launches the tests referenced in file f (one path per line)"
	@echo "  one DIR=p     launches the tests located in path p"
	@echo "  lib           builds library modules"
	@echo "  clean         deletes generated files"
	@echo "  report        prints the report for the last execution, if any"

all:
	@for dir in tests/*; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$dir; \
	done 2>&1 | tee _log
	@$(MAKE) report

list:
	@if [ -z $(FILE) ]; then echo "No value set for variable 'FILE'."; exit 1; fi
	@if [ ! -f $(FILE) ]; then echo "File '$(FILE)' does not exist."; exit 1; fi
	@while read LINE; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$LINE; \
	done < $(FILE) 2>&1 | tee _log
	@$(MAKE) report

one: lib
	@if [ -z $(DIR) ]; then echo "No value set for variable 'DIR'."; exit 1; fi
	@if [ ! -d $(DIR) ]; then echo "Directory '$(DIR)' does not exist."; exit 1; fi
	@$(MAKE) $(NO_PRINT) exec-one DIR=$(DIR)

exec-one:
	@echo "Running tests from '$$DIR' ..."
	@(cd $(DIR) && $(MAKE) BASEDIR=$(BASEDIR) && cd ../..)

lib: FORCE
	@(cd lib && $(MAKE) -s BASEDIR=$(BASEDIR) && cd ..)

clean: FORCE
	@(cd lib && $(MAKE) BASEDIR=$(BASEDIR) clean && cd ..)
	@for file in tests/*; do \
	  if [ -d $$file ]; then \
	    (cd $$file && $(MAKE) BASEDIR=$(BASEDIR) clean && cd ../..); \
	  fi \
	done
	@for file in interactive/*; do \
	  if [ -d $$file ]; then \
	    (cd $$file && $(MAKE) BASEDIR=$(BASEDIR) clean && cd ../..); \
	  fi \
	done

report: FORCE
	@if [ ! -f _log ]; then echo "No '_log' file."; exit 1; fi
	@echo ''
	@echo 'Summary:'
	@echo '  ' `grep 'passed$$' _log | wc -l` 'test(s) passed'
	@echo '  ' `grep 'failed$$' _log | wc -l` 'test(s) failed'
	@echo '  ' `grep '^Error' _log | wc -l` 'compilation error(s)'
	@echo '  ' `grep '^Warning' _log | wc -l` 'compilation warning(s)'

empty: FORCE

FORCE:
