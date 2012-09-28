# $Id: Makefile 12858 2012-08-10 14:45:51Z maranget $

BASEDIR=${PWD}
NO_PRINT=`($(MAKE) empty --no-print-directory > /dev/null 2>&1) && echo '--no-print-directory' || echo ''`

default:
	@echo "Available targets:"
	@echo "  all             launches all tests"
	@echo "  list FILE=f     launches the tests referenced in file f (one path per line)"
	@echo "  one DIR=p       launches the tests located in path p"
	@echo "  promote DIR=p   promotes the reference files for the tests located in path p"
	@echo "  lib             builds library modules"
	@echo "  clean           deletes generated files"
	@echo "  report          prints the report for the last execution, if any"

all: lib
	@for dir in tests/*; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$dir; \
	done 2>&1 | tee _log
	@$(MAKE) report

list: lib
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
	@if [ ! -f $(DIR)/Makefile ]; then \
	  for dir in $(DIR)/*; do \
	    if [ -d $$dir ]; then \
	      $(MAKE) exec-one DIR=$$dir; \
	    fi; \
	  done; \
        else \
	  echo "Running tests from '$$DIR' ..."; \
	  (cd $(DIR) && $(MAKE) TERM=dumb BASEDIR=$(BASEDIR)); \
	fi

promote: FORCE
	@if [ -z $(DIR) ]; then echo "No value set for variable 'DIR'."; exit 1; fi
	@if [ ! -d $(DIR) ]; then echo "Directory '$(DIR)' does not exist."; exit 1; fi
	@(cd $(DIR) && $(MAKE) TERM=dumb BASEDIR=$(BASEDIR) promote)

lib: FORCE
	@(cd lib && $(MAKE) -s BASEDIR=$(BASEDIR))

clean: FORCE
	@(cd lib && $(MAKE) BASEDIR=$(BASEDIR) clean)
	@for file in `find interactive tests -name Makefile`; do \
	  (cd `dirname $$file` && $(MAKE) BASEDIR=$(BASEDIR) clean); \
	done

report: FORCE
	@if [ ! -f _log ]; then echo "No '_log' file."; exit 1; fi
	@echo ''
	@echo 'Summary:'
	@echo '  ' `grep 'passed$$' _log | wc -l` 'test(s) passed'
	@echo '  ' `grep 'failed$$' _log | wc -l` 'test(s) failed'
	@echo '  ' `grep '^Error' _log | wc -l` 'compilation error(s)'
	@echo '  ' `grep '^Warning' _log | wc -l` 'compilation warning(s)'
	@echo '  ' `grep '^make\[2\]: ' _log | wc -l` 'makefile error(s)'

empty: FORCE

FORCE:
