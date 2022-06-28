#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

.NOTPARALLEL:

BASEDIR := $(shell pwd)
ifneq "$(words |$(BASEDIR)|)" "1"
$(error The Testsuite does not handle spaces\
   in the ocaml working directory path: $(BASEDIR))
endif

ROOTDIR = ..
include $(ROOTDIR)/Makefile.common

ifeq "$(UNIX_OR_WIN32)" "win32"
  CYGPATH=cygpath -m
  # Ensure that the test runners definitely use Cygwin's sort and not the
  # Windows sort command
  SORT=/usr/bin/sort
else
  CYGPATH=echo
  SORT=sort
endif

BASEDIR_HOST := $(shell $(CYGPATH) "$(BASEDIR)")
ROOTDIR_HOST := $(BASEDIR_HOST)/$(ROOTDIR)

OCAMLTESTDIR = $(BASEDIR_HOST)/$(DIR)/_ocamltest

failstamp := failure.stamp

TESTLOG ?= _log

ocamltest_directory := ../ocamltest

ocamltest_program := $(or \
  $(wildcard $(ocamltest_directory)/ocamltest.opt$(EXE)),\
  $(wildcard $(ocamltest_directory)/ocamltest$(EXE)))

ifeq "$(UNIX_OR_WIN32)" "unix"
  ifeq "$(SYSTEM)" "cygwin"
    find := /usr/bin/find
  else # Non-cygwin Unix
    find := find
  endif
else # Windows
  find := /usr/bin/find
endif

ifeq "$(BOOTSTRAPPING_FLEXDLL)" "false"
  FLEXLINK_ENV =
else
  # The testsuite needs an absolute path to the runtime, so override the
  # definition in Makefile.common
  FLEXLINK_DLL_LDFLAGS=$(if $(OC_DLL_LDFLAGS), -link "$(OC_DLL_LDFLAGS)")
  FLEXLINK_EXE_LDFLAGS=$(if $(OC_LDFLAGS), -link "$(OC_LDFLAGS)")
ifeq "$(wildcard $(ROOTDIR_HOST)/flexlink.opt$(EXE))" ""
  FLEXLINK_ENV = \
    OCAML_FLEXLINK="$(ROOTDIR_HOST)/boot/ocamlrun$(EXE) \
                    $(ROOTDIR_HOST)/boot/flexlink.byte$(EXE)"
  MKDLL=$(ROOTDIR_HOST)/boot/ocamlrun$(EXE) \
        $(ROOTDIR_HOST)/boot/flexlink.byte$(EXE) \
        -chain $(FLEXDLL_CHAIN) $(FLEXLINK_FLAGS) $(FLEXLINK_DLL_LDFLAGS)
  MKEXE=$(ROOTDIR_HOST)/boot/ocamlrun$(EXE) \
        $(ROOTDIR_HOST)/boot/flexlink.byte$(EXE) \
        -exe -chain $(FLEXDLL_CHAIN) $(FLEXLINK_FLAGS) $(FLEXLINK_EXE_LDFLAGS)
else
  FLEXLINK_ENV = \
    OCAML_FLEXLINK="$(ROOTDIR_HOST)/flexlink.opt$(EXE) \
      -I $(ROOTDIR_HOST)/stdlib/flexdll"
  MKDLL=$(ROOTDIR_HOST)/flexlink.opt$(EXE) -I $(ROOTDIR_HOST)/stdlib/flexdll \
        -chain $(FLEXDLL_CHAIN) $(FLEXLINK_FLAGS) $(FLEXLINK_DLL_LDFLAGS)
  MKEXE=$(ROOTDIR_HOST)/flexlink.opt$(EXE) -I $(ROOTDIR_HOST)/stdlib/flexdll \
        -exe -chain $(FLEXDLL_CHAIN) $(FLEXLINK_FLAGS) $(FLEXLINK_EXE_LDFLAGS)
endif # ifeq "$(wildcard $(ROOTDIR_HOST)/flexlink.opt$(EXE))" ""
endif # ifeq "$(BOOTSTRAPPING_FLEXDLL)" "false"

ifeq "$(ocamltest_program)" ""
  ocamltest = $(error ocamltest not found in $(ocamltest_directory))
else
  ocamltest := $(FLEXLINK_ENV) MKEXE="$(MKEXE)" MKDLL="$(MKDLL)" SORT=$(SORT) \
                               MAKE=$(MAKE) $(ocamltest_program)
endif

# PROMOTE is only meant to be used internally in recursive calls;
# users should call the 'promote' target explicitly.
PROMOTE =
ifeq "$(PROMOTE)" ""
  OCAMLTEST_PROMOTE_FLAG :=
else
  OCAMLTEST_PROMOTE_FLAG := -promote
endif

# KEEP_TEST_DIR_ON_SUCCESS should be set by the user (to a non-empty value)
# if they want to pass the -keep-test-dir-on-success option to ocamltest,
# to preserve test data of successful tests.
KEEP_TEST_DIR_ON_SUCCESS ?=
ifeq "$(KEEP_TEST_DIR_ON_SUCCESS)" ""
  OCAMLTEST_KEEP_TEST_DIR_ON_SUCCESS_FLAG :=
else
  OCAMLTEST_KEEP_TEST_DIR_ON_SUCCESS_FLAG := -keep-test-dir-on-success
endif

TIMEOUT ?= 600 # 10 minutes

# SHOW_TIMINGS should be set by the user (to a non-empty value) if they want
# the timings for each test file to be included in the log
SHOW_TIMINGS ?=
ifeq "$(SHOW_TIMINGS)" ""
  OCAMLTEST_SHOW_TIMINGS_FLAG :=
else
  OCAMLTEST_SHOW_TIMINGS_FLAG := -show-timings
endif

OCAMLTESTFLAGS := \
  -timeout $(TIMEOUT) \
  $(OCAMLTEST_PROMOTE_FLAG) \
  $(OCAMLTEST_KEEP_TEST_DIR_ON_SUCCESS_FLAG) \
  $(OCAMLTEST_SHOW_TIMINGS_FLAG)

# Make sure USE_RUNTIME is defined
USE_RUNTIME ?=

ifneq ($(USE_RUNTIME),)
# Check USE_RUNTIME value
ifeq ($(findstring $(USE_RUNTIME),d i),)
$(error If set, USE_RUNTIME must be equal to "d" (debug runtime) \
        or "i" (instrumented runtime))
endif
# When using the debug or instrumented runtime,
# set the runtime's verbosity to 0 by default
export OCAMLRUNPARAM?=v=0
endif

.PHONY: default
default:
	@echo "Available targets:"
	@echo "  all                        launch all tests"
	@echo "  all-foo                    launch all tests beginning with foo"
	@echo "  parallel                   launch all tests using GNU parallel"
	@echo "  parallel-foo               launch all tests beginning with foo using \
	GNU parallel"
	@echo "  one (TEST|DIR|LIST)=x      launch the specified tests ..."
	@echo "  promote (TEST|DIR|LIST)=x  promote the output for the specified \
	tests ..."
	@echo "    ... TEST=f                 ... the single test in the file f"
	@echo "    ... DIR=d                  ... the tests located in the directory d"
	@echo "    ... LIST=f                 ... the tests listed in the file f (one \
	per line)"
	@echo "  lib                        build library modules"
	@echo "  tools                      build test tools"
	@echo "  clean                      delete generated files"
	@echo "  report                     print the report for the last execution"
	@echo
	@echo "all*, parallel* and list can automatically re-run failed test"
	@echo "directories if MAX_TESTSUITE_DIR_RETRIES permits"
	@echo "(default value = $(MAX_TESTSUITE_DIR_RETRIES))"

.PHONY: all
all:
	@$(MAKE) --no-print-directory new-without-report
	@$(MAKE) --no-print-directory report

.PHONY: new-without-report
new-without-report: lib tools
	@rm -f $(failstamp)
	@(IFS=$$(printf "\r\n"); \
	$(ocamltest) -find-test-dirs tests | while IFS='' read -r dir; do \
	  echo Running tests from \'$$dir\' ... ; \
	  $(MAKE) exec-ocamltest DIR=$$dir \
	    OCAMLTESTENV=""; \
	done || echo outer loop >> $(failstamp)) 2>&1 | tee $(TESTLOG)
	@$(MAKE) check-failstamp

.PHONY: check-failstamp
check-failstamp:
	@if [ -f $(failstamp) ]; then \
	  echo 'Unexpected error in the test infrastructure:'; \
	  cat $(failstamp); \
	  rm $(failstamp); \
	  exit 1; \
	fi

.PHONY: all-%
all-%: lib tools
	@for dir in tests/$**; do \
	  $(MAKE) --no-print-directory exec-one DIR=$$dir; \
	done 2>&1 | tee $(TESTLOG)
	@$(MAKE) report

# The targets below use GNU parallel to parallelize tests
# 'make all' and 'make parallel' should be equivalent
#
# parallel uses specific logic to make sure the output of the commands
# run in parallel are not mangled. By default, it will reproduce
# the output of each completed command atomically, in order of completion.
#
# With the --keep-order option, we ask it to save the completed output
# and replay them in invocation order instead. In theory this costs
# a tiny bit of performance, but I could not measure any difference.
# In theory again, the reporting logic works fine with test outputs
# coming in in arbitrary order (so we should not need --keep-order),
# but keeping the output deterministic is guaranteed to make
# someone's life easier at least once in the future.
#
# Finally, note that the command we run has a 2>&1 redirection, as
# in the other make targets. If we removed the quoting around
# "$(MAKE) ... 2>&1", the rediction would apply to the complete output
# of parallel, and have a slightly different behavior: by default parallel
# cleanly separates the stdout and stderr output of each completed command,
# printing stderr first then stdout second (for each command).
# I chose to keep the previous behavior exactly unchanged,
# but the demangling separation is arguably nicer behavior that we might
# want to implement at the exec-one level to also have it in the 'all' target.
.PHONY: parallel-%
parallel-%: lib tools
	@echo | parallel >/dev/null 2>/dev/null \
	 || (echo "Unable to run the GNU parallel tool;";\
	     echo "You should install it before using the parallel* targets.";\
	     exit 1)
	@echo | parallel --gnu --no-notice >/dev/null 2>/dev/null \
	 || (echo "Your 'parallel' tool seems incompatible with GNU parallel.";\
	     echo "This target requires GNU parallel.";\
	     exit 1)
	@for dir in tests/$**; do echo $$dir; done \
	 | parallel --gnu --no-notice --keep-order \
	     "$(MAKE) --no-print-directory exec-one DIR={} 2>&1" \
	 | tee $(TESTLOG)
	@$(MAKE) report

.PHONY: parallel
parallel: parallel-*

.PHONY: list
list: lib tools
	@if [ -z "$(FILE)" ]; \
    then echo "No value set for variable 'FILE'."; \
    exit 1; \
  fi
	@$(MAKE) --no-print-directory one LIST="$(FILE)"

.PHONY: one
one: lib tools
	@case "$(words $(DIR) $(LIST) $(TEST))" in \
   0) echo 'No value set for variable DIR, LIST or TEST'>&2; exit 1;; \
   1) exit 0;; \
   *) echo 'Please specify exactly one of DIR, LIST or TEST'>&2; exit 1;; \
   esac
	@if [ -n '$(DIR)' ] && [ ! -d '$(DIR)' ]; then \
    echo "Directory '$(DIR)' does not exist."; exit 1; \
  fi
	@if [ -n '$(TEST)' ] && [ ! -e '$(TEST)' ]; then \
    echo "Test '$(TEST)' does not exist."; exit 1; \
  fi
	@if [ -n '$(LIST)' ] && [ ! -e '$(LIST)' ]; then \
    echo "File '$(LIST)' does not exist."; exit 1; \
  fi
	@if [ -n '$(DIR)' ] ; then \
    $(MAKE) --no-print-directory exec-one DIR=$(DIR) \
      2>&1 | tee $(TESTLOG).one ; \
   fi
	@if [ -n '$(TEST)' ] ; then \
    TERM=dumb $(OCAMLTESTENV) $(ocamltest) $(OCAMLTESTFLAGS) $(TEST) \
     2>&1 | tee $(TESTLOG).one; fi
	@if [ -n '$(LIST)' ] ; then \
     while IFS='' read -r LINE; do \
       $(MAKE) --no-print-directory exec-one DIR=$$LINE ; \
     done < $$LIST 2>&1 | tee $(TESTLOG).one ; \
   fi
	@$(MAKE) check-failstamp
	@$(MAKE) TESTLOG=$(TESTLOG).one report

.PHONY: exec-one
exec-one:
	@if $(ocamltest) -list-tests $(DIR) >/dev/null 2>&1; then \
	  echo "Running tests from '$$DIR' ..."; \
	  $(MAKE) exec-ocamltest DIR=$(DIR) \
	    OCAMLTESTENV="OCAMLTESTDIR=$(OCAMLTESTDIR)"; \
	else \
	  for dir in $(DIR)/*; do \
	    if [ -d $$dir ]; then \
	      $(MAKE) exec-one DIR=$$dir; \
	    fi; \
	  done; \
	fi

.PHONY: exec-ocamltest
exec-ocamltest:
	@if [ -z "$(DIR)" ]; then exit 1; fi
	@if [ ! -d "$(DIR)" ]; then exit 1; fi
	@(IFS=$$(printf "\r\n"); \
	$(ocamltest) -list-tests $(DIR) | while IFS='' read -r testfile; do \
	   TERM=dumb $(OCAMLTESTENV) \
	     $(ocamltest) $(OCAMLTESTFLAGS) $(DIR)/$$testfile || \
	   echo " ... testing '$$testfile' => unexpected error"; \
	done) || echo directory "$(DIR)" >>$(failstamp)

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
promote: lib tools
	@$(MAKE) one PROMOTE=true

.PHONY: lib
lib:
	@$(MAKE) -s -C lib

.PHONY: tools
tools:
	@cd tools && $(MAKE) -s BASEDIR=$(BASEDIR)

.PHONY: clean
clean:
	@$(MAKE) -C lib clean
	@cd tools && $(MAKE) BASEDIR=$(BASEDIR) clean
	find . -name '*_ocamltest*' | xargs rm -rf
	rm -f $(failstamp)

.PHONY: distclean
distclean: clean
	rm -f _log*
	$(MACE) -C lib distclean
	$(MAKE) -C tools distclean

.PHONY: report
report:
	@if [ ! -f $(TESTLOG) ]; then echo "No $(TESTLOG) file."; exit 1; fi
	@$(AWK) -f ./summarize.awk < $(TESTLOG)
