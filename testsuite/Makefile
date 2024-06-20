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
  # $(IFS_LINE) for Unix simply clears IFS, so entire lines are read
  IFS_LINE = IFS=''
else # Windows
  find := /usr/bin/find
  # On Windows, ocamltest will produce Windows line-endings (\r\n) and the final
  # \r is kept by the shell. This can either be stripped with tr -d '\r' but we
  # can avoid the additional process by instead setting IFS in the while loop to
  # be the CR character (i.e. treat \r as a field delimiter).
  # The dance with $(CR_CHAR) is because POSIX doesn't provide a way to write \r
  # in a string.
  export CR_CHAR := $(shell printf "\r")
  IFS_LINE = IFS="$$CR_CHAR"
endif

ifeq "$(ocamltest_program)" ""
  ocamltest = $(error ocamltest not found in $(ocamltest_directory))
else
  ocamltest := SORT=$(SORT) MAKE=$(MAKE) $(ocamltest_program)
endif

# PROMOTE is only meant to be used internally in recursive calls;
# users should call the 'promote' target explicitly.
PROMOTE =
ifeq "$(PROMOTE)" ""
  OCAMLTEST_PROMOTE_FLAG =
else
  OCAMLTEST_PROMOTE_FLAG = -promote
endif

# KEEP_TEST_DIR_ON_SUCCESS should be set by the user (to a non-empty value)
# if they want to pass the -keep-test-dir-on-success option to ocamltest,
# to preserve test data of successful tests.

# KEEP is provided as a rather easier to remmber alias of
# KEEP_TEST_DIR_ON_SUCCESS, but to prevent the risk of naming conflict it's only
# recognised when used as make -C testsuite KEEP=1 ...
ifeq "$(origin KEEP)" "command line"
KEEP_TEST_DIR_ON_SUCCESS ?= $(KEEP)
else
KEEP_TEST_DIR_ON_SUCCESS ?=
endif

ifeq "$(KEEP_TEST_DIR_ON_SUCCESS)" ""
  OCAMLTEST_KEEP_TEST_DIR_ON_SUCCESS_FLAG =
else
  OCAMLTEST_KEEP_TEST_DIR_ON_SUCCESS_FLAG = -keep-test-dir-on-success
endif

TIMEOUT ?= 600 # 10 minutes

# SHOW_TIMINGS should be set by the user (to a non-empty value) if they want
# the timings for each test file to be included in the log
SHOW_TIMINGS ?=
OCAMLTEST_SHOW_TIMINGS_FLAG =
ifneq "$(SHOW_TIMINGS)" ""
ifeq "$(lib_unix)" "true"
  OCAMLTEST_SHOW_TIMINGS_FLAG = -show-timings
endif
endif

OCAMLTESTFLAGS = \
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
	@echo "  clean                      delete generated files"
	@echo "  report                     print the report for the last execution"
	@echo
	@echo "By default, artefacts from tests which pass are not kept, but this can"
	@echo "be changed by adding KEEP=1 to the make command line or by setting the"
	@echo "KEEP_TEST_DIR_ON_SUCCESS environment variable to a non-empty value."
	@echo
	@echo "Set the environment variable USE_RUNTIME to \"d\" or \"i\" to run"
	@echo "the tests with the debug or the instrumented runtime."

.PHONY: all
all:
	@$(MAKE) --no-print-directory new-without-report
	@$(MAKE) --no-print-directory report

.PHONY: new-without-report
new-without-report:
	@rm -f $(failstamp)
	@($(ocamltest) -find-test-dirs tests | while $(IFS_LINE) read -r dir; do \
	  echo Running tests from \'$$dir\' ... ; \
	  $(MAKE) exec-ocamltest DIR="$$dir" \
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
all-%:
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

# If make has been invoked with "-j n", pass this on to GNU parallel. parallel
# does not support -j without an argument, hence the double-filter. Note that
# GNU make normalises -j in $(MAKEFLAGS) so it will either be -j alone or -jn
# (i.e. with no space).
J_ARGUMENT = $(filter-out -j,$(filter -j%,$(MAKEFLAGS)))

.PHONY: parallel-%
parallel-%:
	@echo | parallel >/dev/null 2>/dev/null \
	 || (echo "Unable to run the GNU parallel tool;";\
	     echo "You should install it before using the parallel* targets.";\
	     exit 1)
	@echo | parallel --gnu --no-notice >/dev/null 2>/dev/null \
	 || (echo "Your 'parallel' tool seems incompatible with GNU parallel.";\
	     echo "This target requires GNU parallel.";\
	     exit 1)
	@for dir in tests/$**; do echo $$dir; done \
	 | parallel --gnu --no-notice --keep-order $(J_ARGUMENT) \
	     "$(MAKE) --no-print-directory exec-one DIR={} 2>&1" \
	 | tee $(TESTLOG)
	@$(MAKE) report

.PHONY: parallel
parallel: parallel-*

.PHONY: list
list:
	@if [ -z "$(FILE)" ]; \
    then echo "No value set for variable 'FILE'."; \
    exit 1; \
  fi
	@$(MAKE) --no-print-directory one LIST="$(FILE)"

.PHONY: one
one:
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
	@$(ocamltest) -find-test-dirs $(DIR) | while $(IFS_LINE) read -r dir; do \
	  echo "Running tests from '$$dir' ..."; \
	  $(MAKE) exec-ocamltest DIR="$$dir" \
	    OCAMLTESTENV="OCAMLTESTDIR=$(OCAMLTESTDIR)"; \
	done

.PHONY: exec-ocamltest
exec-ocamltest:
	@if [ -z "$(DIR)" ]; then exit 1; fi
	@if [ ! -d "$(DIR)" ]; then exit 1; fi
	@($(ocamltest) -list-tests $(DIR) | while $(IFS_LINE) read -r testfile; do \
	   TERM=dumb $(OCAMLTESTENV) \
	     $(ocamltest) $(OCAMLTESTFLAGS) "$(DIR)/$$testfile" || \
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
promote:
	@$(MAKE) one PROMOTE=true

.PHONY: clean
clean:
	find . -name '*_ocamltest*' | xargs rm -rf
	rm -f $(failstamp)

.PHONY: distclean
distclean: clean
	rm -f _log*

.PHONY: report
report:
	@if [ ! -f $(TESTLOG) ]; then echo "No $(TESTLOG) file."; exit 1; fi
	@$(AWK) -f ./summarize.awk < $(TESTLOG)
