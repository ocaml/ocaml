#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
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

BASEDIR=../..

include $(BASEDIR)/../config/Makefile

default:
	@if $(BYTECODE_ONLY) || $(SKIP) ; then $(MAKE) skips ; else \
	  $(MAKE) all; \
	fi

all:
	@$(MAKE) arch
	@$(MAKE) tests

CASES=fib tak quicksort quicksort2 soli \
      arith checkbound tagged-fib tagged-integr tagged-quicksort tagged-tak \
      catch-try catch-rec even-odd even-odd-spill pgcd
ARGS_fib=-DINT_INT -DFUN=fib main.c
ARGS_tak=-DUNIT_INT -DFUN=takmain main.c
ARGS_quicksort=-DSORT -DFUN=quicksort main.c
ARGS_quicksort2=-DSORT -DFUN=quicksort main.c
ARGS_soli=-DUNIT_INT -DFUN=solitaire main.c
ARGS_integr=-DINT_FLOAT -DFUN=test main.c
ARGS_arith=mainarith.c
ARGS_checkbound=-DCHECKBOUND main.c
ARGS_tagged-fib=-DINT_INT -DFUN=fib main.c
ARGS_tagged-integr=-DINT_FLOAT -DFUN=test main.c
ARGS_tagged-quicksort=-DSORT -DFUN=quicksort main.c
ARGS_tagged-tak=-DUNIT_INT -DFUN=takmain main.c
ARGS_catch-try=-DINT_INT -DFUN=catch_exit main.c
ARGS_catch-rec=-DINT_INT -DFUN=catch_fact main.c
ARGS_even-odd=-DINT_INT -DFUN=is_even main.c
ARGS_even-odd-spill=-DINT_INT -DFUN=is_even main.c
ARGS_pgcd=-DINT_INT -DFUN=pgcd_30030 main.c

skips:
	@for c in $(CASES); do \
	  echo " ... testing '$$c': => skipped"; \
	done

one:
	@$(call CCOMP,$(NAME).out $(ARGS_$(NAME)) $(NAME).$(O) $(ARCH).$(O)) \
	&& echo " => passed" || echo " => failed"

clean: defaultclean
	@rm -f *.out *.out.manifest *.$(O) *.exe
	@rm -f $(CASES:=.s)

include $(BASEDIR)/makefiles/Makefile.common

ifeq "$(CCOMPTYPE)-$(ARCH)" "msvc-amd64"
# these tests are not ported to MSVC64 yet
SKIP=true
else
SKIP=false
endif

ifeq "$(WITH_SPACETIME)" "true"
# These tests have not been ported for Spacetime
SKIP=true
endif

ifeq ($(CCOMPTYPE),msvc)
CCOMP=set -o pipefail ; $(CC) $(CFLAGS) /Fe$(1) | tail -n +2
else
CCOMP=$(CC) $(CFLAGS) -o $(1)
endif
tests: $(CASES:=.$(O))
	@for c in $(CASES); do \
	  printf " ... testing '$$c':"; \
	  $(MAKE) one NAME=$$c; \
	done

promote:

arch: $(ARCH).$(O)

i386.obj: i386nt.asm
	@set -o pipefail ; \
	$(ASM) $@ $^ | tail -n +2
