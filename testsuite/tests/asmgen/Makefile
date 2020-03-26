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

INCLUDES=\
  -I $(OTOPDIR)/parsing \
  -I $(OTOPDIR)/utils \
  -I $(OTOPDIR)/typing \
  -I $(OTOPDIR)/middle_end \
  -I $(OTOPDIR)/bytecomp \
  -I $(OTOPDIR)/asmcomp

OTHEROBJS=\
  $(OTOPDIR)/compilerlibs/ocamlcommon.cma \
  $(OTOPDIR)/compilerlibs/ocamloptcomp.cma

OBJS=parsecmmaux.cmo parsecmm.cmo lexcmm.cmo

ADD_COMPFLAGS=$(INCLUDES) -w -40 -g

default:
	@if $(BYTECODE_ONLY) || $(SKIP) ; then $(MAKE) skips ; else \
	  $(MAKE) all; \
	fi

all:
	@$(MAKE) arch codegen
	@$(MAKE) tests

main.cmo: parsecmm.cmo

codegen: parsecmm.ml lexcmm.ml $(OBJS:.cmo=.cmi) $(OBJS) main.cmo
	@$(OCAMLC) $(LINKFLAGS) -o codegen $(OTHEROBJS) $(OBJS) main.cmo

parsecmm.mli parsecmm.ml: parsecmm.mly
	@$(OCAMLYACC) -q parsecmm.mly

lexcmm.ml: lexcmm.mll
	@$(OCAMLLEX) -q lexcmm.mll

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
	@rm -f ./codegen *.out *.out.manifest *.$(O) *.exe
	@rm -f parsecmm.ml parsecmm.mli lexcmm.ml
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
