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

BASEDIR=../..

CC=$(NATIVECC)
CFLAGS=$(NATIVECCCOMPOPTS) -g

INCLUDES=\
  -I $(TTOP)/utils \
  -I $(TTOP)/typing \
  -I $(TTOP)/bytecomp \
  -I $(TTOP)/asmcomp

OTHEROBJS=\
  $(TTOP)/utils/misc.cmo \
  $(TTOP)/utils/tbl.cmo \
  $(TTOP)/utils/config.cmo \
  $(TTOP)/utils/clflags.cmo \
  $(TTOP)/utils/terminfo.cmo \
  $(TTOP)/utils/ccomp.cmo \
  $(TTOP)/utils/warnings.cmo \
  $(TTOP)/utils/consistbl.cmo \
  $(TTOP)/parsing/location.cmo \
  $(TTOP)/parsing/longident.cmo \
  $(TTOP)/parsing/syntaxerr.cmo \
  $(TTOP)/parsing/parser.cmo \
  $(TTOP)/parsing/lexer.cmo \
  $(TTOP)/parsing/parse.cmo \
  $(TTOP)/parsing/printast.cmo \
  $(TTOP)/typing/ident.cmo \
  $(TTOP)/typing/path.cmo \
  $(TTOP)/typing/primitive.cmo \
  $(TTOP)/typing/types.cmo \
  $(TTOP)/typing/btype.cmo \
  $(TTOP)/typing/oprint.cmo \
  $(TTOP)/typing/subst.cmo \
  $(TTOP)/typing/predef.cmo \
  $(TTOP)/typing/datarepr.cmo \
  $(TTOP)/typing/cmi_format.cmo \
  $(TTOP)/typing/env.cmo \
  $(TTOP)/typing/typedtree.cmo \
  $(TTOP)/typing/ctype.cmo \
  $(TTOP)/typing/printtyp.cmo \
  $(TTOP)/typing/includeclass.cmo \
  $(TTOP)/typing/mtype.cmo \
  $(TTOP)/typing/includecore.cmo \
  $(TTOP)/typing/includemod.cmo \
  $(TTOP)/typing/parmatch.cmo \
  $(TTOP)/typing/typetexp.cmo \
  $(TTOP)/typing/typedtreeMap.cmo \
  $(TTOP)/typing/cmt_format.cmo \
  $(TTOP)/typing/stypes.cmo \
  $(TTOP)/typing/typecore.cmo \
  $(TTOP)/typing/typedecl.cmo \
  $(TTOP)/typing/typeclass.cmo \
  $(TTOP)/typing/typemod.cmo \
  $(TTOP)/bytecomp/lambda.cmo \
  $(TTOP)/bytecomp/printlambda.cmo \
  $(TTOP)/bytecomp/typeopt.cmo \
  $(TTOP)/bytecomp/switch.cmo \
  $(TTOP)/bytecomp/matching.cmo \
  $(TTOP)/bytecomp/translobj.cmo \
  $(TTOP)/bytecomp/translcore.cmo \
  $(TTOP)/bytecomp/translclass.cmo \
  $(TTOP)/bytecomp/translmod.cmo \
  $(TTOP)/bytecomp/simplif.cmo \
  $(TTOP)/bytecomp/runtimedef.cmo \
  $(TTOP)/asmcomp/arch.cmo \
  $(TTOP)/asmcomp/debuginfo.cmo \
  $(TTOP)/asmcomp/cmm.cmo \
  $(TTOP)/asmcomp/printcmm.cmo \
  $(TTOP)/asmcomp/reg.cmo \
  $(TTOP)/asmcomp/mach.cmo \
  $(TTOP)/asmcomp/proc.cmo \
  $(TTOP)/asmcomp/clambda.cmo \
  $(TTOP)/asmcomp/compilenv.cmo \
  $(TTOP)/asmcomp/closure.cmo \
  $(TTOP)/asmcomp/cmmgen.cmo \
  $(TTOP)/asmcomp/printmach.cmo \
  $(TTOP)/asmcomp/selectgen.cmo \
  $(TTOP)/asmcomp/selection.cmo \
  $(TTOP)/asmcomp/comballoc.cmo \
  $(TTOP)/asmcomp/liveness.cmo \
  $(TTOP)/asmcomp/spill.cmo \
  $(TTOP)/asmcomp/split.cmo \
  $(TTOP)/asmcomp/interf.cmo \
  $(TTOP)/asmcomp/coloring.cmo \
  $(TTOP)/asmcomp/reloadgen.cmo \
  $(TTOP)/asmcomp/reload.cmo \
  $(TTOP)/asmcomp/printlinear.cmo \
  $(TTOP)/asmcomp/linearize.cmo \
  $(TTOP)/asmcomp/schedgen.cmo \
  $(TTOP)/asmcomp/scheduling.cmo \
  $(TTOP)/asmcomp/emitaux.cmo \
  $(TTOP)/asmcomp/emit.cmo \
  $(TTOP)/asmcomp/printclambda.cmo \
  $(TTOP)/asmcomp/asmgen.cmo

OBJS=parsecmmaux.cmo parsecmm.cmo lexcmm.cmo

ADD_COMPFLAGS=$(INCLUDES) -g

default:
	@if [ -z "$(BYTECODE_ONLY)" ]; then \
	  $(MAKE) all; \
	fi

all: arch codegen tests

codegen: parsecmm.ml lexcmm.ml $(OBJS:.cmo=.cmi) $(OBJS) main.cmo
	@$(OCAMLC) $(LINKFLAGS) -o codegen $(OTHEROBJS) $(OBJS) main.cmo

parsecmm.mli parsecmm.ml: parsecmm.mly
	@$(OCAMLYACC) -q parsecmm.mly

lexcmm.ml: lexcmm.mll
	@$(OCAMLLEX) -q lexcmm.mll

CASES=fib tak quicksort quicksort2 soli \
      arith checkbound tagged-fib tagged-integr tagged-quicksort tagged-tak
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

tests: $(CASES:=.o)
	@for c in $(CASES); do \
	  printf " ... testing '$$c':"; \
	  $(MAKE) one CC="$(CC) $(CFLAGS)" NAME=$$c; \
	done

one:
	@$(CC) -o $(NAME).out $(ARGS_$(NAME)) $(NAME).o $(ARCH).o || (echo " => failed" && exit 1)
	@echo " => passed"

clean: defaultclean
	@rm -f ./codegen *.out
	@rm -f parsecmm.ml parsecmm.mli lexcmm.ml
	@rm -f $(CASES:=.s)

include $(BASEDIR)/makefiles/Makefile.common

power.o: power-$(SYSTEM).o
	@cp power-$(SYSTEM).o power.o

promote:

arch: $(ARCH).o
