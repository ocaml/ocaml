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
  -I $(OTOPDIR)/utils \
  -I $(OTOPDIR)/typing \
  -I $(OTOPDIR)/bytecomp \
  -I $(OTOPDIR)/asmcomp

OTHEROBJS=\
  $(OTOPDIR)/utils/misc.cmo \
  $(OTOPDIR)/utils/tbl.cmo \
  $(OTOPDIR)/utils/config.cmo \
  $(OTOPDIR)/utils/clflags.cmo \
  $(OTOPDIR)/utils/terminfo.cmo \
  $(OTOPDIR)/utils/ccomp.cmo \
  $(OTOPDIR)/utils/warnings.cmo \
  $(OTOPDIR)/utils/consistbl.cmo \
  $(OTOPDIR)/parsing/location.cmo \
  $(OTOPDIR)/parsing/longident.cmo \
  $(OTOPDIR)/parsing/syntaxerr.cmo \
  $(OTOPDIR)/parsing/ast_helper.cmo \
  $(OTOPDIR)/parsing/parser.cmo \
  $(OTOPDIR)/parsing/lexer.cmo \
  $(OTOPDIR)/parsing/parse.cmo \
  $(OTOPDIR)/parsing/printast.cmo \
  $(OTOPDIR)/typing/ident.cmo \
  $(OTOPDIR)/typing/path.cmo \
  $(OTOPDIR)/typing/primitive.cmo \
  $(OTOPDIR)/typing/types.cmo \
  $(OTOPDIR)/typing/btype.cmo \
  $(OTOPDIR)/typing/oprint.cmo \
  $(OTOPDIR)/typing/subst.cmo \
  $(OTOPDIR)/typing/predef.cmo \
  $(OTOPDIR)/typing/datarepr.cmo \
  $(OTOPDIR)/typing/cmi_format.cmo \
  $(OTOPDIR)/typing/env.cmo \
  $(OTOPDIR)/typing/typedtree.cmo \
  $(OTOPDIR)/typing/ctype.cmo \
  $(OTOPDIR)/typing/printtyp.cmo \
  $(OTOPDIR)/typing/includeclass.cmo \
  $(OTOPDIR)/typing/mtype.cmo \
  $(OTOPDIR)/typing/includecore.cmo \
  $(OTOPDIR)/typing/includemod.cmo \
  $(OTOPDIR)/typing/parmatch.cmo \
  $(OTOPDIR)/typing/typetexp.cmo \
  $(OTOPDIR)/typing/typedtreeMap.cmo \
  $(OTOPDIR)/typing/cmt_format.cmo \
  $(OTOPDIR)/typing/stypes.cmo \
  $(OTOPDIR)/typing/typecore.cmo \
  $(OTOPDIR)/typing/typedecl.cmo \
  $(OTOPDIR)/typing/typeclass.cmo \
  $(OTOPDIR)/typing/typemod.cmo \
  $(OTOPDIR)/bytecomp/lambda.cmo \
  $(OTOPDIR)/bytecomp/printlambda.cmo \
  $(OTOPDIR)/bytecomp/typeopt.cmo \
  $(OTOPDIR)/bytecomp/switch.cmo \
  $(OTOPDIR)/bytecomp/matching.cmo \
  $(OTOPDIR)/bytecomp/translobj.cmo \
  $(OTOPDIR)/bytecomp/translcore.cmo \
  $(OTOPDIR)/bytecomp/translclass.cmo \
  $(OTOPDIR)/bytecomp/translmod.cmo \
  $(OTOPDIR)/bytecomp/simplif.cmo \
  $(OTOPDIR)/bytecomp/runtimedef.cmo \
  $(OTOPDIR)/asmcomp/arch.cmo \
  $(OTOPDIR)/asmcomp/debuginfo.cmo \
  $(OTOPDIR)/asmcomp/cmm.cmo \
  $(OTOPDIR)/asmcomp/printcmm.cmo \
  $(OTOPDIR)/asmcomp/reg.cmo \
  $(OTOPDIR)/asmcomp/mach.cmo \
  $(OTOPDIR)/asmcomp/proc.cmo \
  $(OTOPDIR)/asmcomp/clambda.cmo \
  $(OTOPDIR)/asmcomp/compilenv.cmo \
  $(OTOPDIR)/asmcomp/closure.cmo \
  $(OTOPDIR)/asmcomp/cmmgen.cmo \
  $(OTOPDIR)/asmcomp/printmach.cmo \
  $(OTOPDIR)/asmcomp/selectgen.cmo \
  $(OTOPDIR)/asmcomp/selection.cmo \
  $(OTOPDIR)/asmcomp/comballoc.cmo \
  $(OTOPDIR)/asmcomp/liveness.cmo \
  $(OTOPDIR)/asmcomp/spill.cmo \
  $(OTOPDIR)/asmcomp/split.cmo \
  $(OTOPDIR)/asmcomp/interf.cmo \
  $(OTOPDIR)/asmcomp/coloring.cmo \
  $(OTOPDIR)/asmcomp/reloadgen.cmo \
  $(OTOPDIR)/asmcomp/reload.cmo \
  $(OTOPDIR)/asmcomp/printlinear.cmo \
  $(OTOPDIR)/asmcomp/linearize.cmo \
  $(OTOPDIR)/asmcomp/schedgen.cmo \
  $(OTOPDIR)/asmcomp/scheduling.cmo \
  $(OTOPDIR)/asmcomp/emitaux.cmo \
  $(OTOPDIR)/asmcomp/emit.cmo \
  $(OTOPDIR)/asmcomp/printclambda.cmo \
  $(OTOPDIR)/asmcomp/asmgen.cmo

OBJS=parsecmmaux.cmo parsecmm.cmo lexcmm.cmo

ADD_COMPFLAGS=$(INCLUDES) -g

default:
	@if $(BYTECODE_ONLY) || [ -z "$(ASPP)" ]; then : ; else \
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
	@$(CC) -o $(NAME).out $(ARGS_$(NAME)) $(NAME).o $(ARCH).o \
	&& echo " => passed" || echo " => failed"

clean: defaultclean
	@rm -f ./codegen *.out
	@rm -f parsecmm.ml parsecmm.mli lexcmm.ml
	@rm -f $(CASES:=.s)

include $(BASEDIR)/makefiles/Makefile.common

power.o: power-$(SYSTEM).o
	@cp power-$(SYSTEM).o power.o

promote:

arch: $(ARCH).o
