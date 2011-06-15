BASEDIR=../..
CC=$(NATIVECC)
CFLAGS=$(NATIVECCCOMPOPTS) -g

INCLUDES=\
  -I $(TOPDIR)/utils \
  -I $(TOPDIR)/typing \
  -I $(TOPDIR)/bytecomp \
  -I $(TOPDIR)/asmcomp

OTHEROBJS=\
  $(TOPDIR)/utils/misc.cmo \
  $(TOPDIR)/utils/tbl.cmo \
  $(TOPDIR)/utils/config.cmo \
  $(TOPDIR)/utils/clflags.cmo \
  $(TOPDIR)/utils/terminfo.cmo \
  $(TOPDIR)/utils/ccomp.cmo \
  $(TOPDIR)/utils/warnings.cmo \
  $(TOPDIR)/utils/consistbl.cmo \
  $(TOPDIR)/parsing/linenum.cmo \
  $(TOPDIR)/parsing/location.cmo \
  $(TOPDIR)/parsing/longident.cmo \
  $(TOPDIR)/parsing/syntaxerr.cmo \
  $(TOPDIR)/parsing/parser.cmo \
  $(TOPDIR)/parsing/lexer.cmo \
  $(TOPDIR)/parsing/parse.cmo \
  $(TOPDIR)/parsing/printast.cmo \
  $(TOPDIR)/typing/unused_var.cmo \
  $(TOPDIR)/typing/ident.cmo \
  $(TOPDIR)/typing/path.cmo \
  $(TOPDIR)/typing/primitive.cmo \
  $(TOPDIR)/typing/types.cmo \
  $(TOPDIR)/typing/btype.cmo \
  $(TOPDIR)/typing/oprint.cmo \
  $(TOPDIR)/typing/subst.cmo \
  $(TOPDIR)/typing/predef.cmo \
  $(TOPDIR)/typing/datarepr.cmo \
  $(TOPDIR)/typing/env.cmo \
  $(TOPDIR)/typing/typedtree.cmo \
  $(TOPDIR)/typing/ctype.cmo \
  $(TOPDIR)/typing/printtyp.cmo \
  $(TOPDIR)/typing/includeclass.cmo \
  $(TOPDIR)/typing/mtype.cmo \
  $(TOPDIR)/typing/includecore.cmo \
  $(TOPDIR)/typing/includemod.cmo \
  $(TOPDIR)/typing/parmatch.cmo \
  $(TOPDIR)/typing/typetexp.cmo \
  $(TOPDIR)/typing/stypes.cmo \
  $(TOPDIR)/typing/typecore.cmo \
  $(TOPDIR)/typing/typedecl.cmo \
  $(TOPDIR)/typing/typeclass.cmo \
  $(TOPDIR)/typing/typemod.cmo \
  $(TOPDIR)/bytecomp/lambda.cmo \
  $(TOPDIR)/bytecomp/printlambda.cmo \
  $(TOPDIR)/bytecomp/typeopt.cmo \
  $(TOPDIR)/bytecomp/switch.cmo \
  $(TOPDIR)/bytecomp/matching.cmo \
  $(TOPDIR)/bytecomp/translobj.cmo \
  $(TOPDIR)/bytecomp/translcore.cmo \
  $(TOPDIR)/bytecomp/translclass.cmo \
  $(TOPDIR)/bytecomp/translmod.cmo \
  $(TOPDIR)/bytecomp/simplif.cmo \
  $(TOPDIR)/bytecomp/runtimedef.cmo \
  $(TOPDIR)/asmcomp/arch.cmo \
  $(TOPDIR)/asmcomp/debuginfo.cmo \
  $(TOPDIR)/asmcomp/cmm.cmo \
  $(TOPDIR)/asmcomp/printcmm.cmo \
  $(TOPDIR)/asmcomp/reg.cmo \
  $(TOPDIR)/asmcomp/mach.cmo \
  $(TOPDIR)/asmcomp/proc.cmo \
  $(TOPDIR)/asmcomp/clambda.cmo \
  $(TOPDIR)/asmcomp/compilenv.cmo \
  $(TOPDIR)/asmcomp/closure.cmo \
  $(TOPDIR)/asmcomp/cmmgen.cmo \
  $(TOPDIR)/asmcomp/printmach.cmo \
  $(TOPDIR)/asmcomp/selectgen.cmo \
  $(TOPDIR)/asmcomp/selection.cmo \
  $(TOPDIR)/asmcomp/comballoc.cmo \
  $(TOPDIR)/asmcomp/liveness.cmo \
  $(TOPDIR)/asmcomp/spill.cmo \
  $(TOPDIR)/asmcomp/split.cmo \
  $(TOPDIR)/asmcomp/interf.cmo \
  $(TOPDIR)/asmcomp/coloring.cmo \
  $(TOPDIR)/asmcomp/reloadgen.cmo \
  $(TOPDIR)/asmcomp/reload.cmo \
  $(TOPDIR)/asmcomp/printlinear.cmo \
  $(TOPDIR)/asmcomp/linearize.cmo \
  $(TOPDIR)/asmcomp/schedgen.cmo \
  $(TOPDIR)/asmcomp/scheduling.cmo \
  $(TOPDIR)/asmcomp/emitaux.cmo \
  $(TOPDIR)/asmcomp/emit.cmo \
  $(TOPDIR)/asmcomp/asmgen.cmo

OBJS=parsecmmaux.cmo parsecmm.cmo lexcmm.cmo

ADD_COMPFLAGS=$(INCLUDES) -g

default: arch codegen tests

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

power.o: power-$(SYSTEM).o
	@cp power-$(SYSTEM).o power.o

promote:

include $(BASEDIR)/makefiles/Makefile.common

arch: $(ARCH).o
