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

INCLUDES=\
  -I $(OTOPDIR)/parsing \
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
  $(OTOPDIR)/utils/ext_types.cmo \
  $(OTOPDIR)/utils/sort_connected_components.cmo \
  $(OTOPDIR)/parsing/location.cmo \
  $(OTOPDIR)/parsing/longident.cmo \
  $(OTOPDIR)/parsing/syntaxerr.cmo \
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
  $(OTOPDIR)/bytecomp/debuginfo.cmo \
  $(OTOPDIR)/bytecomp/symbol.cmo \
  $(OTOPDIR)/bytecomp/abstract_identifiers.cmo \
  $(OTOPDIR)/bytecomp/flambda.cmo \
  $(OTOPDIR)/bytecomp/printflambda.cmo \
  $(OTOPDIR)/bytecomp/flambdaiter.cmo \
  $(OTOPDIR)/bytecomp/flambdacheck.cmo \
  $(OTOPDIR)/bytecomp/flambdagen.cmo \
  $(OTOPDIR)/bytecomp/flambdaconstants.cmo \
  $(OTOPDIR)/asmcomp/arch.cmo \
  $(OTOPDIR)/asmcomp/cmm.cmo \
  $(OTOPDIR)/asmcomp/printcmm.cmo \
  $(OTOPDIR)/asmcomp/reg.cmo \
  $(OTOPDIR)/asmcomp/mach.cmo \
  $(OTOPDIR)/asmcomp/proc.cmo \
  $(OTOPDIR)/asmcomp/flambdaexport.cmo \
  $(OTOPDIR)/asmcomp/clambda.cmo \
  $(OTOPDIR)/asmcomp/compilenv.cmo \
  $(OTOPDIR)/asmcomp/flambdaanf.cmo \
  $(OTOPDIR)/asmcomp/flambdareachability.cmo \
  $(OTOPDIR)/asmcomp/flambdapasses.cmo \
  $(OTOPDIR)/asmcomp/flambdasimplify.cmo \
  $(OTOPDIR)/asmcomp/flambdaspec.cmo \
  $(OTOPDIR)/asmcomp/flambdasubst.cmo \
  $(OTOPDIR)/asmcomp/flambdaifstaticraise.cmo \
  $(OTOPDIR)/asmcomp/flambdapurity.cmo \
  $(OTOPDIR)/asmcomp/flambdamovelets.cmo \
  $(OTOPDIR)/asmcomp/flambdasym.cmo \
  $(OTOPDIR)/asmcomp/clambdagen.cmo \
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

OBJS= \
	test_utils.cmo test_flambdacheck.cmo test_flambdagen.cmo \
	test_connected_components.cmo test_kept_parameters.cmo \
	test_recursives.cmo test_movelets.cmo test_flambdaifstaticraise.cmo \
	test_purity.cmo test_flambdareach.cmo

ADD_COMPFLAGS=$(INCLUDES) -g

all: run

main.cmo: $(OBJS:.cmo=.cmi)

main: $(OBJS:.cmo=.cmi) $(OBJS) main.cmo
	@$(OCAMLC) $(LINKFLAGS) -g -o main $(OTHEROBJS) $(OBJS) main.cmo

run: main
	@$(OCAMLRUN) main \
	&& echo " => passed" || echo " => failed"

clean: defaultclean
	@rm -f ./main *.out

include $(BASEDIR)/makefiles/Makefile.common

promote:
