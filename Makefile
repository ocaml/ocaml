#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# The main Makefile

ROOTDIR = .
# NOTE: it is important that the OCAMLDEP and OCAMLLEX variables
# are defined *before* Makefile.common gets included, so that
# their local definitions here take precedence over their
# general shared definitions in Makefile.common.
OCAMLLEX ?= $(BOOT_OCAMLLEX)
include Makefile.common
include Makefile.best_binaries

.PHONY: defaultentry
defaultentry: $(DEFAULT_BUILD_TARGET)

include stdlib/StdlibModules

CAMLC = $(BOOT_OCAMLC) $(BOOT_STDLIBFLAGS) -use-prims runtime/primitives
CAMLOPT=$(OCAMLRUN) ./ocamlopt$(EXE) $(STDLIBFLAGS) -I otherlibs/dynlink
ARCHES=amd64 arm64 power s390x riscv
VPATH = utils parsing typing bytecomp file_formats lambda middle_end \
  middle_end/closure middle_end/flambda middle_end/flambda/base_types \
  asmcomp driver toplevel tools $(addprefix otherlibs/, $(ALL_OTHERLIBS))
INCLUDES = $(addprefix -I ,$(VPATH))

ifeq "$(strip $(NATDYNLINKOPTS))" ""
OCAML_NATDYNLINKOPTS=
else
OCAML_NATDYNLINKOPTS = -ccopt "$(NATDYNLINKOPTS)"
endif

OC_OCAMLDEPDIRS = $(VPATH)

# This list is passed to expunge, which accepts both uncapitalized and
# capitalized module names.
PERVASIVES=$(STDLIB_MODULES) outcometree topprinters topdirs toploop

LIBFILES=stdlib.cma std_exit.cmo *.cmi $(HEADER_NAME)

COMPLIBDIR=$(LIBDIR)/compiler-libs

TOPINCLUDES=$(addprefix -I otherlibs/,$(filter-out %threads,$(OTHERLIBRARIES)))

expunge := expunge$(EXE)

# Targets and dependencies for the compilerlibs/*.{cma,cmxa} archives

utils_SOURCES = $(addprefix utils/, \
  config.mli config.ml \
  build_path_prefix_map.mli build_path_prefix_map.ml \
  misc.mli misc.ml \
  identifiable.mli identifiable.ml \
  numbers.mli numbers.ml \
  arg_helper.mli arg_helper.ml \
  local_store.mli local_store.ml \
  load_path.mli load_path.ml \
  clflags.mli clflags.ml \
  profile.mli profile.ml \
  terminfo.mli terminfo.ml \
  ccomp.mli ccomp.ml \
  warnings.mli warnings.ml \
  consistbl.mli consistbl.ml \
  strongly_connected_components.mli strongly_connected_components.ml \
  targetint.mli targetint.ml \
  int_replace_polymorphic_compare.mli int_replace_polymorphic_compare.ml \
  domainstate.mli domainstate.ml \
  binutils.mli binutils.ml \
  lazy_backtrack.mli lazy_backtrack.ml \
  diffing.mli diffing.ml \
  diffing_with_keys.mli diffing_with_keys.ml \
  compression.mli compression.ml)

parsing_SOURCES = $(addprefix parsing/, \
  location.mli location.ml \
  unit_info.mli unit_info.ml \
  asttypes.mli \
  longident.mli longident.ml \
  parsetree.mli \
  docstrings.mli docstrings.ml \
  syntaxerr.mli syntaxerr.ml \
  ast_helper.mli ast_helper.ml \
  ast_iterator.mli ast_iterator.ml \
  builtin_attributes.mli builtin_attributes.ml \
  camlinternalMenhirLib.mli camlinternalMenhirLib.ml \
  parser.mly \
  lexer.mll \
  pprintast.mli pprintast.ml \
  parse.mli parse.ml \
  printast.mli printast.ml \
  ast_mapper.mli ast_mapper.ml \
  attr_helper.mli attr_helper.ml \
  ast_invariants.mli ast_invariants.ml \
  depend.mli depend.ml)

typing_SOURCES = \
  typing/annot.mli \
  typing/value_rec_types.mli \
  typing/ident.mli typing/ident.ml \
  typing/path.mli typing/path.ml \
  typing/primitive.mli typing/primitive.ml \
  typing/type_immediacy.mli typing/type_immediacy.ml \
  typing/outcometree.mli \
  typing/shape.mli typing/shape.ml \
  typing/types.mli typing/types.ml \
  typing/btype.mli typing/btype.ml \
  typing/oprint.mli typing/oprint.ml \
  typing/subst.mli typing/subst.ml \
  typing/predef.mli typing/predef.ml \
  typing/datarepr.mli typing/datarepr.ml \
  file_formats/cmi_format.mli file_formats/cmi_format.ml \
  typing/persistent_env.mli typing/persistent_env.ml \
  typing/env.mli typing/env.ml \
  typing/errortrace.mli typing/errortrace.ml \
  typing/typedtree.mli typing/typedtree.ml \
  typing/signature_group.mli typing/signature_group.ml \
  typing/printtyped.mli typing/printtyped.ml \
  typing/ctype.mli typing/ctype.ml \
  typing/printtyp.mli typing/printtyp.ml \
  typing/includeclass.mli typing/includeclass.ml \
  typing/mtype.mli typing/mtype.ml \
  typing/envaux.mli typing/envaux.ml \
  typing/includecore.mli typing/includecore.ml \
  typing/tast_iterator.mli typing/tast_iterator.ml \
  typing/tast_mapper.mli typing/tast_mapper.ml \
  typing/stypes.mli typing/stypes.ml \
  typing/shape_reduce.mli typing/shape_reduce.ml \
  file_formats/cmt_format.mli file_formats/cmt_format.ml \
  typing/cmt2annot.mli typing/cmt2annot.ml \
  typing/untypeast.mli typing/untypeast.ml \
  typing/includemod.mli typing/includemod.ml \
  typing/includemod_errorprinter.mli typing/includemod_errorprinter.ml \
  typing/typetexp.mli typing/typetexp.ml \
  typing/printpat.mli typing/printpat.ml \
  typing/patterns.mli typing/patterns.ml \
  typing/parmatch.mli typing/parmatch.ml \
  typing/typedecl_properties.mli typing/typedecl_properties.ml \
  typing/typedecl_variance.mli typing/typedecl_variance.ml \
  typing/typedecl_unboxed.mli typing/typedecl_unboxed.ml \
  typing/typedecl_immediacy.mli typing/typedecl_immediacy.ml \
  typing/typedecl_separability.mli typing/typedecl_separability.ml \
  typing/typeopt.mli typing/typeopt.ml \
  typing/typedecl.mli typing/typedecl.ml \
  typing/value_rec_check.mli typing/value_rec_check.ml \
  typing/typecore.mli typing/typecore.ml \
  typing/typeclass.mli typing/typeclass.ml \
  typing/typemod.mli typing/typemod.ml

lambda_SOURCES = $(addprefix lambda/, \
  debuginfo.mli debuginfo.ml \
  lambda.mli lambda.ml \
  printlambda.mli printlambda.ml \
  switch.mli switch.ml \
  matching.mli matching.ml \
  value_rec_compiler.mli value_rec_compiler.ml \
  translobj.mli translobj.ml \
  translattribute.mli translattribute.ml \
  translprim.mli translprim.ml \
  translcore.mli translcore.ml \
  translclass.mli translclass.ml \
  translmod.mli translmod.ml \
  tmc.mli tmc.ml \
  simplif.mli simplif.ml \
  runtimedef.mli runtimedef.ml)

comp_SOURCES = \
  file_formats/cmo_format.mli \
  file_formats/cmx_format.mli \
  file_formats/cmxs_format.mli \
  bytecomp/meta.mli bytecomp/meta.ml \
  bytecomp/opcodes.mli bytecomp/opcodes.ml \
  bytecomp/bytesections.mli bytecomp/bytesections.ml \
  bytecomp/dll.mli bytecomp/dll.ml \
  bytecomp/symtable.mli bytecomp/symtable.ml \
  driver/pparse.mli driver/pparse.ml \
  driver/compenv.mli driver/compenv.ml \
  driver/main_args.mli driver/main_args.ml \
  driver/compmisc.mli driver/compmisc.ml \
  driver/makedepend.mli driver/makedepend.ml \
  driver/compile_common.mli driver/compile_common.ml
# All file format descriptions (including cmx{,s}) are in the
# ocamlcommon library so that ocamlobjinfo can depend on them.

ocamlcommon_SOURCES = \
  $(utils_SOURCES) $(parsing_SOURCES) $(typing_SOURCES) \
  $(lambda_SOURCES) $(comp_SOURCES)

ocamlbytecomp_SOURCES = \
  bytecomp/instruct.mli bytecomp/instruct.ml \
  bytecomp/bytegen.mli bytecomp/bytegen.ml \
  bytecomp/printinstr.mli bytecomp/printinstr.ml \
  bytecomp/emitcode.mli bytecomp/emitcode.ml \
  bytecomp/bytelink.mli bytecomp/bytelink.ml \
  bytecomp/bytelibrarian.mli bytecomp/bytelibrarian.ml \
  bytecomp/bytepackager.mli bytecomp/bytepackager.ml \
  driver/errors.mli driver/errors.ml \
  driver/compile.mli driver/compile.ml \
  driver/maindriver.mli driver/maindriver.ml

intel_SOURCES = \
  x86_ast.mli \
  x86_proc.mli x86_proc.ml \
  x86_dsl.mli x86_dsl.ml \
  x86_gas.mli x86_gas.ml \
  x86_masm.mli x86_masm.ml

asmcomp_SOURCES = \
  $(addprefix asmcomp/, $(arch_specific_SOURCES)) \
  asmcomp/arch.mli asmcomp/arch.ml \
  asmcomp/cmm.mli asmcomp/cmm.ml \
  asmcomp/printcmm.mli asmcomp/printcmm.ml \
  asmcomp/reg.mli asmcomp/reg.ml \
  asmcomp/mach.mli asmcomp/mach.ml \
  asmcomp/proc.mli asmcomp/proc.ml \
  asmcomp/strmatch.mli asmcomp/strmatch.ml \
  asmcomp/cmmgen_state.mli asmcomp/cmmgen_state.ml \
  asmcomp/cmm_helpers.mli asmcomp/cmm_helpers.ml \
  asmcomp/afl_instrument.mli asmcomp/afl_instrument.ml \
  asmcomp/thread_sanitizer.mli asmcomp/thread_sanitizer.ml \
  asmcomp/cmmgen.mli asmcomp/cmmgen.ml \
  asmcomp/cmm_invariants.mli asmcomp/cmm_invariants.ml \
  asmcomp/interval.mli asmcomp/interval.ml \
  asmcomp/printmach.mli asmcomp/printmach.ml \
  asmcomp/dataflow.mli asmcomp/dataflow.ml \
  asmcomp/polling.mli asmcomp/polling.ml \
  asmcomp/selectgen.mli asmcomp/selectgen.ml \
  asmcomp/selection.mli asmcomp/selection.ml \
  asmcomp/comballoc.mli asmcomp/comballoc.ml \
  asmcomp/CSEgen.mli asmcomp/CSEgen.ml \
  asmcomp/CSE.mli asmcomp/CSE.ml \
  asmcomp/liveness.mli asmcomp/liveness.ml \
  asmcomp/spill.mli asmcomp/spill.ml \
  asmcomp/split.mli asmcomp/split.ml \
  asmcomp/interf.mli asmcomp/interf.ml \
  asmcomp/coloring.mli asmcomp/coloring.ml \
  asmcomp/linscan.mli asmcomp/linscan.ml \
  asmcomp/reloadgen.mli asmcomp/reloadgen.ml \
  asmcomp/reload.mli asmcomp/reload.ml \
  asmcomp/deadcode.mli asmcomp/deadcode.ml \
  asmcomp/stackframegen.mli asmcomp/stackframegen.ml \
  asmcomp/stackframe.mli asmcomp/stackframe.ml \
  asmcomp/linear.mli asmcomp/linear.ml \
  asmcomp/printlinear.mli asmcomp/printlinear.ml \
  asmcomp/linearize.mli asmcomp/linearize.ml \
  file_formats/linear_format.mli file_formats/linear_format.ml \
  asmcomp/schedgen.mli asmcomp/schedgen.ml \
  asmcomp/scheduling.mli asmcomp/scheduling.ml \
  asmcomp/branch_relaxation.mli asmcomp/branch_relaxation.ml \
  asmcomp/emitaux.mli asmcomp/emitaux.ml \
  asmcomp/emit.mli asmcomp/emit.ml \
  asmcomp/asmgen.mli asmcomp/asmgen.ml \
  asmcomp/asmlink.mli asmcomp/asmlink.ml \
  asmcomp/asmlibrarian.mli asmcomp/asmlibrarian.ml \
  asmcomp/asmpackager.mli asmcomp/asmpackager.ml \
  driver/opterrors.mli driver/opterrors.ml \
  driver/optcompile.mli driver/optcompile.ml \
  driver/optmaindriver.mli driver/optmaindriver.ml

# Files under middle_end/ are not to reference files under asmcomp/.
# This ensures that the middle end can be linked (e.g. for objinfo) even when
# the native code compiler is not present for some particular target.

middle_end_closure_SOURCES = $(addprefix middle_end/closure/, \
  closure.mli closure.ml \
  closure_middle_end.mli closure_middle_end.ml)

# Owing to dependencies through [Compilenv], which would be
# difficult to remove, some of the lower parts of Flambda (anything that is
# saved in a .cmx file) have to be included in the [MIDDLE_END] stanza, below.
middle_end_flambda_SOURCES = \
$(addprefix middle_end/flambda/, \
  import_approx.mli import_approx.ml \
  lift_code.mli lift_code.ml \
  closure_conversion_aux.mli closure_conversion_aux.ml \
  closure_conversion.mli closure_conversion.ml \
  initialize_symbol_to_let_symbol.mli initialize_symbol_to_let_symbol.ml \
  lift_let_to_initialize_symbol.mli lift_let_to_initialize_symbol.ml \
  find_recursive_functions.mli find_recursive_functions.ml \
  invariant_params.mli invariant_params.ml \
  inconstant_idents.mli inconstant_idents.ml \
  alias_analysis.mli alias_analysis.ml \
  lift_constants.mli lift_constants.ml \
  share_constants.mli share_constants.ml \
  simplify_common.mli simplify_common.ml \
  remove_unused_arguments.mli remove_unused_arguments.ml \
  remove_unused_closure_vars.mli remove_unused_closure_vars.ml \
  remove_unused_program_constructs.mli remove_unused_program_constructs.ml \
  simplify_boxed_integer_ops.mli simplify_boxed_integer_ops.ml \
  simplify_primitives.mli simplify_primitives.ml \
  inlining_stats_types.mli inlining_stats_types.ml \
  inlining_stats.mli inlining_stats.ml \
  inline_and_simplify_aux.mli inline_and_simplify_aux.ml \
  inlining_decision_intf.mli \
  remove_free_vars_equal_to_args.mli remove_free_vars_equal_to_args.ml \
  extract_projections.mli extract_projections.ml \
  augment_specialised_args.mli augment_specialised_args.ml \
  unbox_free_vars_of_closures.mli unbox_free_vars_of_closures.ml \
  unbox_specialised_args.mli unbox_specialised_args.ml \
  unbox_closures.mli unbox_closures.ml \
  inlining_transforms.mli inlining_transforms.ml \
  inlining_decision.mli inlining_decision.ml \
  inline_and_simplify.mli inline_and_simplify.ml \
  ref_to_variables.mli ref_to_variables.ml \
  flambda_invariants.mli flambda_invariants.ml \
  traverse_for_exported_symbols.mli traverse_for_exported_symbols.ml \
  build_export_info.mli build_export_info.ml \
  closure_offsets.mli closure_offsets.ml \
  un_anf.mli un_anf.ml \
  flambda_to_clambda.mli flambda_to_clambda.ml \
  flambda_middle_end.mli flambda_middle_end.ml \
  simplify_boxed_integer_ops_intf.mli)

ocamlmiddleend_SOURCES = \
$(addprefix middle_end/, \
  internal_variable_names.mli internal_variable_names.ml \
  linkage_name.mli linkage_name.ml \
  compilation_unit.mli compilation_unit.ml \
  variable.mli variable.ml \
  $(addprefix flambda/base_types/, \
    closure_element.mli closure_element.ml \
    closure_id.mli closure_id.ml) \
  symbol.mli symbol.ml \
  backend_var.mli backend_var.ml \
  clambda_primitives.mli clambda_primitives.ml \
  printclambda_primitives.mli printclambda_primitives.ml \
  clambda.mli clambda.ml \
  printclambda.mli printclambda.ml \
  semantics_of_primitives.mli semantics_of_primitives.ml \
  convert_primitives.mli convert_primitives.ml \
  $(addprefix flambda/, \
    $(addprefix base_types/, \
      id_types.mli id_types.ml \
      export_id.mli export_id.ml \
      tag.mli tag.ml \
      mutable_variable.mli mutable_variable.ml \
      set_of_closures_id.mli set_of_closures_id.ml \
      set_of_closures_origin.mli set_of_closures_origin.ml \
      closure_origin.mli closure_origin.ml \
      var_within_closure.mli var_within_closure.ml \
      static_exception.mli static_exception.ml) \
    pass_wrapper.mli pass_wrapper.ml \
    allocated_const.mli allocated_const.ml \
    parameter.mli parameter.ml \
    projection.mli projection.ml \
    flambda.mli flambda.ml \
    flambda_iterators.mli flambda_iterators.ml \
    flambda_utils.mli flambda_utils.ml \
    freshening.mli freshening.ml \
    effect_analysis.mli effect_analysis.ml \
    inlining_cost.mli inlining_cost.ml \
    simple_value_approx.mli simple_value_approx.ml \
    export_info.mli export_info.ml \
    export_info_for_pack.mli export_info_for_pack.ml) \
  compilenv.mli compilenv.ml \
  backend_intf.mli) \
  $(middle_end_closure_SOURCES) \
  $(middle_end_flambda_SOURCES)

ocamloptcomp_SOURCES = $(ocamlmiddleend_SOURCES) $(asmcomp_SOURCES)

ocamltoplevel_SOURCES = $(addprefix toplevel/, \
  genprintval.mli genprintval.ml \
  topcommon.mli topcommon.ml \
  native/tophooks.mli native/tophooks.ml \
  byte/topeval.mli byte/topeval.ml \
  native/topeval.mli native/topeval.ml \
  byte/trace.mli byte/trace.ml \
  native/trace.mli native/trace.ml \
  toploop.mli toploop.ml \
  topprinters.mli topprinters.ml \
  topdirs.mli topdirs.ml \
  byte/topmain.mli byte/topmain.ml \
  native/topmain.mli native/topmain.ml)

TOPLEVEL_SHARED_MLIS = topeval.mli trace.mli topmain.mli
TOPLEVEL_SHARED_CMIS = $(TOPLEVEL_SHARED_MLIS:%.mli=%.cmi)
TOPLEVEL_SHARED_ARTEFACTS = $(TOPLEVEL_SHARED_MLIS) $(TOPLEVEL_SHARED_CMIS)

$(addprefix toplevel/byte/, $(TOPLEVEL_SHARED_CMIS)):\
toplevel/byte/%.cmi: toplevel/%.cmi
	cp $< toplevel/$*.mli $(@D)

$(addprefix toplevel/native/, $(TOPLEVEL_SHARED_CMIS)):\
toplevel/native/%.cmi: toplevel/%.cmi
	cp $< toplevel/$*.mli $(@D)

beforedepend::
	cd toplevel ; cp $(TOPLEVEL_SHARED_MLIS) byte/
	cd toplevel ; cp $(TOPLEVEL_SHARED_MLIS) native/

partialclean::
	cd toplevel/byte ; rm -f $(TOPLEVEL_SHARED_ARTEFACTS)
	cd toplevel/native ; rm -f $(TOPLEVEL_SHARED_ARTEFACTS)

ALL_CONFIG_CMO = utils/config_main.cmo utils/config_boot.cmo

utils/config_%.mli: utils/config.mli
	cp $^ $@

beforedepend:: utils/config_main.mli utils/config_boot.mli

$(addprefix compilerlibs/ocamlcommon., cma cmxa): \
  OC_COMMON_LINKFLAGS += -linkall

COMPRESSED_MARSHALING_FLAGS=-cclib -lcomprmarsh \
           $(patsubst %, -ccopt %, $(filter-out -l%,$(ZSTD_LIBS))) \
           $(patsubst %, -cclib %, $(filter -l%,$(ZSTD_LIBS))) \

compilerlibs/ocamlcommon.cmxa: \
  OC_NATIVE_LINKFLAGS += $(COMPRESSED_MARSHALING_FLAGS)

compilerlibs/ocamlcommon.cmxa: stdlib/libcomprmarsh.$(A)

partialclean::
	rm -f compilerlibs/ocamlcommon.cma

partialclean::
	rm -f compilerlibs/ocamlcommon.cmxa \
	      compilerlibs/ocamlcommon.a compilerlibs/ocamlcommon.lib


partialclean::
	rm -f compilerlibs/ocamlbytecomp.cma

partialclean::
	rm -f compilerlibs/ocamlbytecomp.cmxa \
	      compilerlibs/ocamlbytecomp.a compilerlibs/ocamlbytecomp.lib


partialclean::
	rm -f compilerlibs/ocamlmiddleend.cma \
	      compilerlibs/ocamlmiddleend.cmxa \
	      compilerlibs/ocamlmiddleend.a \
	      compilerlibs/ocamlmiddleend.lib


partialclean::
	rm -f compilerlibs/ocamloptcomp.cma

partialclean::
	rm -f compilerlibs/ocamloptcomp.cmxa \
	      compilerlibs/ocamloptcomp.a compilerlibs/ocamloptcomp.lib


compilerlibs/ocamltoplevel.cma: VPATH += toplevel/byte
partialclean::
	rm -f compilerlibs/ocamltoplevel.cma

compilerlibs/ocamltoplevel.cmxa: VPATH += toplevel/native
partialclean::
	rm -f compilerlibs/ocamltoplevel.cmxa \
	  compilerlibs/ocamltoplevel.a compilerlibs/ocamltoplevel.lib

# The configuration file

utils/config.ml: \
  utils/config_$(if $(filter true,$(IN_COREBOOT_CYCLE)),boot,main).ml
	$(V_GEN)cp $< $@
utils/config_boot.ml: utils/config.fixed.ml utils/config.common.ml
	$(V_GEN)cat $^ > $@

utils/config_main.ml: utils/config.generated.ml utils/config.common.ml
	$(V_GEN)cat $^ > $@

.PHONY: reconfigure
reconfigure:
	ac_read_git_config=true ./configure $(CONFIGURE_ARGS)

utils/domainstate.ml: utils/domainstate.ml.c runtime/caml/domain_state.tbl
	$(V_GEN)$(CPP) -I runtime/caml $< > $@

utils/domainstate.mli: utils/domainstate.mli.c runtime/caml/domain_state.tbl
	$(V_GEN)$(CPP) -I runtime/caml $< > $@

configure: tools/autogen configure.ac aclocal.m4 build-aux/ocaml_version.m4
	$<

.PHONY: partialclean
partialclean::
	rm -f utils/config.ml \
	      utils/config_main.ml utils/config_main.mli \
	      utils/config_boot.ml utils/config_boot.mli \
        utils/domainstate.ml utils/domainstate.mli

.PHONY: beforedepend
beforedepend:: \
  utils/config.ml utils/config_boot.ml utils/config_main.ml \
  utils/domainstate.ml utils/domainstate.mli

ocamllex_PROGRAMS = $(addprefix lex/,ocamllex ocamllex.opt)

ocamlyacc_PROGRAM = yacc/ocamlyacc

# Tools to be compiled to native and bytecode, then installed
TOOLS_TO_INSTALL_NAT = ocamldep ocamlobjinfo

# Tools to be compiled to bytecode only, then installed
TOOLS_TO_INSTALL_BYT = \
  ocamlcmt ocamlprof ocamlcp ocamlmklib ocamlmktop

ifeq "$(NATIVE_COMPILER)" "true"
TOOLS_TO_INSTALL_BYT += ocamloptp
endif

# Clean should remove tools/ocamloptp etc. unconditionally because
# the configuration is not available during clean so we don't
# know whether they have been configured / built or not
clean::
	rm -f $(addprefix tools/ocamlopt,p p.opt p.exe p.opt.exe)

TOOLS_NAT = $(TOOLS_TO_INSTALL_NAT)
TOOLS_BYT = $(TOOLS_TO_INSTALL_BYT) dumpobj primreq stripdebug cmpbyt

TOOLS_NAT_PROGRAMS = $(addprefix tools/,$(TOOLS_NAT))
TOOLS_BYT_PROGRAMS = $(addprefix tools/,$(TOOLS_BYT))

TOOLS_MODULES = tools/profiling

# C programs

C_PROGRAMS = $(ocamlyacc_PROGRAM)

$(foreach PROGRAM, $(C_PROGRAMS),\
  $(eval $(call PROGRAM_SYNONYM,$(PROGRAM))))

# OCaml programs that are compiled in both bytecode and native code

OCAML_PROGRAMS = ocamlc ocamlopt lex/ocamllex $(TOOLS_NAT_PROGRAMS) \
  ocamldoc/ocamldoc ocamltest/ocamltest

$(foreach PROGRAM, $(OCAML_PROGRAMS),\
  $(eval $(call OCAML_PROGRAM,$(PROGRAM))))

# OCaml programs that are compiled only in bytecode
# Note: the bytecode toplevel, ocaml, is a bytecode program but at the
# moment it's a special one, because it needs to be expunged, so we
# cannot declare it as we do for other bytecode-only programs.
# We have to use dedicated rules to build it

OCAML_BYTECODE_PROGRAMS = expunge \
  $(TOOLS_BYT_PROGRAMS) \
  $(addprefix tools/, cvt_emit make_opcodes ocamltex) \
  $(OPTIONAL_BYTECODE_TOOLS)

$(foreach PROGRAM, $(OCAML_BYTECODE_PROGRAMS),\
  $(eval $(call OCAML_BYTECODE_PROGRAM,$(PROGRAM))))

# OCaml programs that are compiled only in native code

OCAML_NATIVE_PROGRAMS = \
  ocamlnat tools/lintapidiff.opt $(OPTIONAL_NATIVE_TOOLS)

$(foreach PROGRAM, $(OCAML_NATIVE_PROGRAMS),\
  $(eval $(call OCAML_NATIVE_PROGRAM,$(PROGRAM))))

# OCaml libraries that are compiled in both bytecode and native code

# List of compilerlibs

COMPILERLIBS = $(addprefix compilerlibs/, \
  ocamlbytecomp \
  ocamlcommon \
  ocamlmiddleend \
  ocamloptcomp \
  ocamltoplevel)

# Since the compiler libraries are necessarily compiled with boot/ocamlc,
# make sure they *always are*, even when rebuilding a program compiled
# with ./ocamlc (e.g. ocamltex)

$(COMPILERLIBS:=.cma): \
  CAMLC = $(BOOT_OCAMLC) $(BOOT_STDLIBFLAGS) -use-prims runtime/primitives

# The following dependency ensures that the two versions of the
# configuration module (the one for the bootstrap compiler and the
# one for the compiler to be installed) are compiled. This is to make
# sure these two versions remain in sync with each other

compilerlibs/ocamlcommon.cma: $(ALL_CONFIG_CMO)

OCAML_LIBRARIES = $(COMPILERLIBS) $(OPTIONAL_LIBRARIES)

$(foreach LIBRARY, $(OCAML_LIBRARIES),\
  $(eval $(call OCAML_LIBRARY,$(LIBRARY))))

# OCaml libraries that are compiled only in bytecode

OCAML_BYTECODE_LIBRARIES =

$(foreach LIBRARY, $(OCAML_BYTECODE_LIBRARIES),\
  $(eval $(call OCAML_BYTECODE_LIBRARY,$(LIBRARY))))

# OCaml libraries that are compiled only in native code

OCAML_NATIVE_LIBRARIES =

$(foreach LIBRARY, $(OCAML_NATIVE_LIBRARIES),\
  $(eval $(call OCAML_NATIVE_LIBRARY,$(LIBRARY))))

USE_RUNTIME_PRIMS = -use-prims ../runtime/primitives
USE_STDLIB = -nostdlib -I ../stdlib

FLEXDLL_OBJECTS = \
  flexdll_$(FLEXDLL_CHAIN).$(O) flexdll_initer_$(FLEXDLL_CHAIN).$(O)
FLEXLINK_BUILD_ENV = \
  MSVCC_ROOT= \
  MSVC_DETECT=0 OCAML_CONFIG_FILE=../Makefile.config \
  CHAINS=$(FLEXDLL_CHAIN) ROOTDIR=..
FLEXDLL_SOURCES = \
  $(addprefix $(FLEXDLL_SOURCE_DIR)/, flexdll.c flexdll_initer.c flexdll.h) \
  $(wildcard $(FLEXDLL_SOURCE_DIR)/*.ml*)

$(BYTE_BINDIR) $(OPT_BINDIR):
	$(MKDIR) $@

flexlink.byte$(EXE): $(FLEXDLL_SOURCES)
	rm -f $(FLEXDLL_SOURCE_DIR)/flexlink.exe
	$(MAKE) -C $(FLEXDLL_SOURCE_DIR) $(FLEXLINK_BUILD_ENV) \
	  OCAMLRUN='$$(ROOTDIR)/boot/ocamlrun$(EXE)' NATDYNLINK=false \
	  OCAMLOPT='$(value BOOT_OCAMLC) $(USE_RUNTIME_PRIMS) $(USE_STDLIB)' \
	  flexlink.exe support
	cp $(FLEXDLL_SOURCE_DIR)/flexlink.exe $@

partialclean::
	rm -f flexlink.byte flexlink.byte.exe

$(BYTE_BINDIR)/flexlink$(EXE): \
    boot/ocamlrun$(EXE) flexlink.byte$(EXE) | $(BYTE_BINDIR)
	rm -f $@
# Start with a copy to ensure that the result is always executable
	cp boot/ocamlrun$(EXE) $@
	cat flexlink.byte$(EXE) >> $@
	cp $(addprefix $(FLEXDLL_SOURCE_DIR)/, $(FLEXDLL_OBJECTS)) $(BYTE_BINDIR)

partialclean::
	rm -f $(BYTE_BINDIR)/flexlink $(BYTE_BINDIR)/flexlink.exe

ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
# The recipe for runtime/ocamlruns$(EXE) also produces runtime/primitives
boot/ocamlrun$(EXE): runtime/ocamlruns$(EXE)

$(foreach runtime, ocamlrun ocamlrund ocamlruni, \
  $(eval runtime/$(runtime)$(EXE): | $(BYTE_BINDIR)/flexlink$(EXE)))

tools/checkstack$(EXE): | $(BYTE_BINDIR)/flexlink$(EXE)
else
boot/ocamlrun$(EXE): runtime/ocamlrun$(EXE) runtime/primitives
endif

# $< refers to runtime/ocamlruns when bootstrapping flexlink and
# runtime/ocamlrun otherwise (see above).
boot/ocamlrun$(EXE):
	cp $< $@

# Start up the system from the distribution compiler
.PHONY: coldstart
coldstart: boot/ocamlrun$(EXE) runtime/libcamlrun.$(A)
	$(MAKE) -C stdlib OCAMLRUN='$$(ROOTDIR)/$<' USE_BOOT_OCAMLC=true all
	rm -f $(addprefix boot/, libcamlrun.$(A) $(LIBFILES))
	cp $(addprefix stdlib/, $(LIBFILES)) boot
	cd boot; $(LN) ../runtime/libcamlrun.$(A) .

# Recompile the core system using the bootstrap compiler
.PHONY: coreall
coreall: runtime
	$(MAKE) ocamlc
	$(MAKE) ocamllex ocamltools library

# Build the core system: the minimum needed to make depend and bootstrap
.PHONY: core
core: coldstart
	$(MAKE) coreall

# Check if fixpoint reached

# We use tools/cmpbyt because it has better error reporting, but cmp could also
# be used.
CMPCMD ?= $(OCAMLRUN) tools/cmpbyt$(EXE)

.PHONY: compare
compare:
# The core system has to be rebuilt after bootstrap anyway, so strip ocamlc
# and ocamllex, which means the artefacts should be identical.
	mv ocamlc$(EXE) ocamlc.tmp
	$(OCAMLRUN) tools/stripdebug -all ocamlc.tmp ocamlc$(EXE)
	mv lex/ocamllex$(EXE) ocamllex.tmp
	$(OCAMLRUN) tools/stripdebug -all ocamllex.tmp lex/ocamllex$(EXE)
	rm -f ocamllex.tmp ocamlc.tmp
	@if $(CMPCMD) boot/ocamlc ocamlc$(EXE) \
         && $(CMPCMD) boot/ocamllex lex/ocamllex$(EXE); \
	then echo "Fixpoint reached, bootstrap succeeded."; \
	else \
	  echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	  exit 1; \
	fi

# Promote a compiler

PROMOTE ?= cp

.PHONY: promote-common
promote-common:
	$(PROMOTE) ocamlc$(EXE) boot/ocamlc
	$(PROMOTE) lex/ocamllex$(EXE) boot/ocamllex
	cd stdlib; cp $(LIBFILES) ../boot

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
.PHONY: promote-cross
promote-cross: promote-common

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
.PHONY: promote
promote: PROMOTE = $(OCAMLRUN) tools/stripdebug -all
promote: promote-common
	rm -f boot/ocamlrun$(EXE)
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)

# Compile the native-code compiler
.PHONY: opt-core
opt-core: runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt

.PHONY: opt
opt: checknative
	$(MAKE) runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt
	$(MAKE) otherlibrariesopt ocamltoolsopt

# Native-code versions of the tools
.PHONY: opt.opt
opt.opt: checknative
	$(MAKE) checkstack
	$(MAKE) coreall
	$(MAKE) ocaml
	$(MAKE) opt-core
ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
	$(MAKE) flexlink.opt$(EXE)
endif
	$(MAKE) ocamlc.opt
# TODO: introduce OPTIONAL_LIBRARIES and OPTIONAL_TOOLS variables to be
# computed at configure time to keep track of which tools and libraries
# need to be built
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(OCAMLDOC_TARGET) \
	  $(OCAMLTEST_TARGET)
	$(MAKE) ocamlopt.opt
	$(MAKE) otherlibrariesopt
	$(MAKE) ocamllex.opt ocamltoolsopt ocamltoolsopt.opt \
	  $(OCAMLDOC_OPT_TARGET) \
	  $(OCAMLTEST_OPT_TARGET) othertools ocamlnat
ifeq "$(build_libraries_manpages)" "true"
	$(MAKE) manpages
endif

# Core bootstrapping cycle
.PHONY: coreboot
ifeq "$(FLAT_FLOAT_ARRAY)" "true"
coreboot:
# Promote the new compiler but keep the old runtime
# This compiler runs on boot/ocamlrun and produces bytecode for
# runtime/ocamlrun
	$(MAKE) promote-cross
# Rebuild ocamlc and ocamllex (run on runtime/ocamlrun)
# utils/config.ml will have the fixed bootstrap configuration
	$(MAKE) partialclean
	$(MAKE) IN_COREBOOT_CYCLE=true ocamlc ocamllex ocamltools
# Rebuild the library (using runtime/ocamlrun ./ocamlc)
	$(MAKE) library-cross
# Promote the new compiler and the new runtime
	$(MAKE) OCAMLRUN=runtime/ocamlrun$(EXE) promote
# Rebuild the core system
# utils/config.ml must still have the fixed bootstrap configuration
	$(MAKE) partialclean
	$(MAKE) IN_COREBOOT_CYCLE=true core
# Check if fixpoint reached
	$(MAKE) compare
else
coreboot:
	$(error Cannot bootstrap when configured with \
--disable-flat-float-array)
endif

# Recompile the system using the bootstrap compiler

.PHONY: all
all: coreall
	$(MAKE) ocaml
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(OCAMLDOC_TARGET) \
         $(OCAMLTEST_TARGET)
	$(MAKE) othertools
ifeq "$(build_libraries_manpages)" "true"
	$(MAKE) manpages
endif

# Bootstrap and rebuild the whole system.
# The compilation of ocaml will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.
.PHONY: bootstrap
bootstrap: coreboot
# utils/config.ml must be restored to config.status's configuration
# lex/ocamllex$(EXE) was stripped in order to compare it
	rm -f utils/config.ml lex/ocamllex$(EXE)
	$(MAKE) all

# Compile everything the first time

.PHONY: world
world: coldstart
	$(MAKE) all

# Compile also native code compiler and libraries, fast
.PHONY: world.opt
world.opt: checknative
	$(MAKE) coldstart
	$(MAKE) opt.opt

# FlexDLL sources missing error messages
# Different git mechanism displayed depending on whether this source tree came
# from a git clone or a source tarball.

.PHONY: flexdll flexlink flexlink.opt

ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"

.PHONY: flexdll
flexdll: flexdll/Makefile
	@echo WARNING! make flexdll is no longer required
	@echo This target will be removed in a future release.

.PHONY: flexlink
flexlink:
	@echo Bootstrapping just flexlink.exe is no longer supported
	@echo Bootstrapping FlexDLL is now enabled with
	@echo ./configure --with-flexdll
	@false

ifeq "$(wildcard ocamlopt.opt$(EXE))" ""
  FLEXLINK_OCAMLOPT=../runtime/ocamlrun$(EXE) ../ocamlopt$(EXE)
else
  FLEXLINK_OCAMLOPT=../ocamlopt.opt$(EXE)
endif

flexlink.opt$(EXE): \
    $(FLEXDLL_SOURCES) | $(BYTE_BINDIR)/flexlink$(EXE) $(OPT_BINDIR)
	rm -f $(FLEXDLL_SOURCE_DIR)/flexlink.exe
	$(MAKE) -C $(FLEXDLL_SOURCE_DIR) $(FLEXLINK_BUILD_ENV) \
	  OCAMLOPT='$(FLEXLINK_OCAMLOPT) -nostdlib -I ../stdlib' flexlink.exe
	cp $(FLEXDLL_SOURCE_DIR)/flexlink.exe $@
	rm -f $(OPT_BINDIR)/flexlink$(EXE)
	cd $(OPT_BINDIR); $(LN) $(call ROOT_FROM, $(OPT_BINDIR))/$@ flexlink$(EXE)
	cp $(addprefix $(BYTE_BINDIR)/, $(FLEXDLL_OBJECTS)) $(OPT_BINDIR)

partialclean::
	rm -f flexlink.opt$(EXE) $(OPT_BINDIR)/flexlink$(EXE)

else

flexdll flexlink flexlink.opt:
	@echo It is no longer necessary to bootstrap FlexDLL with a separate
	@echo make invocation. Simply place the sources for FlexDLL in a
	@echo sub-directory.
	@echo This can either be done by downloading a source tarball from
	@echo \  https://github.com/ocaml/flexdll/releases
	@if [ -d .git ]; then \
	  echo or by checking out the flexdll submodule with; \
	  echo \  git submodule update --init; \
	else \
	  echo or by cloning the git repository; \
	  echo \  git clone https://github.com/ocaml/flexdll.git; \
	fi
	@echo "Then pass --with-flexdll=<dir> to configure and build as normal."
	@false

endif # ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"

INSTALL_COMPLIBDIR = $(DESTDIR)$(COMPLIBDIR)
INSTALL_FLEXDLLDIR = $(INSTALL_LIBDIR)/flexdll
FLEXDLL_MANIFEST = default_$(ARCH).manifest

DOC_FILES=\
  Changes \
  README.adoc \
  README.win32.adoc \
  LICENSE

# Run all tests

.PHONY: tests
tests:
	$(MAKE) -C testsuite all

# Make clean in the test suite

.PHONY: clean
clean::
	$(MAKE) -C testsuite clean

# Build the manual latex files from the etex source files
# (see manual/README.md)
.PHONY: manual-pregen
manual-pregen: opt.opt
	cd manual; $(MAKE) clean && $(MAKE) pregen-etex

clean::
	$(MAKE) -C manual clean

# The clean target
clean:: partialclean
	rm -f configure~
	rm -f $(C_PROGRAMS) $(C_PROGRAMS:=.exe)
	rm -f $(OCAML_PROGRAMS) $(OCAML_PROGRAMS:=.exe)
	rm -f $(OCAML_PROGRAMS:=.opt) $(OCAML_PROGRAMS:=.opt.exe)
	rm -f $(OCAML_BYTECODE_PROGRAMS) $(OCAML_BYTECODE_PROGRAMS:=.exe)
	rm -f $(OCAML_NATIVE_PROGRAMS) $(OCAML_NATIVE_PROGRAMS:=.exe)

# The bytecode compiler

ocamlc_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)

ocamlc_SOURCES = driver/main.mli driver/main.ml

ocamlc$(EXE): OC_BYTECODE_LINKFLAGS += -compat-32 -g

partialclean::
	rm -f ocamlc ocamlc.exe ocamlc.opt ocamlc.opt.exe

# The native-code compiler

ocamlopt_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamloptcomp)

ocamlopt_SOURCES = driver/optmain.mli driver/optmain.ml

ocamlopt$(EXE): OC_BYTECODE_LINKFLAGS += -g

partialclean::
	rm -f ocamlopt ocamlopt.exe ocamlopt.opt ocamlopt.opt.exe

# The toplevel

# At the moment, the toplevel can't be built with the general build macros
# because its build involves calling expunge. We thus give its build
# rules explicitly until the day expunge can hopefully be removed.

ocaml_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp ocamltoplevel)

ocaml_CMA_FILES = $(ocaml_LIBRARIES:=.cma)

ocaml_SOURCES = toplevel/topstart.mli toplevel/topstart.ml

ocaml_CMO_FILES = toplevel/topstart.cmo

.INTERMEDIATE: ocaml.tmp
ocaml.tmp: OC_BYTECODE_LINKFLAGS += -I toplevel/byte -linkall -g
ocaml.tmp: $(ocaml_CMA_FILES) $(ocaml_CMO_FILES)
	$(V_LINKC)$(LINK_BYTECODE_PROGRAM) -o $@ $^

$(eval $(call PROGRAM_SYNONYM,ocaml))
ocaml$(EXE): $(expunge) ocaml.tmp
	- $(V_GEN)$(OCAMLRUN) $^ $@ $(PERVASIVES)

partialclean::
	rm -f ocaml ocaml.exe

# Use TOPFLAGS to pass additional flags to the bytecode or native toplevel
# when running make runtop or make natruntop
TOPFLAGS ?=
OC_TOPFLAGS = $(STDLIBFLAGS) -I toplevel -noinit $(TOPINCLUDES) $(TOPFLAGS)

RUN_OCAML = $(RLWRAP) $(OCAMLRUN) ./ocaml$(EXE) $(OC_TOPFLAGS)
RUN_OCAMLNAT = $(RLWRAP) ./ocamlnat$(EXE) $(OC_TOPFLAGS)

# Note: Beware that, since these rules begin with a coldstart, both
# boot/ocamlrun and runtime/ocamlrun will be the same when the toplevel
# is run.
.PHONY: runtop
runtop: coldstart
	$(MAKE) ocamlc
	$(MAKE) ocaml
	@$(RUN_OCAML)

.PHONY: runtop-with-otherlibs
runtop-with-otherlibs: coldstart
	$(MAKE) ocamlc
	$(MAKE) otherlibraries
	$(MAKE) ocaml
	@$(RUN_OCAML)

.PHONY: natruntop
natruntop:
	$(MAKE) core
	$(MAKE) opt
	$(MAKE) ocamlnat
	@$(RUN_OCAMLNAT)

# Native dynlink

otherlibs/dynlink/dynlink.cmxa: otherlibs/dynlink/native/dynlink.ml
	$(MAKE) -C otherlibs/dynlink allopt

# Cleanup the lexer

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The predefined exceptions and primitives

lambda/runtimedef.ml: lambda/generate_runtimedef.sh runtime/caml/fail.h \
    runtime/primitives
	$(V_GEN)$^ > $@

partialclean::
	rm -f lambda/runtimedef.ml

beforedepend:: lambda/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.mli: asmcomp/$(ARCH)/arch.mli
	cd asmcomp; $(LN) $(ARCH)/arch.mli .

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	cd asmcomp; $(LN) $(ARCH)/arch.ml .

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	cd asmcomp; $(LN) $(ARCH)/proc.ml .

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	cd asmcomp; $(LN) $(ARCH)/selection.ml .

asmcomp/CSE.ml: asmcomp/$(ARCH)/CSE.ml
	cd asmcomp; $(LN) $(ARCH)/CSE.ml .

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	cd asmcomp; $(LN) $(ARCH)/reload.ml .

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	cd asmcomp; $(LN) $(ARCH)/scheduling.ml .

asmcomp/stackframe.ml: asmcomp/$(ARCH)/stackframe.ml
	cd asmcomp; $(LN) $(ARCH)/stackframe.ml .

# Preprocess the code emitters
cvt_emit = tools/cvt_emit$(EXE)

beforedepend:: tools/cvt_emit.ml

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp $(cvt_emit)
	$(V_GEN)echo \# 1 \"asmcomp/$(ARCH)/emit.mlp\" > $@ && \
	$(OCAMLRUN) $(cvt_emit) < $< >> $@ \
	|| { rm -f $@; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml tools/cvt_emit.ml

beforedepend:: asmcomp/emit.ml

cvt_emit_LIBRARIES =
cvt_emit_SOURCES = tools/cvt_emit.mli tools/cvt_emit.mll

# The "expunge" utility

expunge_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)

expunge_SOURCES = toplevel/expunge.mli toplevel/expunge.ml

partialclean::
	rm -f expunge expunge.exe

# The runtime system

## Lists of source files

runtime_COMMON_C_SOURCES = \
  addrmap \
  afl \
  alloc \
  array \
  backtrace \
  bigarray \
  blake2 \
  callback \
  codefrag \
  compare \
  custom \
  debugger \
  domain \
  dynlink \
  extern \
  fiber \
  finalise \
  floats \
  gc_ctrl \
  gc_stats \
  globroots \
  hash \
  intern \
  ints \
  io \
  lexing \
  lf_skiplist \
  main \
  major_gc \
  md5 \
  memory \
  memprof \
  meta \
  minor_gc \
  misc \
  obj \
  parsing \
  platform \
  printexc \
  prng \
  roots \
  runtime_events \
  shared_heap \
  signals \
  skiplist \
  startup_aux \
  str \
  sync \
  sys \
  $(TSAN_NATIVE_RUNTIME_C_SOURCES) \
  $(UNIX_OR_WIN32) \
  weak

runtime_BYTECODE_ONLY_C_SOURCES = \
  backtrace_byt \
  fail_byt \
  fix_code \
  interp \
  startup_byt \
  zstd
runtime_BYTECODE_C_SOURCES = \
  $(runtime_COMMON_C_SOURCES:%=runtime/%.c) \
  $(runtime_BYTECODE_ONLY_C_SOURCES:%=runtime/%.c)

runtime_NATIVE_ONLY_C_SOURCES = \
  backtrace_nat \
  clambda_checks \
  dynlink_nat \
  fail_nat \
  frame_descriptors \
  startup_nat \
  signals_nat
runtime_NATIVE_C_SOURCES = \
  $(runtime_COMMON_C_SOURCES:%=runtime/%.c) \
  $(runtime_NATIVE_ONLY_C_SOURCES:%=runtime/%.c)

## Header files generated by configure
runtime_CONFIGURED_HEADERS = \
  $(addprefix runtime/caml/, exec.h m.h s.h version.h)

## Header files generated by make
runtime_BUILT_HEADERS = $(addprefix runtime/, \
  caml/opnames.h caml/jumptbl.h build_config.h)

## Targets to build and install

runtime_PROGRAMS = runtime/ocamlrun$(EXE)
runtime_BYTECODE_STATIC_LIBRARIES = $(addprefix runtime/, \
  ld.conf libcamlrun.$(A))
runtime_BYTECODE_SHARED_LIBRARIES =
runtime_NATIVE_STATIC_LIBRARIES = \
  runtime/libasmrun.$(A) runtime/libcomprmarsh.$(A)
runtime_NATIVE_SHARED_LIBRARIES =

ifeq "$(RUNTIMED)" "true"
runtime_PROGRAMS += runtime/ocamlrund$(EXE)
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlrund.$(A)
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmrund.$(A)
endif

ifeq "$(INSTRUMENTED_RUNTIME)" "true"
runtime_PROGRAMS += runtime/ocamlruni$(EXE)
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlruni.$(A)
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmruni.$(A)
endif

ifeq "$(UNIX_OR_WIN32)" "unix"
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlrun_pic.$(A)
runtime_BYTECODE_SHARED_LIBRARIES += runtime/libcamlrun_shared.$(SO)
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmrun_pic.$(A)
runtime_NATIVE_SHARED_LIBRARIES += runtime/libasmrun_shared.$(SO)
endif
endif

## List of object files for each target

libcamlrun_OBJECTS = $(runtime_BYTECODE_C_SOURCES:.c=.b.$(O))

libcamlrun_non_shared_OBJECTS = \
  $(subst $(UNIX_OR_WIN32).b.$(O),$(UNIX_OR_WIN32)_non_shared.b.$(O), \
          $(libcamlrun_OBJECTS))

libcamlrund_OBJECTS = $(runtime_BYTECODE_C_SOURCES:.c=.bd.$(O)) \
  runtime/instrtrace.bd.$(O)

libcamlruni_OBJECTS = $(runtime_BYTECODE_C_SOURCES:.c=.bi.$(O))

libcamlrunpic_OBJECTS = $(runtime_BYTECODE_C_SOURCES:.c=.bpic.$(O))

libasmrun_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.n.$(O)) $(runtime_ASM_OBJECTS)

libasmrund_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.nd.$(O)) $(runtime_ASM_OBJECTS:.$(O)=.d.$(O))

libasmruni_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.ni.$(O)) $(runtime_ASM_OBJECTS:.$(O)=.i.$(O))

libasmrunpic_OBJECTS = $(runtime_NATIVE_C_SOURCES:.c=.npic.$(O)) \
  $(runtime_ASM_OBJECTS:.$(O)=_libasmrunpic.$(O))

libcomprmarsh_OBJECTS = runtime/zstd.npic.$(O)

## General (non target-specific) assembler and compiler flags

runtime_CPPFLAGS = -DCAMLDLLIMPORT= -DIN_CAML_RUNTIME
ocamlrund_CPPFLAGS = -DDEBUG
ocamlruni_CPPFLAGS = -DCAML_INSTR

## Runtime targets

.PHONY: runtime-all
runtime-all: \
  $(runtime_BYTECODE_STATIC_LIBRARIES) $(runtime_BYTECODE_SHARED_LIBRARIES) \
  $(runtime_PROGRAMS) $(SAK)

.PHONY: runtime-allopt
ifeq "$(NATIVE_COMPILER)" "true"
runtime-allopt: \
  $(runtime_NATIVE_STATIC_LIBRARIES) $(runtime_NATIVE_SHARED_LIBRARIES)
else
runtime-allopt:
	$(error The build has been configured with --disable-native-compiler)
endif

## Generated non-object files

runtime/ld.conf: $(ROOTDIR)/Makefile.config
	$(V_GEN)echo "$(STUBLIBDIR)" > $@ && \
	echo "$(LIBDIR)" >> $@

runtime/primitives: runtime/gen_primitives.sh $(runtime_BYTECODE_C_SOURCES)
	$(V_GEN)runtime/gen_primitives.sh $@ $(runtime_BYTECODE_C_SOURCES)

runtime/prims.c: runtime/gen_primsc.sh runtime/primitives
	$(V_GEN)runtime/gen_primsc.sh \
                    runtime/primitives $(runtime_BYTECODE_C_SOURCES) \
                    > $@

runtime/caml/opnames.h : runtime/caml/instruct.h
	$(V_GEN)tr -d '\r' < $< | \
	sed -e '/\/\*/d' \
	    -e '/^#/d' \
	    -e 's/enum /static char * names_of_/' \
	    -e 's/{$$/[] = {/' \
	    -e 's/\([[:upper:]][[:upper:]_0-9]*\)/"\1"/g' > $@

# runtime/caml/jumptbl.h is required only if you have GCC 2.0 or later
runtime/caml/jumptbl.h : runtime/caml/instruct.h
	$(V_GEN)tr -d '\r' < $< | \
	sed -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' \
	       -e '/^}/q' > $@

# These are provided as a temporary shim to allow cross-compilation systems
# to supply a host C compiler and different flags and a linking macro.
SAK_CC ?= $(CC)
SAK_CFLAGS ?= $(OC_CFLAGS) $(CFLAGS) $(OC_CPPFLAGS) $(CPPFLAGS)
SAK_LINK ?= $(MKEXE_VIA_CC)

$(SAK): runtime/sak.$(O)
	$(V_MKEXE)$(call SAK_LINK,$@,$^)

runtime/sak.$(O): runtime/sak.c runtime/caml/misc.h runtime/caml/config.h
	$(V_CC)$(SAK_CC) -c $(SAK_CFLAGS) $(OUTPUTOBJ)$@ $<

C_LITERAL = $(shell $(SAK) encode-C-literal '$(1)')

runtime/build_config.h: $(ROOTDIR)/Makefile.config $(SAK)
	$(V_GEN)echo '/* This file is generated from $(ROOTDIR)/Makefile.config */' > $@ && \
	echo '#define OCAML_STDLIB_DIR $(call C_LITERAL,$(LIBDIR))' >> $@ && \
	echo '#define HOST "$(HOST)"' >> $@

## Runtime libraries and programs

runtime/ocamlrun$(EXE): runtime/prims.$(O) runtime/libcamlrun.$(A)
	$(V_MKEXE)$(MKEXE) -o $@ $^ $(BYTECCLIBS)

runtime/ocamlruns$(EXE): runtime/prims.$(O) runtime/libcamlrun_non_shared.$(A)
	$(V_MKEXE)$(call MKEXE_VIA_CC,$@,$^ $(BYTECCLIBS))

runtime/libcamlrun.$(A): $(libcamlrun_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libcamlrun_non_shared.$(A): $(libcamlrun_non_shared_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/ocamlrund$(EXE): runtime/prims.$(O) runtime/libcamlrund.$(A)
	$(V_MKEXE)$(MKEXE) $(MKEXEDEBUGFLAG) -o $@ $^ $(BYTECCLIBS)

runtime/libcamlrund.$(A): $(libcamlrund_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/ocamlruni$(EXE): runtime/prims.$(O) runtime/libcamlruni.$(A)
	$(V_MKEXE)$(MKEXE) -o $@ $^ $(INSTRUMENTED_RUNTIME_LIBS) $(BYTECCLIBS)

runtime/libcamlruni.$(A): $(libcamlruni_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libcamlrun_pic.$(A): $(libcamlrunpic_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libcamlrun_shared.$(SO): $(libcamlrunpic_OBJECTS)
	$(V_MKDLL)$(MKDLL) -o $@ $^ $(BYTECCLIBS)

runtime/libasmrun.$(A): $(libasmrun_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmrund.$(A): $(libasmrund_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmruni.$(A): $(libasmruni_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmrun_pic.$(A): $(libasmrunpic_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmrun_shared.$(SO): $(libasmrunpic_OBJECTS)
	$(V_MKDLL)$(MKDLL) -o $@ $^ $(NATIVECCLIBS)

runtime/libcomprmarsh.$(A): $(libcomprmarsh_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

## Runtime target-specific preprocessor and compiler flags

runtime/%.$(O): OC_CPPFLAGS += $(runtime_CPPFLAGS)
$(DEPDIR)/runtime/%.$(D): OC_CPPFLAGS += $(runtime_CPPFLAGS)

runtime/%.bd.$(O): OC_CPPFLAGS += $(ocamlrund_CPPFLAGS)
$(DEPDIR)/runtime/%.bd.$(D): OC_CPPFLAGS += $(ocamlrund_CPPFLAGS)

runtime/%.bi.$(O): OC_CPPFLAGS += $(ocamlruni_CPPFLAGS)
$(DEPDIR)/runtime/%.bi.$(D): OC_CPPFLAGS += $(ocamlruni_CPPFLAGS)

runtime/%.bpic.$(O): OC_CFLAGS += $(SHAREDLIB_CFLAGS)

runtime/%.n.$(O): OC_CFLAGS += $(OC_NATIVE_CFLAGS)
runtime/%.n.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS)
$(DEPDIR)/runtime/%.n.$(D): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS)

runtime/%.nd.$(O): OC_CFLAGS += $(OC_NATIVE_CFLAGS)
runtime/%.nd.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(ocamlrund_CPPFLAGS)
$(DEPDIR)/runtime/%.nd.$(D): \
  OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(ocamlrund_CPPFLAGS)

runtime/%.ni.$(O): OC_CFLAGS += $(OC_NATIVE_CFLAGS)
runtime/%.ni.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(ocamlruni_CPPFLAGS)
$(DEPDIR)/runtime/%.ni.$(D): \
  OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(ocamlruni_CPPFLAGS)

runtime/%.npic.$(O): OC_CFLAGS += $(OC_NATIVE_CFLAGS) $(SHAREDLIB_CFLAGS)
runtime/%.npic.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS)
$(DEPDIR)/runtime/%.npic.$(D): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS)

## Compilation of runtime C files

# The COMPILE_C_FILE macro below receives as argument the pattern
# that corresponds to the name of the generated object file
# (without the extension, which is added by the macro)
define COMPILE_C_FILE
ifeq "$(COMPUTE_DEPS)" "true"
ifneq "$(1)" "%"
# -MG would ensure that the dependencies are generated even if the files listed
# in $$(runtime_BUILT_HEADERS) haven't been assembled yet. However,
# this goes subtly wrong if the user has the headers installed,
# as gcc will pick up a dependency on those instead and the local
# ones will not be generated. For this reason, we don't use -MG and
# instead include $(runtime_BUILT_HEADERS) in the order only dependencies
# to ensure that they exist before dependencies are computed.
$(DEPDIR)/$(1).$(D): runtime/%.c | $(DEPDIR)/runtime $(runtime_BUILT_HEADERS)
	$$(V_CCDEPS)$$(DEP_CC) $$(OC_CPPFLAGS) $$(CPPFLAGS) $$< -MT \
	  'runtime/$$*$(subst runtime/%,,$(1)).$(O)' -MF $$@
endif # ifneq "$(1)" "%"
$(1).$(O): $(2).c
else
$(1).$(O): $(2).c \
  $(runtime_CONFIGURED_HEADERS) $(runtime_BUILT_HEADERS) \
  $(RUNTIME_HEADERS)
endif # ifeq "$(COMPUTE_DEPS)" "true"
	$$(V_CC)$$(CC) -c $$(OC_CFLAGS) $$(CFLAGS) $$(OC_CPPFLAGS) $$(CPPFLAGS) \
	  $$(OUTPUTOBJ)$$@ $$<
endef

$(DEPDIR)/runtime:
	$(MKDIR) $@

runtime_OBJECT_TYPES = % %.b %.bd %.bi %.bpic
ifeq "$(NATIVE_COMPILER)" "true"
runtime_OBJECT_TYPES += %.n %.nd %.ni %.np %.npic
endif

$(foreach runtime_OBJECT_TYPE, $(runtime_OBJECT_TYPES), \
  $(eval $(call COMPILE_C_FILE,runtime/$(runtime_OBJECT_TYPE),runtime/%)))

runtime/$(UNIX_OR_WIN32)_non_shared.%.$(O): \
  OC_CPPFLAGS += -DBUILDING_LIBCAMLRUNS

$(eval $(call COMPILE_C_FILE,runtime/$(UNIX_OR_WIN32)_non_shared.%, \
  runtime/$(UNIX_OR_WIN32)))

$(foreach runtime_OBJECT_TYPE,$(subst %,,$(runtime_OBJECT_TYPES)), \
  $(eval \
    runtime/dynlink$(runtime_OBJECT_TYPE).$(O): $(ROOTDIR)/Makefile.config))

## Compilation of runtime assembly files

ASPP_ERROR = \
  { echo "If your assembler produced syntax errors, it is probably";\
          echo "unhappy with the preprocessor. Check your assembler, or";\
          echo "try producing $*.o by hand.";\
          exit 2; }
runtime/%.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%.d.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) $(ocamlrund_CPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%.i.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) $(ocamlruni_CPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%_libasmrunpic.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) $(SHAREDLIB_CFLAGS) -o $@ $<

runtime/domain_state.inc: runtime/caml/domain_state.tbl
	$(V_GEN)$(CPP) $< > $@

runtime/amd64nt.obj: runtime/amd64nt.asm runtime/domain_state.inc
	$(V_ASM)$(ASM)$@ $<

runtime/amd64nt.d.obj: runtime/amd64nt.asm runtime/domain_state.inc
	$(V_ASM)$(ASM)$@ $(ocamlrund_CPPFLAGS) $<

runtime/amd64nt.i.obj: runtime/amd64nt.asm runtime/domain_state.inc
	$(V_ASM)$(ASM)$@ $(ocamlruni_CPPFLAGS) $<

runtime/%_libasmrunpic.obj: runtime/%.asm
	$(V_ASM)$(ASM)$@ $<

## Runtime dependencies

runtime_DEP_FILES := $(addsuffix .b, \
  $(basename $(runtime_BYTECODE_C_SOURCES) runtime/instrtrace))
ifeq "$(NATIVE_COMPILER)" "true"
runtime_DEP_FILES += $(addsuffix .n, $(basename $(runtime_NATIVE_C_SOURCES)))
endif
runtime_DEP_FILES += $(addsuffix d, $(runtime_DEP_FILES)) \
             $(addsuffix i, $(runtime_DEP_FILES)) \
             $(addsuffix pic, $(runtime_DEP_FILES))
runtime_DEP_FILES := $(addsuffix .$(D), $(runtime_DEP_FILES))

ifeq "$(COMPUTE_DEPS)" "true"
include $(addprefix $(DEPDIR)/, $(runtime_DEP_FILES))
endif

.PHONY: runtime
runtime: stdlib/libcamlrun.$(A)

.PHONY: makeruntime
makeruntime: runtime-all
stdlib/libcamlrun.$(A): runtime-all
	cd stdlib; $(LN) ../runtime/libcamlrun.$(A) .
clean::
	rm -f $(addprefix runtime/, *.o *.obj *.a *.lib *.so *.dll ld.conf)
	rm -f $(addprefix runtime/, ocamlrun ocamlrund ocamlruni ocamlruns sak)
	rm -f $(addprefix runtime/, \
	  ocamlrun.exe ocamlrund.exe ocamlruni.exe ocamlruns.exe sak.exe)
	rm -f runtime/primitives runtime/primitives*.new runtime/prims.c \
	  $(runtime_BUILT_HEADERS)
	rm -f runtime/domain_state.inc
	rm -rf $(DEPDIR)
	rm -f stdlib/libcamlrun.a stdlib/libcamlrun.lib

.PHONY: runtimeopt
runtimeopt: stdlib/libasmrun.$(A)

.PHONY: makeruntimeopt
makeruntimeopt: runtime-allopt
stdlib/libasmrun.$(A): runtime-allopt
	cd stdlib; $(LN) ../runtime/libasmrun.$(A) .
stdlib/libcomprmarsh.$(A): runtime/libcomprmarsh.$(A)
	cd stdlib; $(LN) ../runtime/libcomprmarsh.$(A) .

clean::
	rm -f stdlib/libasmrun.a stdlib/libasmrun.lib
	rm -f stdlib/libcomprmarsh.a stdlib/libcomprmarsh.lib

# Dependencies

subdirs = stdlib $(addprefix otherlibs/, $(ALL_OTHERLIBS))

.PHONY: alldepend
alldepend: depend
	for dir in $(subdirs); do \
	  $(MAKE) -C $$dir depend || exit; \
	done

# The standard library

.PHONY: library
library: ocamlc
	$(MAKE) -C stdlib all

.PHONY: library-cross
library-cross:
	$(MAKE) -C stdlib OCAMLRUN=../runtime/ocamlrun$(EXE) all

.PHONY: libraryopt
libraryopt:
	$(MAKE) -C stdlib allopt

partialclean::
	$(MAKE) -C stdlib clean

# The lexer generator

ocamllex_LIBRARIES =

ocamllex_SOURCES = $(addprefix lex/,\
  cset.mli cset.ml \
  syntax.mli syntax.ml \
  parser.mly \
  lexer.mli lexer.mll \
  table.mli table.ml \
  lexgen.mli lexgen.ml \
  compact.mli compact.ml \
  common.mli common.ml \
  output.mli output.ml \
  outputbis.mli outputbis.ml \
  main.mli main.ml)

.PHONY: lex-all
lex-all: lex/ocamllex

.PHONY: lex-allopt
lex-allopt: lex/ocamllex.opt

.PHONY: ocamllex
ocamllex: ocamlyacc
	$(MAKE) lex-all

.PHONY: ocamllex.opt
ocamllex.opt: ocamlopt
	$(MAKE) lex-allopt

lex/ocamllex$(EXE): OC_BYTECODE_LINKFLAGS += -compat-32

partialclean::
	rm -f lex/*.cm* lex/*.o lex/*.obj \
        $(ocamllex_PROGRAMS) $(ocamllex_PROGRAMS:=.exe) \
        lex/parser.ml lex/parser.mli lex/parser.output \
        lex/lexer.ml

beforedepend:: lex/parser.ml lex/parser.mli lex/lexer.ml

# The ocamlyacc parser generator

ocamlyacc_OTHER_MODULES = $(addprefix yacc/,\
  closure error lalr lr0 main mkpar output reader skeleton symtab \
  verbose warshall)

ocamlyacc_MODULES = $(ocamlyacc_WSTR_MODULE) $(ocamlyacc_OTHER_MODULES)

ocamlyacc_OBJECTS = $(ocamlyacc_MODULES:=.$(O))

# Do not compile assertions in ocamlyacc
ocamlyacc_CPPFLAGS = -DNDEBUG

.PHONY: ocamlyacc
ocamlyacc: $(ocamlyacc_PROGRAM)$(EXE)

$(ocamlyacc_PROGRAM)$(EXE): $(ocamlyacc_OBJECTS)
	$(V_MKEXE)$(MKEXE) -o $@ $^

clean::
	rm -f $(ocamlyacc_MODULES:=.o) $(ocamlyacc_MODULES:=.obj)

$(ocamlyacc_OTHER_MODULES:=.$(O)): yacc/defs.h

$(ocamlyacc_OTHER_MODULES:=.$(O)): OC_CPPFLAGS += $(ocamlyacc_CPPFLAGS)

# The Menhir-generated parser

# In order to avoid a build-time dependency on Menhir,
# we store the result of the parser generator (which
# are OCaml source files) and Menhir's runtime libraries
# (that the parser files rely on) in boot/.

# The rules below do not depend on Menhir being available,
# they just build the parser from boot/.

# See Makefile.menhir for the rules to rebuild the parser and update
# boot/, which require Menhir. The targets in Makefile.menhir
# (also included here for convenience) must be used after any
# modification of parser.mly.
include Makefile.menhir

# To avoid module-name conflicts with compiler-lib users that link
# with their code with their own MenhirLib module (possibly with
# a different Menhir version), we rename MenhirLib into
# CamlinternalMenhirlib -- and replace the module occurrences in the
# generated parser.ml.

parsing/camlinternalMenhirLib.ml: boot/menhir/menhirLib.ml
	$(V_GEN)cp $< $@
parsing/camlinternalMenhirLib.mli: boot/menhir/menhirLib.mli
	$(V_GEN)echo '[@@@ocaml.warning "-67"]' > $@ && \
	cat $< >> $@

# Copy parsing/parser.ml from boot/

PARSER_DEPS = boot/menhir/parser.ml parsing/parser.mly

ifeq "$(OCAML_DEVELOPMENT_VERSION)" "true"
PARSER_DEPS += tools/check-parser-uptodate-or-warn.sh
endif

parsing/parser.ml: $(PARSER_DEPS)
ifeq "$(OCAML_DEVELOPMENT_VERSION)" "true"
	@-tools/check-parser-uptodate-or-warn.sh
endif
	$(V_GEN)sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@
parsing/parser.mli: boot/menhir/parser.mli
	$(V_GEN)sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@

beforedepend:: parsing/camlinternalMenhirLib.ml \
  parsing/camlinternalMenhirLib.mli \
  parsing/parser.ml parsing/parser.mli

partialclean:: partialclean-menhir


# OCamldoc

# First define the odoc_info library used to build OCamldoc

odoc_info_SOURCES = $(addprefix ocamldoc/,\
  odoc_config.mli odoc_config.ml \
  odoc_messages.mli odoc_messages.ml \
  odoc_global.mli odoc_global.ml \
  odoc_types.mli odoc_types.ml \
  odoc_misc.mli odoc_misc.ml \
  odoc_text_parser.mly \
  odoc_text_lexer.mli odoc_text_lexer.mll \
  odoc_text.mli odoc_text.ml \
  odoc_name.mli odoc_name.ml \
  odoc_parameter.mli odoc_parameter.ml \
  odoc_value.mli odoc_value.ml \
  odoc_type.mli odoc_type.ml \
  odoc_extension.mli odoc_extension.ml \
  odoc_exception.mli odoc_exception.ml \
  odoc_class.mli odoc_class.ml \
  odoc_module.mli odoc_module.ml \
  odoc_print.mli odoc_print.ml \
  odoc_str.mli odoc_str.ml \
  odoc_comments_global.mli odoc_comments_global.ml \
  odoc_parser.mly \
  odoc_lexer.mli odoc_lexer.mll \
  odoc_see_lexer.mli odoc_see_lexer.mll \
  odoc_env.mli odoc_env.ml \
  odoc_merge.mli odoc_merge.ml \
  odoc_sig.mli odoc_sig.ml \
  odoc_ast.mli odoc_ast.ml \
  odoc_search.mli odoc_search.ml \
  odoc_scan.mli odoc_scan.ml \
  odoc_cross.mli odoc_cross.ml \
  odoc_comments.mli odoc_comments.ml \
  odoc_dep.mli odoc_dep.ml \
  odoc_analyse.mli odoc_analyse.ml \
  odoc_info.mli odoc_info.ml)

ocamldoc_LIBRARIES = \
  compilerlibs/ocamlcommon \
  $(addprefix otherlibs/,\
    unix/unix \
    str/str \
    dynlink/dynlink) \
  ocamldoc/odoc_info

ocamldoc_SOURCES = $(addprefix ocamldoc/,\
  odoc_dag2html.mli odoc_dag2html.ml \
  odoc_to_text.mli odoc_to_text.ml \
  odoc_ocamlhtml.mli odoc_ocamlhtml.mll \
  odoc_html.mli odoc_html.ml \
  odoc_man.mli odoc_man.ml \
  odoc_latex_style.mli odoc_latex_style.ml \
  odoc_latex.mli odoc_latex.ml \
  odoc_texi.mli odoc_texi.ml \
  odoc_dot.mli odoc_dot.ml \
  odoc_gen.mli odoc_gen.ml \
  odoc_args.mli odoc_args.ml \
  odoc.mli odoc.ml)

# OCamldoc files to install (a subset of what is built)

OCAMLDOC_LIBMLIS = $(addprefix ocamldoc/,$(addsuffix .mli,\
  odoc_dep odoc_dot odoc_extension odoc_html odoc_info odoc_latex \
  odoc_latex_style odoc_man odoc_messages odoc_ocamlhtml odoc_parameter \
  odoc_texi odoc_text_lexer odoc_to_text odoc_type odoc_value))
OCAMLDOC_LIBCMIS=$(OCAMLDOC_LIBMLIS:.mli=.cmi)
OCAMLDOC_LIBCMTS=$(OCAMLDOC_LIBMLIS:.mli=.cmt) $(OCAMLDOC_LIBMLIS:.mli=.cmti)

ocamldoc/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)

ocamldoc/%: CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)

.PHONY: ocamldoc
ocamldoc: ocamldoc/ocamldoc$(EXE) ocamldoc/odoc_test.cmo

ocamldoc/ocamldoc$(EXE): ocamlc ocamlyacc ocamllex

.PHONY: ocamldoc.opt
ocamldoc.opt: ocamldoc/ocamldoc.opt$(EXE)

ocamldoc/ocamldoc.opt$(EXE): ocamlopt ocamlyacc ocamllex

# OCamltest

ifeq "$(build_ocamltest)" "true"

# Libraries ocamltest depends on

ocamltest_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp) \
  $(unix_library)

# List of source files from which ocamltest is compiled
# (all the different sorts of files are derived from this)

# ocamltest has two components: its core and the OCaml "plugin"
# which is actually built into the tool but clearly separated from its core

ocamltest_CORE = \
  run_$(UNIX_OR_WIN32).c run_stubs.c \
  ocamltest_config.ml.in ocamltest_config.mli \
  ocamltest_unix.mli ocamltest_unix.ml \
  ocamltest_stdlib.mli ocamltest_stdlib.ml \
  run_command.mli run_command.ml \
  filecompare.mli filecompare.ml \
  variables.mli variables.ml \
  environments.mli environments.ml \
  result.mli result.ml \
  actions.mli actions.ml \
  tests.mli tests.ml \
  strace.mli strace.ml \
  tsl_ast.mli tsl_ast.ml \
  tsl_parser.mly \
  tsl_lexer.mli tsl_lexer.mll \
  modifier_parser.mli modifier_parser.ml \
  tsl_semantics.mli tsl_semantics.ml \
  builtin_variables.mli builtin_variables.ml \
  actions_helpers.mli actions_helpers.ml \
  builtin_actions.mli builtin_actions.ml \
  translate.mli translate.ml

ocamltest_ocaml_PLUGIN = \
  ocaml_backends.mli ocaml_backends.ml \
  ocaml_filetypes.mli ocaml_filetypes.ml \
  ocaml_variables.mli ocaml_variables.ml \
  ocaml_modifiers.mli ocaml_modifiers.ml \
  ocaml_directories.mli ocaml_directories.ml \
  ocaml_files.mli ocaml_files.ml \
  ocaml_flags.mli ocaml_flags.ml \
  ocaml_commands.mli ocaml_commands.ml \
  ocaml_tools.mli ocaml_tools.ml \
  ocaml_compilers.mli ocaml_compilers.ml \
  ocaml_toplevels.mli ocaml_toplevels.ml \
  ocaml_actions.mli ocaml_actions.ml \
  ocaml_tests.mli ocaml_tests.ml

ocamltest_SOURCES = $(addprefix ocamltest/, \
  $(ocamltest_CORE) $(ocamltest_ocaml_PLUGIN) \
  options.mli options.ml \
  main.mli main.ml)

$(eval $(call COMPILE_C_FILE,ocamltest/%.b,ocamltest/%))
$(eval $(call COMPILE_C_FILE,ocamltest/%.n,ocamltest/%))

ifeq "$(COMPUTE_DEPS)" "true"
include $(addprefix $(DEPDIR)/, $(ocamltest_C_FILES:.c=.$(D)))
endif

ocamltest_DEP_FILES = $(addprefix $(DEPDIR)/, $(ocamltest_C_FILES:.c=.$(D)))

$(ocamltest_DEP_FILES): | $(DEPDIR)/ocamltest

$(DEPDIR)/ocamltest:
	$(MKDIR) $@

$(ocamltest_DEP_FILES): $(DEPDIR)/ocamltest/%.$(D): ocamltest/%.c
	$(V_CCDEPS)$(DEP_CC) $(OC_CPPFLAGS) $(CPPFLAGS) $< -MT '$*.$(O)' -MF $@

ocamltest/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)

ocamltest/%: CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)

ocamltest: ocamltest/ocamltest$(EXE) \
  testsuite/lib/lib.cmo testsuite/lib/testing.cma testsuite/tools/expect$(EXE)

testsuite/lib/%: VPATH += testsuite/lib

testing_SOURCES = testsuite/lib/testing.mli testsuite/lib/testing.ml
testing_LIBRARIES =

$(addprefix testsuite/lib/testing., cma cmxa): \
  OC_COMMON_LINKFLAGS += -linkall

testsuite/tools/%: VPATH += testsuite/tools

expect_SOURCES = $(addprefix testsuite/tools/,expect.mli expect.ml)
expect_LIBRARIES = $(addprefix compilerlibs/,\
  ocamlcommon ocamlbytecomp ocamltoplevel)

testsuite/tools/expect$(EXE): OC_BYTECODE_LINKFLAGS += -linkall

codegen_SOURCES = $(addprefix testsuite/tools/,\
  parsecmmaux.mli parsecmmaux.ml \
  parsecmm.mly \
  lexcmm.mli lexcmm.mll \
  codegen_main.mli codegen_main.ml)
codegen_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamloptcomp)

# The asmgen tests are not ported to MSVC64 yet, so make sure
# to compile the arch specific module they require only if necessary
ifeq "$(CCOMPTYPE)-$(ARCH)" "msvc-amd64"
asmgen_OBJECT =
else
asmgen_MODULE = testsuite/tools/asmgen_$(ARCH)
asmgen_SOURCE = $(asmgen_MODULE).S
asmgen_OBJECT = $(asmgen_MODULE).$(O)
$(asmgen_OBJECT): $(asmgen_SOURCE)
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) -o $@ $< || $(ASPP_ERROR)
endif

ocamltest/ocamltest$(EXE): OC_BYTECODE_LINKFLAGS += -custom

ocamltest/ocamltest$(EXE): ocamlc ocamlyacc ocamllex

ocamltest.opt: ocamltest/ocamltest.opt$(EXE) \
  testsuite/lib/testing.cmxa $(asmgen_OBJECT) testsuite/tools/codegen$(EXE)

ocamltest/ocamltest.opt$(EXE): ocamlopt ocamlyacc ocamllex

# ocamltest does _not_ want to have access to the Unix interface by default,
# to ensure functions and types are only used via Ocamltest_stdlib.Unix
# (see #9797)
ocamltest/%: \
  VPATH := $(filter-out $(unix_directory), $(VPATH))

# Ocamltest_unix and the linking of the executable itself should include the
# Unix library, if it's being built.
ocamltest/ocamltest_unix.% \
ocamltest/ocamltest$(EXE) ocamltest/ocamltest.opt$(EXE): \
  VPATH += $(unix_directory)

# For flambda mode, it is necessary for Ocamltest_unix to be compiled with
# -opaque to prevent errors compiling the other modules of ocamltest.
ocamltest/ocamltest_unix.%: \
  OC_COMMON_COMPFLAGS += -opaque
else # ifeq "$(build_ocamltest)" "true"
ocamltest_TARGETS = ocamltest ocamltest.opt
.PHONY: $(ocamltest_TARGETS)
$(ocamltest_TARGETS):
	@echo ocamltest is disabled
	@echo To build it, run configure again with --enable-ocamltest
	@false
endif # ifeq "$(build_ocamltest)" "true"

partialclean::
	rm -f ocamltest/ocamltest ocamltest/ocamltest.exe
	rm -f ocamltest/ocamltest.opt ocamltest/ocamltest.opt.exe
	rm -f $(addprefix ocamltest/,*.o *.obj *.cm*)
	rm -f $(patsubst %.mll,%.ml, $(wildcard ocamltest/*.mll))
	rm -f $(patsubst %.mly,%.ml, $(wildcard ocamltest/*.mly))
	rm -f $(patsubst %.mly,%.mli, $(wildcard ocamltest/*.mly))
	rm -f $(patsubst %.mly,%.output, $(wildcard ocamltest/*.mly))
	rm -f ocamltest/ocamltest.html
	rm -f $(addprefix testsuite/lib/*.,cm* o obj a lib)
	rm -f $(addprefix testsuite/tools/*.,cm* o obj a lib)
	rm -f testsuite/tools/codegen testsuite/tools/codegen.exe
	rm -f testsuite/tools/expect testsuite/tools/expect.exe
	rm -f testsuite/tools/lexcmm.ml
	rm -f $(addprefix testsuite/tools/parsecmm., ml mli output)

# Documentation

.PHONY: html_doc
html_doc: ocamldoc
	$(MAKE) -C api_docgen html

.PHONY: manpages
manpages:
	$(MAKE) -C api_docgen man

partialclean::
	rm -f ocamldoc/\#*\#
	rm -f ocamldoc/*.cm[aiotx] ocamldoc/*.cmxa ocamldoc/*.cmti \
	  ocamldoc/*.a ocamldoc/*.lib ocamldoc/*.o ocamldoc/*.obj
	rm -f ocamldoc/odoc_parser.output ocamldoc/odoc_text_parser.output
	rm -f ocamldoc/odoc_lexer.ml ocamldoc/odoc_text_lexer.ml \
	  ocamldoc/odoc_see_lexer.ml ocamldoc/odoc_ocamlhtml.ml
	rm -f ocamldoc/odoc_parser.ml ocamldoc/odoc_parser.mli \
	  ocamldoc/odoc_text_parser.ml ocamldoc/odoc_text_parser.mli

partialclean::
	$(MAKE) -C api_docgen clean

# The OCamltest manual

.PHONY: ocamltest-manual
ocamltest-manual: ocamltest/ocamltest.html

ocamltest/ocamltest.html: ocamltest/ocamltest.org
	pandoc -s --toc -N -f org -t html -o $@ $<

# The extra libraries

.PHONY: otherlibraries
otherlibraries: ocamltools
	$(MAKE) -C otherlibs all

.PHONY: otherlibrariesopt
otherlibrariesopt:
	$(MAKE) -C otherlibs allopt

otherlibs/unix/unix.cmxa: otherlibrariesopt
otherlibs/dynlink/dynlink.cmxa: otherlibrariesopt
otherlibs/str/str.cmxa: otherlibrariesopt

partialclean::
	$(MAKE) -C otherlibs partialclean

clean::
	$(MAKE) -C otherlibs clean

# The replay debugger

ocamldebug_LIBRARIES = compilerlibs/ocamlcommon \
  $(addprefix otherlibs/,unix/unix dynlink/dynlink)

# The following dependencies are necessary at the moment, because the
# root Makefile does not know yet how to build the other libraries
# Once their build will happen in this root Makefile, too, it will become
# possible to get rid of these dependencies

otherlibs/unix/unix.cma: otherlibraries
otherlibs/dynlink/dynlink.cma: otherlibraries
otherlibs/str/str.cma: otherlibraries

debugger/%: VPATH += otherlibs/unix otherlibs/dynlink

ocamldebug_COMPILER_SOURCES = $(addprefix toplevel/, \
  genprintval.mli genprintval.ml \
  topprinters.mli topprinters.ml)

# The modules listed in the following variable are packed into ocamldebug.cmo

ocamldebug_DEBUGGER_SOURCES = $(addprefix debugger/,\
  int64ops.mli int64ops.ml \
  primitives.mli primitives.ml \
  unix_tools.mli unix_tools.ml \
  debugger_config.mli debugger_config.ml \
  parameters.mli parameters.ml \
  debugger_lexer.mli debugger_lexer.mll \
  input_handling.mli input_handling.ml \
  question.mli question.ml \
  debugcom.mli debugcom.ml \
  exec.mli exec.ml \
  source.mli source.ml \
  pos.mli pos.ml \
  checkpoints.mli checkpoints.ml \
  events.mli events.ml \
  program_loading.mli program_loading.ml \
  symbols.mli symbols.ml \
  breakpoints.mli breakpoints.ml \
  trap_barrier.mli trap_barrier.ml \
  history.mli history.ml \
  printval.mli printval.ml \
  show_source.mli show_source.ml \
  time_travel.mli time_travel.ml \
  program_management.mli program_management.ml \
  frames.mli frames.ml \
  eval.mli eval.ml \
  show_information.mli show_information.ml \
  loadprinter.mli loadprinter.ml \
  debugger_parser.mly \
  command_line.mli command_line.ml \
  main.mli main.ml)

ocamldebug_DEBUGGER_OBJECTS = \
  $(patsubst %.ml, %.cmo, \
    $(patsubst %.mll, %.cmo, \
      $(patsubst %.mly, %.cmo, \
        $(filter-out %.mli, $(ocamldebug_DEBUGGER_SOURCES)))))

ocamldebug_SOURCES = \
  $(ocamldebug_COMPILER_SOURCES) \
  $(addprefix debugger/, \
    ocamldebug.ml \
    ocamldebug_entry.mli ocamldebug_entry.ml)

debugger/%: OC_BYTECODE_LINKFLAGS = -linkall

debugger/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)

.PHONY: ocamldebug ocamldebugger
ocamldebug: debugger/ocamldebug$(EXE)
ocamldebugger: debugger/ocamldebug$(EXE)
# the 'ocamldebugger' target is an alias of 'ocamldebug' for
# backward-compatibility with old ./configure scripts; it can be
# removed after most contributors have re-run ./configure once, for
# example after 5.2 is branched

debugger/ocamldebug$(EXE): ocamlc ocamlyacc ocamllex

$(ocamldebug_DEBUGGER_OBJECTS): OC_COMMON_COMPFLAGS += -for-pack ocamldebug
debugger/ocamldebug.cmo: $(ocamldebug_DEBUGGER_OBJECTS)
	$(V_OCAMLC)$(CAMLC) $(OC_COMMON_COMPFLAGS) -pack -o $@ $^

debugger/ocamldebug_entry.cmo: debugger/ocamldebug.cmo

clean::
	rm -f debugger/ocamldebug debugger/ocamldebug.exe
	rm -f debugger/debugger_lexer.ml
	rm -f $(addprefix debugger/debugger_parser.,ml mli output)

beforedepend:: debugger/debugger_lexer.ml

beforedepend:: debugger/debugger_parser.ml debugger/debugger_parser.mli

# Check that the native-code compiler is supported
.PHONY: checknative
checknative:
ifeq "$(NATIVE_COMPILER)" "false"
	$(error The source tree was configured with --disable-native-compiler!)
else
ifeq "$(ARCH)" "none"
	$(error The native-code compiler is not supported on this platform)
else
	@
endif
endif

# Check that the stack limit is reasonable (Unix-only)
.PHONY: checkstack
ifeq "$(UNIX_OR_WIN32)" "unix"
checkstack: tools/checkstack$(EXE)
	$<

.INTERMEDIATE: tools/checkstack$(EXE) tools/checkstack.$(O)
tools/checkstack$(EXE): tools/checkstack.$(O)
	$(V_MKEXE)$(MKEXE) $(OUTPUTEXE)$@ $<
else
checkstack:
	@
endif

# Lint @since and @deprecated annotations

lintapidiff_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp) \
  otherlibs/str/str
lintapidiff_SOURCES = tools/lintapidiff.mli tools/lintapidiff.ml

tools/lintapidiff.opt$(EXE): VPATH += otherlibs/str

VERSIONS=$(shell git tag|grep '^[0-9]*.[0-9]*.[0-9]*$$'|grep -v '^[12].')
.PHONY: lintapidiff
lintapidiff: tools/lintapidiff.opt$(EXE)
	git ls-files -- 'otherlibs/*/*.mli' 'stdlib/*.mli' |\
	    grep -Ev internal\|obj\|stdLabels\|moreLabels |\
	    tools/lintapidiff.opt $(VERSIONS)

# Tools

TOOLS_BYTECODE_TARGETS = \
  $(TOOLS_NAT_PROGRAMS) $(TOOLS_BYT_PROGRAMS) $(TOOLS_MODULES:=.cmo)

TOOLS_NATIVE_TARGETS = $(TOOLS_MODULES:=.cmx)

TOOLS_OPT_TARGETS = $(TOOLS_NAT_PROGRAMS:=.opt)

.PHONY: ocamltools
ocamltools: ocamlc ocamllex
	$(MAKE) tools-all

.PHONY: tools-all
tools-all: $(TOOLS_BYTECODE_TARGETS)

.PHONY: tools-allopt
tools-allopt: $(TOOLS_NATIVE_TARGETS)

.PHONY: tools-allopt.opt
tools-allopt.opt: $(TOOLS_OPT_TARGETS)

.PHONY: ocamltoolsopt
ocamltoolsopt: ocamlopt
	$(MAKE) tools-allopt

.PHONY: ocamltoolsopt.opt
ocamltoolsopt.opt: ocamlc.opt ocamllex.opt
	$(MAKE) tools-allopt.opt

# Tools that require a full ocaml distribution: otherlibs and toplevel

OTHER_TOOLS =

ocamltex = tools/ocamltex$(EXE)

ifeq "$(build_ocamltex)" "true"
OTHER_TOOLS += $(ocamltex)
endif

.PHONY: othertools
othertools: $(OTHER_TOOLS)

partialclean::
	for prefix in cm* dll so lib a obj; do \
	  rm -f tools/*.$$prefix; \
	done

# The dependency generator

ocamldep_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
ocamldep_SOURCES = tools/ocamldep.mli tools/ocamldep.ml

tools/ocamldep$(EXE): OC_BYTECODE_LINKFLAGS += -compat-32

# The profiler

ocamlprof_LIBRARIES =
ocamlprof_SOURCES = \
  config.mli config.ml \
  build_path_prefix_map.mli build_path_prefix_map.ml \
  misc.mli misc.ml \
  identifiable.mli identifiable.ml \
  numbers.mli numbers.ml \
  arg_helper.mli arg_helper.ml \
  local_store.mli local_store.ml \
  load_path.mli load_path.ml \
  clflags.mli clflags.ml \
  terminfo.mli terminfo.ml \
  warnings.mli warnings.ml \
  location.mli location.ml \
  longident.mli longident.ml \
  docstrings.mli docstrings.ml \
  syntaxerr.mli syntaxerr.ml \
  ast_helper.mli ast_helper.ml \
  ast_iterator.mli ast_iterator.ml \
  builtin_attributes.mli builtin_attributes.ml \
  camlinternalMenhirLib.mli camlinternalMenhirLib.ml \
  parser.mli parser.ml \
  lexer.mli lexer.ml \
  pprintast.mli pprintast.ml \
  parse.mli parse.ml \
  ocamlprof.mli ocamlprof.ml

ocamlcp_ocamloptp_SOURCES = \
  config.mli config.ml \
  build_path_prefix_map.mli build_path_prefix_map.ml \
  misc.mli misc.ml \
  profile.mli profile.ml \
  warnings.mli warnings.ml \
  identifiable.mli identifiable.ml \
  numbers.mli numbers.ml \
  arg_helper.mli arg_helper.ml \
  local_store.mli local_store.ml \
  load_path.mli load_path.ml \
  clflags.mli clflags.ml \
  terminfo.mli terminfo.ml \
  location.mli location.ml \
  ccomp.mli ccomp.ml \
  compenv.mli compenv.ml \
  main_args.mli main_args.ml \
  ocamlcp_common.mli ocamlcp_common.ml

ocamlcp_LIBRARIES =
ocamlcp_SOURCES = $(ocamlcp_ocamloptp_SOURCES) ocamlcp.mli ocamlcp.ml

ocamloptp_LIBRARIES =
ocamloptp_SOURCES = $(ocamlcp_ocamloptp_SOURCES) ocamloptp.mli ocamloptp.ml

# To help building mixed-mode libraries (OCaml + C)
ocamlmklib_LIBRARIES =
ocamlmklib_SOURCES = \
  config.ml \
  build_path_prefix_map.ml \
  misc.ml \
  ocamlmklib.mli ocamlmklib.ml

# To make custom toplevels

ocamlmktop_LIBRARIES =
ocamlmktop_SOURCES = \
  config.mli config.ml \
  build_path_prefix_map.mli build_path_prefix_map.ml \
  misc.mli misc.ml \
  identifiable.mli identifiable.ml \
  numbers.mli numbers.ml \
  arg_helper.mli arg_helper.ml \
  local_store.mli local_store.ml \
  load_path.mli load_path.ml \
  clflags.mli clflags.ml \
  profile.mli profile.ml \
  ccomp.mli ccomp.ml \
  ocamlmktop.mli ocamlmktop.ml

# Reading cmt files

ocamlcmt_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
ocamlcmt_SOURCES = tools/ocamlcmt.mli tools/ocamlcmt.ml

# The bytecode disassembler

dumpobj_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
dumpobj_SOURCES = $(addprefix tools/, \
  opnames.mli opnames.ml \
  dumpobj.mli dumpobj.ml)

make_opcodes = tools/make_opcodes$(EXE)

make_opcodes_LIBRARIES =
make_opcodes_SOURCES = tools/make_opcodes.mli tools/make_opcodes.mll

tools/opnames.ml: runtime/caml/instruct.h $(make_opcodes)
	$(V_GEN)$(NEW_OCAMLRUN) $(make_opcodes) -opnames < $< > $@

clean::
	rm -f $(addprefix tools/,opnames.ml make_opcodes.ml)

beforedepend:: $(addprefix tools/,opnames.ml make_opcodes.ml)

# Display info on compiled files

ocamlobjinfo_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp ocamlmiddleend)
ocamlobjinfo_SOURCES = tools/objinfo.mli tools/objinfo.ml

# Scan object files for required primitives

primreq_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
primreq_SOURCES = tools/primreq.mli tools/primreq.ml

# Copy a bytecode executable, stripping debug info

stripdebug_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
stripdebug_SOURCES = tools/stripdebug.mli tools/stripdebug.ml

# Compare two bytecode executables

cmpbyt_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
cmpbyt_SOURCES = tools/cmpbyt.mli tools/cmpbyt.ml

# Scan latex files, and run ocaml code examples

ocamltex_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp ocamltoplevel) \
  $(addprefix otherlibs/,str/str unix/unix)
ocamltex_SOURCES = tools/ocamltex.mli tools/ocamltex.ml

# ocamltex uses str.cma and unix.cma and so must be compiled with
# $(ROOTDIR)/ocamlc rather than with $(ROOTDIR)/boot/ocamlc since the boot
# compiler does not necessarily have the correct shared library
# configuration.
# Note: the following definitions apply to all the prerequisites
# of ocamltex.
$(ocamltex): CAMLC = $(OCAMLRUN) $(ROOTDIR)/ocamlc$(EXE) $(STDLIBFLAGS)
$(ocamltex): OC_COMMON_LINKFLAGS += -linkall
$(ocamltex): VPATH += $(addprefix otherlibs/,str unix)

tools/ocamltex.cmo: OC_COMMON_COMPFLAGS += -no-alias-deps

# we need str and unix which depend on the bytecode version of other tools
# thus we use the othertools target
## Test compilation of backend-specific parts

ARCH_SPECIFIC =\
  asmcomp/arch.mli asmcomp/arch.ml asmcomp/proc.ml asmcomp/CSE.ml \
  asmcomp/selection.ml asmcomp/scheduling.ml asmcomp/reload.ml \
  asmcomp/stackframe.ml

partialclean::
	rm -f $(ARCH_SPECIFIC)

beforedepend:: $(ARCH_SPECIFIC)

# This rule provides a quick way to check that machine-dependent
# files compiles fine for a foreign architecture (passed as ARCH=xxx).

.PHONY: check_arch
check_arch:
	@echo "========= CHECKING asmcomp/$(ARCH) =============="
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*
	@$(MAKE) compilerlibs/ocamloptcomp.cma \
	            >/dev/null
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*

.PHONY: check_all_arches
check_all_arches:
ifeq ($(ARCH64),true)
	@STATUS=0; \
	 for i in $(ARCHES); do \
	   $(MAKE) --no-print-directory check_arch ARCH=$$i || STATUS=1; \
	 done; \
	 exit $$STATUS
else
	 @echo "Architecture tests are disabled on 32-bit platforms."
endif

# The native toplevel

ocamlnat_LIBRARIES = \
  compilerlibs/ocamlcommon compilerlibs/ocamloptcomp \
  compilerlibs/ocamlbytecomp otherlibs/dynlink/dynlink \
  compilerlibs/ocamltoplevel

ocamlnat_SOURCES = $(ocaml_SOURCES)

ocamlnat$(EXE): OC_NATIVE_LINKFLAGS += -linkall -I toplevel/native

COMPILE_NATIVE_MODULE = \
  $(CAMLOPT) $(OC_COMMON_COMPFLAGS) -I $(@D) $(INCLUDES) \
  $(OC_NATIVE_COMPFLAGS)


toplevel/topdirs.cmx toplevel/toploop.cmx $(ocamlnat_CMX_FILES): \
  OC_NATIVE_COMPFLAGS += -I toplevel/native

toplevel/toploop.cmx: toplevel/native/topeval.cmx

$(ocamlnat_CMX_FILES): toplevel/native/topmain.cmx

partialclean::
	rm -f ocamlnat ocamlnat.exe

toplevel/native/topeval.cmx: otherlibs/dynlink/dynlink.cmxa

# The numeric opcodes

bytecomp/opcodes.ml: runtime/caml/instruct.h $(make_opcodes)
	$(V_GEN)$(NEW_OCAMLRUN) $(make_opcodes) -opcodes < $< > $@

bytecomp/opcodes.mli: bytecomp/opcodes.ml
	$(V_GEN)$(CAMLC) -i $< > $@

partialclean::
	rm -f bytecomp/opcodes.ml
	rm -f bytecomp/opcodes.mli

beforedepend:: bytecomp/opcodes.ml bytecomp/opcodes.mli

ifneq "$(wildcard .git)" ""
include Makefile.dev
endif

# Default rules

%.cmo: %.ml
	$(V_OCAMLC)$(CAMLC) $(OC_COMMON_COMPFLAGS) -I $(@D) $(INCLUDES) -c $<

%.cmi: %.mli
	$(V_OCAMLC)$(CAMLC) $(OC_COMMON_COMPFLAGS) -I $(@D) $(INCLUDES) -c $<

%.cmx: %.ml
	$(V_OCAMLOPT)$(COMPILE_NATIVE_MODULE) -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp middle_end file_formats \
           lambda middle_end/closure middle_end/flambda \
           middle_end/flambda/base_types \
           driver toplevel toplevel/byte toplevel/native tools debugger; do \
	  rm -f $$d/*.cm[ioxt] $$d/*.cmti $$d/*.annot $$d/*.s $$d/*.asm \
	    $$d/*.o $$d/*.obj $$d/*.so $$d/*.dll; \
	done

.PHONY: depend
depend: beforedepend
	$(V_GEN)for d in utils parsing typing bytecomp asmcomp middle_end \
         lambda file_formats middle_end/closure middle_end/flambda \
         middle_end/flambda/base_types \
         driver toplevel toplevel/byte toplevel/native lex tools debugger \
	 ocamldoc ocamltest testsuite/lib testsuite/tools; \
	 do \
	   $(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I $$d $(INCLUDES) \
	   $(OCAMLDEPFLAGS) $$d/*.mli $$d/*.ml \
	   || exit; \
         done > .depend

.PHONY: distclean
distclean: clean
	if [ -f flexdll/Makefile ]; then $(MAKE) -C flexdll distclean MSVC_DETECT=0; fi
	$(MAKE) -C manual distclean
	rm -f ocamldoc/META
	rm -f $(addprefix ocamltest/,ocamltest_config.ml ocamltest_unix.ml)
	$(MAKE) -C otherlibs distclean
	rm -f $(runtime_CONFIGURED_HEADERS)
	$(MAKE) -C stdlib distclean
	$(MAKE) -C testsuite distclean
	rm -f tools/eventlog_metadata tools/*.bak
	rm -f utils/config.common.ml utils/config.generated.ml
	rm -f compilerlibs/META
	rm -f boot/ocamlrun boot/ocamlrun.exe boot/$(HEADER_NAME) \
	      boot/flexdll_*.o boot/flexdll_*.obj \
	      boot/*.cm* boot/libcamlrun.a boot/libcamlrun.lib boot/ocamlc.opt
	rm -f Makefile.config Makefile.build_config
	rm -rf autom4te.cache flexdll-sources $(BYTE_BUILD_TREE) $(OPT_BUILD_TREE)
	rm -f config.log config.status libtool

# Installation
.PHONY: install
install:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)"
	$(MKDIR) "$(INSTALL_STUBLIBDIR)"
	$(MKDIR) "$(INSTALL_COMPLIBDIR)"
	$(MKDIR) "$(INSTALL_DOCDIR)"
	$(MKDIR) "$(INSTALL_INCDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR_PROFILING)"
	$(INSTALL_PROG) $(runtime_PROGRAMS) "$(INSTALL_BINDIR)"
	$(INSTALL_DATA) $(runtime_BYTECODE_STATIC_LIBRARIES) \
	  "$(INSTALL_LIBDIR)"
ifneq "$(runtime_BYTECODE_SHARED_LIBRARIES)" ""
	$(INSTALL_PROG) $(runtime_BYTECODE_SHARED_LIBRARIES) \
	  "$(INSTALL_LIBDIR)"
endif
	$(INSTALL_DATA) runtime/caml/domain_state.tbl runtime/caml/*.h \
	  "$(INSTALL_INCDIR)"
	$(INSTALL_PROG) ocaml$(EXE) "$(INSTALL_BINDIR)"
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(call INSTALL_STRIPPED_BYTE_PROG,\
               ocamlc$(EXE),"$(INSTALL_BINDIR)/ocamlc.byte$(EXE)")
endif
	$(MAKE) -C stdlib install
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) lex/ocamllex$(EXE) \
	  "$(INSTALL_BINDIR)/ocamllex.byte$(EXE)"
	for i in $(TOOLS_TO_INSTALL_NAT); \
	do \
	  $(INSTALL_PROG) "tools/$$i$(EXE)" "$(INSTALL_BINDIR)/$$i.byte$(EXE)";\
	  if test -f "tools/$$i".opt$(EXE); then \
	    $(INSTALL_PROG) "tools/$$i.opt$(EXE)" "$(INSTALL_BINDIR)" && \
	    (cd "$(INSTALL_BINDIR)" && $(LN) "$$i.opt$(EXE)" "$$i$(EXE)"); \
	  else \
	    (cd "$(INSTALL_BINDIR)" && $(LN) "$$i.byte$(EXE)" "$$i$(EXE)"); \
	  fi; \
	done
else
	for i in $(TOOLS_TO_INSTALL_NAT); \
	do \
	  if test -f "tools/$$i".opt$(EXE); then \
	    $(INSTALL_PROG) "tools/$$i.opt$(EXE)" "$(INSTALL_BINDIR)"; \
	    (cd "$(INSTALL_BINDIR)" && $(LN) "$$i.opt$(EXE)" "$$i$(EXE)"); \
	  fi; \
	done
endif
	for i in $(TOOLS_TO_INSTALL_BYT); \
	do \
	  $(INSTALL_PROG) "tools/$$i$(EXE)" "$(INSTALL_BINDIR)";\
	done
	$(INSTALL_PROG) $(ocamlyacc_PROGRAM)$(EXE) "$(INSTALL_BINDIR)"
	$(INSTALL_DATA) \
	   utils/*.cmi \
	   parsing/*.cmi \
	   typing/*.cmi \
	   bytecomp/*.cmi \
	   file_formats/*.cmi \
	   lambda/*.cmi \
	   driver/*.cmi \
	   toplevel/*.cmi \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   toplevel/byte/*.cmi \
	   "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   utils/*.cmt utils/*.cmti utils/*.mli \
	   parsing/*.cmt parsing/*.cmti parsing/*.mli \
	   typing/*.cmt typing/*.cmti typing/*.mli \
	   file_formats/*.cmt file_formats/*.cmti file_formats/*.mli \
	   lambda/*.cmt lambda/*.cmti lambda/*.mli \
	   bytecomp/*.cmt bytecomp/*.cmti bytecomp/*.mli \
	   driver/*.cmt driver/*.cmti driver/*.mli \
	   toplevel/*.cmt toplevel/*.cmti toplevel/*.mli \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   toplevel/byte/*.cmt \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	  tools/profiling.cmt tools/profiling.cmti \
	  "$(INSTALL_LIBDIR_PROFILING)"
endif
	$(INSTALL_DATA) \
	  compilerlibs/*.cma compilerlibs/META \
	  "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   $(ocamlc_CMO_FILES) $(ocaml_CMO_FILES) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_PROG) $(expunge) "$(INSTALL_LIBDIR)"
# If installing over a previous OCaml version, ensure some modules are removed
# from the previous installation.
	rm -f "$(INSTALL_LIBDIR)"/topdirs.cm* "$(INSTALL_LIBDIR)/topdirs.mli"
	rm -f "$(INSTALL_LIBDIR)"/profiling.cm* "$(INSTALL_LIBDIR)/profiling.$(O)"
	$(INSTALL_DATA) \
	  tools/profiling.cmi tools/profiling.cmo \
	  "$(INSTALL_LIBDIR_PROFILING)"
ifeq "$(UNIX_OR_WIN32)" "unix" # Install manual pages only on Unix
	$(MAKE) -C man install
endif
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i install || exit $$?; \
	done
ifeq "$(build_ocamldoc)" "true"
	$(MKDIR) "$(INSTALL_LIBDIR)/ocamldoc"
	$(INSTALL_PROG) $(OCAMLDOC) "$(INSTALL_BINDIR)"
	$(INSTALL_DATA) \
	  ocamldoc/ocamldoc.hva ocamldoc/*.cmi ocamldoc/odoc_info.cma \
	  ocamldoc/META \
	  "$(INSTALL_LIBDIR)/ocamldoc"
	$(INSTALL_DATA) \
	  $(OCAMLDOC_LIBCMIS) \
	  "$(INSTALL_LIBDIR)/ocamldoc"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  $(OCAMLDOC_LIBMLIS) $(OCAMLDOC_LIBCMTS) \
	  "$(INSTALL_LIBDIR)/ocamldoc"
endif
endif
ifeq "$(build_libraries_manpages)" "true"
	$(MAKE) -C api_docgen install
endif
	if test -n "$(WITH_DEBUGGER)"; then \
	  $(INSTALL_PROG) debugger/ocamldebug$(EXE) "$(INSTALL_BINDIR)"; \
	fi
ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
ifeq "$(TOOLCHAIN)" "msvc"
	$(INSTALL_DATA) $(FLEXDLL_SOURCE_DIR)/$(FLEXDLL_MANIFEST) \
    "$(INSTALL_BINDIR)/"
endif
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) \
	  flexlink.byte$(EXE) "$(INSTALL_BINDIR)"
endif # ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(MKDIR) "$(INSTALL_FLEXDLLDIR)"
	$(INSTALL_DATA) $(addprefix $(BYTE_BINDIR)/, $(FLEXDLL_OBJECTS)) \
    "$(INSTALL_FLEXDLLDIR)"
endif # ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
	$(INSTALL_DATA) Makefile.config "$(INSTALL_LIBDIR)"
	$(INSTALL_DATA) $(DOC_FILES) "$(INSTALL_DOCDIR)"
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	if test -f ocamlopt$(EXE); then $(MAKE) installopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
	   (test -f flexlink.byte$(EXE) && \
	      $(LN) flexlink.byte$(EXE) flexlink$(EXE)) || true; \
	fi
else
	if test -f ocamlopt$(EXE); then $(MAKE) installopt; fi
endif

# Installation of the native-code compiler
.PHONY: installopt
installopt:
	$(INSTALL_DATA) $(runtime_NATIVE_STATIC_LIBRARIES) "$(INSTALL_LIBDIR)"
ifneq "$(runtime_NATIVE_SHARED_LIBRARIES)" ""
	$(INSTALL_PROG) $(runtime_NATIVE_SHARED_LIBRARIES) "$(INSTALL_LIBDIR)"
endif
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(call INSTALL_STRIPPED_BYTE_PROG,\
               ocamlopt$(EXE),"$(INSTALL_BINDIR)/ocamlopt.byte$(EXE)")
endif
	$(MAKE) -C stdlib installopt
	$(INSTALL_DATA) \
	    middle_end/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/closure/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/base_types/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    asmcomp/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	    middle_end/*.cmt middle_end/*.cmti \
	    middle_end/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/closure/*.cmt middle_end/closure/*.cmti \
	    middle_end/closure/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/*.cmt middle_end/flambda/*.cmti \
	    middle_end/flambda/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/base_types/*.cmt \
            middle_end/flambda/base_types/*.cmti \
	    middle_end/flambda/base_types/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    asmcomp/*.cmt asmcomp/*.cmti \
	    asmcomp/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
endif
	$(INSTALL_DATA) \
	    $(ocamlopt_CMO_FILES) \
	    "$(INSTALL_COMPLIBDIR)"
ifeq "$(build_ocamldoc)" "true"
	$(MKDIR) "$(INSTALL_LIBDIR)/ocamldoc"
	$(INSTALL_PROG) $(OCAMLDOC_OPT) "$(INSTALL_BINDIR)"
	$(INSTALL_DATA) \
	  $(OCAMLDOC_LIBCMIS) \
	  "$(INSTALL_LIBDIR)/ocamldoc"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  $(OCAMLDOC_LIBMLIS) $(OCAMLDOC_LIBCMTS) \
	  "$(INSTALL_LIBDIR)/ocamldoc"
endif
	$(INSTALL_DATA) \
	  ocamldoc/ocamldoc.hva ocamldoc/*.cmx ocamldoc/odoc_info.$(A) \
	  ocamldoc/odoc_info.cmxa \
	  "$(INSTALL_LIBDIR)/ocamldoc"
endif
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i installopt || exit $$?; \
	done
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	if test -f ocamlopt.opt$(EXE); then $(MAKE) installoptopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.byte$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
	   (test -f flexlink.byte$(EXE) && \
	     $(LN) flexlink.byte$(EXE) flexlink$(EXE)) || true; \
	fi
else
	if test -f ocamlopt.opt$(EXE); then $(MAKE) installoptopt; fi
endif
	$(INSTALL_DATA) \
          tools/profiling.cmx tools/profiling.$(O) \
	  "$(INSTALL_LIBDIR_PROFILING)"

.PHONY: installoptopt
installoptopt:
	$(INSTALL_PROG) ocamlc.opt$(EXE) "$(INSTALL_BINDIR)"
	$(INSTALL_PROG) ocamlopt.opt$(EXE) "$(INSTALL_BINDIR)"
	$(INSTALL_PROG) lex/ocamllex.opt$(EXE) "$(INSTALL_BINDIR)"
	cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.opt$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.opt$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.opt$(EXE) ocamllex$(EXE)
ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
	$(INSTALL_PROG) flexlink.opt$(EXE) "$(INSTALL_BINDIR)"
	cd "$(INSTALL_BINDIR)"; \
	  $(LN) flexlink.opt$(EXE) flexlink$(EXE)
endif
	$(INSTALL_DATA) \
	   utils/*.cmx parsing/*.cmx typing/*.cmx bytecomp/*.cmx \
	   toplevel/*.cmx toplevel/native/*.cmx \
	   toplevel/native/tophooks.cmi \
	   file_formats/*.cmx \
	   lambda/*.cmx \
	   driver/*.cmx asmcomp/*.cmx middle_end/*.cmx \
           middle_end/closure/*.cmx \
           middle_end/flambda/*.cmx \
           middle_end/flambda/base_types/*.cmx \
          "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   compilerlibs/*.cmxa compilerlibs/*.$(A) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   $(ocamlc_CMX_FILES) $(ocamlc_CMX_FILES:.cmx=.$(O)) \
	   $(ocamlopt_CMX_FILES) $(ocamlopt_CMX_FILES:.cmx=.$(O)) \
	   $(ocamlnat_CMX_FILES:.cmx=.$(O)) \
	   "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_OCAMLNAT)" "true"
	  $(INSTALL_PROG) ocamlnat$(EXE) "$(INSTALL_BINDIR)"
endif

# Installation of the *.ml sources of compiler-libs
.PHONY: install-compiler-sources
install-compiler-sources:
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   utils/*.ml parsing/*.ml typing/*.ml bytecomp/*.ml driver/*.ml \
           file_formats/*.ml \
           lambda/*.ml \
	   toplevel/*.ml toplevel/byte/*.ml \
	   middle_end/*.ml middle_end/closure/*.ml \
     middle_end/flambda/*.ml middle_end/flambda/base_types/*.ml \
	   asmcomp/*.ml \
	   asmcmp/debug/*.ml \
	   "$(INSTALL_COMPLIBDIR)"
endif

include .depend

Makefile.config Makefile.build_config: config.status
config.status:
	@echo "Please refer to the installation instructions:"
	@echo "- In file INSTALL for Unix systems."
	@echo "- In file README.win32.adoc for Windows systems."
	@echo "On Unix systems, if you've just unpacked the distribution,"
	@echo "something like"
	@echo "  ./configure"
	@echo "  make"
	@echo "  make install"
	@echo "should work."
	@false

# We need to express that all the CMX files depend on the native compiler,
# so that they get invalidated and rebuilt when the compiler is updated
# This dependency must appear after all the definitions of the
# _SOURCES variable so that GNU make's secondary expansion mechanism works
# This is why this dependency is kept at the very end of this file

$(ALL_CMX_FILES): ocamlopt$(EXE)
