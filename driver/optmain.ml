(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Config
open Clflags
open Compenv

module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end
let backend = (module Backend : Backend_intf.S)

let process_interface_file ppf name =
  let opref = output_prefix name in
  Optcompile.interface ppf name opref;
  if !make_package then objfiles := (opref ^ ".cmi") :: !objfiles

let process_implementation_file ppf name =
  let opref = output_prefix name in
  Optcompile.implementation ppf name opref ~backend;
  objfiles := (opref ^ ".cmx") :: !objfiles

let cmxa_present = ref false;;

let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then
    process_implementation_file ppf name
  else if Filename.check_suffix name !Config.interface_suffix then
    process_interface_file ppf name
  else if Filename.check_suffix name ".cmx" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".cmxa" then begin
    cmxa_present := true;
    objfiles := name :: !objfiles
  end else if Filename.check_suffix name ".cmi" && !make_package then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ext_obj
       || Filename.check_suffix name ext_lib then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ".c" then begin
    Optcompile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
              :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let usage = "Usage: ocamlopt <options> <files>\nOptions are:"

let ppf = Format.err_formatter

(* Error messages to standard error formatter *)
let anonymous filename =
  readenv ppf (Before_compile filename);
  process_file ppf filename;;

let impl filename =
  readenv ppf (Before_compile filename);
  process_implementation_file ppf filename;;

let intf filename =
  readenv ppf (Before_compile filename);
  process_interface_file ppf filename;;

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

module Options = Main_args.Make_optcomp_options (struct
  let set r () = r := true
  let clear r () = r := false

  let _a = set make_archive
  let _absname = set Location.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _clambda_checks () = clambda_checks := true
  let _compact = clear optimize_for_speed
  let _config () = show_config ()
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I dir = include_dirs := dir :: !include_dirs
  let _impl = impl
  let _inline spec =
    Float_arg_helper.parse spec ~update:inline_threshold
      ~help_text:"Syntax: -inline <n> | <round>=<n>[,...]"
  let _inline_toplevel spec =
    Int_arg_helper.parse spec ~update:inline_toplevel_threshold
      ~help_text:"Syntax: -inline-toplevel <n> | <round>=<n>[,...]"
  let _inlining_report () = inlining_report := true
  let _dump_pass pass = set_dumped_pass pass true
  let _rounds n = simplify_rounds := Some n
  let _inline_max_unroll spec =
    Int_arg_helper.parse spec ~update:inline_max_unroll
      ~help_text:"Syntax: -inline-max-unroll <n> | <round>=<n>[,...]"
  let _classic_inlining () = classic_inlining := true
  let _inline_call_cost spec =
    Int_arg_helper.parse spec ~update:inline_call_cost
      ~help_text:"Syntax: -inline-call-cost <n> | <round>=<n>[,...]"
  let _inline_alloc_cost spec =
    Int_arg_helper.parse spec ~update:inline_alloc_cost
      ~help_text:"Syntax: -inline-alloc-cost <n> | <round>=<n>[,...]"
  let _inline_prim_cost spec =
    Int_arg_helper.parse spec ~update:inline_prim_cost
      ~help_text:"Syntax: -inline-prim-cost <n> | <round>=<n>[,...]"
  let _inline_branch_cost spec =
    Int_arg_helper.parse spec ~update:inline_branch_cost
      ~help_text:"Syntax: -inline-branch-cost <n> | <round>=<n>[,...]"
  let _inline_indirect_cost spec =
    Int_arg_helper.parse spec ~update:inline_indirect_cost
      ~help_text:"Syntax: -inline-indirect-cost <n> | <round>=<n>[,...]"
  let _inline_lifting_benefit spec =
    Int_arg_helper.parse spec ~update:inline_lifting_benefit
      ~help_text:"Syntax: -inline-lifting-benefit <n> | <round>=<n>[,...]"
  let _inline_branch_factor spec =
    Float_arg_helper.parse spec ~update:inline_branch_factor
      ~help_text:"Syntax: -inline-branch-factor <n> | <round>=<n>[,...]"
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _keep_docs = set keep_docs
  let _no_keep_docs = clear keep_docs
  let _keep_locs = set keep_locs
  let _no_keep_locs = clear keep_locs
  let _labels = clear classic
  let _linkall = set link_everything
  let _inline_max_depth spec =
    Int_arg_helper.parse spec ~update:inline_max_depth
      ~help_text:"Syntax: -inline-max-depth <n> | <round>=<n>[,...]"
  let _alias_deps = clear transparent_modules
  let _no_alias_deps = set transparent_modules
  let _app_funct = set applicative_functors
  let _no_app_funct = clear applicative_functors
  let _no_float_const_prop = clear float_const_prop
  let _noassert = set noassert
  let _noautolink = set no_auto_link
  let _nodynlink = clear dlcode
  let _nolabels = set classic
  let _nostdlib = set no_std_include
  let _no_unbox_free_vars_of_closures = clear unbox_free_vars_of_closures
  let _no_unbox_specialised_args = clear unbox_specialised_args
  let _o s = output_name := Some s
  (* CR-someday mshinwell: should stop e.g. -O2 -classic-inlining
     lgesbert: could be done in main() below, like for -pack and -c, but that
     would prevent overriding using OCAMLPARAM.
     mshinwell: We're going to defer this for the moment and add a note in
     the manual that the behaviour is unspecified in cases such as this.
     We should refactor the code so that the user's requirements are
     collected, then checked all at once for illegal combinations, and then
     transformed into the settings of the individual parameters.
  *)
  let _o2 () =
    default_simplify_rounds := 2;
    use_inlining_arguments_set o2_arguments;
    use_inlining_arguments_set ~round:0 o1_arguments
  let _o3 () =
    default_simplify_rounds := 3;
    use_inlining_arguments_set o3_arguments;
    use_inlining_arguments_set ~round:1 o2_arguments;
    use_inlining_arguments_set ~round:0 o1_arguments
  let _open s = open_modules := s :: !open_modules
  let _output_obj = set output_c_object
  let _output_complete_obj () =
    set output_c_object (); set output_complete_object ()
  let _p = set gprofile
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = first_ppx := s :: !first_ppx
  let _principal = set principal
  let _no_principal = clear principal
  let _rectypes = set recursive_types
  let _no_rectypes = clear recursive_types
  let _remove_unused_arguments = set remove_unused_arguments
  let _runtime_variant s = runtime_variant := s
  let _safe_string = clear unsafe_string
  let _short_paths = clear real_paths
  let _strict_sequence = set strict_sequence
  let _no_strict_sequence = clear strict_sequence
  let _strict_formats = set strict_formats
  let _no_strict_formats = clear strict_formats
  let _shared () = shared := true; dlcode := true
  let _S = set keep_asm_file
  let _thread = set use_threads
  let _unbox_closures = set unbox_closures
  let _unbox_closures_factor f = unbox_closures_factor := f
  let _unsafe = set fast
  let _unsafe_string = set unsafe_string
  let _v () = print_version_and_library "native-code compiler"
  let _version () = print_version_string ()
  let _vnum () = print_version_string ()
  let _verbose = set verbose
  let _w s = Warnings.parse_options false s
  let _warn_error s = Warnings.parse_options true s
  let _warn_help = Warnings.help_warnings
  let _color option =
    begin match Clflags.parse_color_setting option with
          | None -> ()
          | Some setting -> Clflags.color := setting
    end
  let _where () = print_standard_library ()

  let _nopervasives = set nopervasives
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _drawclambda = set dump_rawclambda
  let _dclambda = set dump_clambda
  let _drawflambda = set dump_rawflambda
  let _dflambda = set dump_flambda
  let _dflambda_let stamp = dump_flambda_let := Some stamp
  let _dflambda_verbose () =
    set dump_flambda ();
    set dump_flambda_verbose ()
  let _dflambda_no_invariants = clear flambda_invariant_checks
  let _dcmm = set dump_cmm
  let _dsel = set dump_selection
  let _dcombine = set dump_combine
  let _dcse = set dump_cse
  let _dlive () = dump_live := true; Printmach.print_live := true
  let _dspill = set dump_spill
  let _dsplit = set dump_split
  let _dinterf = set dump_interf
  let _dprefer = set dump_prefer
  let _dalloc = set dump_regalloc
  let _dreload = set dump_reload
  let _dscheduling = set dump_scheduling
  let _dlinear = set dump_linear
  let _dstartup = set keep_startup_file
  let _dtimings = set print_timings
  let _opaque = set opaque

  let anonymous = anonymous
end);;

let main () =
  native_code := true;
  let ppf = Format.err_formatter in
  try
    readenv ppf Before_args;
    Arg.parse (Arch.command_line_options @ Options.list) anonymous usage;
    readenv ppf Before_link;
    if
      List.length (List.filter (fun x -> !x)
                     [make_package; make_archive; shared;
                      compile_only; output_c_object]) > 1
    then
      fatal "Please specify at most one of -pack, -a, -shared, -c, -output-obj";
    if !make_archive then begin
      if !cmxa_present then
        fatal "Option -a cannot be used with .cmxa input files.";
      Compmisc.init_path true;
      let target = extract_output !output_name in
      Asmlibrarian.create_archive (get_objfiles ()) target;
      Warnings.check_fatal ();
    end
    else if !make_package then begin
      Compmisc.init_path true;
      let target = extract_output !output_name in
      Asmpackager.package_files ppf (Compmisc.initial_env ())
        (get_objfiles ()) target ~backend;
      Warnings.check_fatal ();
    end
    else if !shared then begin
      Compmisc.init_path true;
      let target = extract_output !output_name in
      Asmlink.link_shared ppf (get_objfiles ()) target;
      Warnings.check_fatal ();
    end
    else if not !compile_only && !objfiles <> [] then begin
      let target =
        if !output_c_object then
          let s = extract_output !output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll)
          then s
          else
            fatal
              (Printf.sprintf
                 "The extension of the output file must be %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
          default_output !output_name
      in
      Compmisc.init_path true;
      Asmlink.link ppf (get_objfiles ()) target;
      Warnings.check_fatal ();
    end;
  with x ->
      Location.report_exception ppf x;
      exit 2

let _ =
  Timings.(time All) main ();
  if !Clflags.print_timings then Timings.print Format.std_formatter;
  exit 0
