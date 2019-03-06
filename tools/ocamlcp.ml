(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Printf

let make_archive = ref false;;
let with_impl = ref false;;
let with_intf = ref false;;
let with_mli = ref false;;
let with_ml = ref false;;

let process_file filename =
  if Filename.check_suffix filename ".ml" then with_ml := true;
  if Filename.check_suffix filename ".mli" then with_mli := true;
;;

let usage = "Usage: ocamlcp <options> <files>\noptions are:"

let incompatible o =
  fprintf stderr "ocamlcp: profiling is incompatible with the %s option\n" o;
  exit 2

module Options = Main_args.Make_bytecomp_options (struct
  let _a () = make_archive := true
  let _absname = ignore
  let _alert = ignore
  let _annot = ignore
  let _binannot = ignore
  let _c = ignore
  let _cc = ignore
  let _cclib = ignore
  let _ccopt = ignore
  let _config = ignore
  let _config_var = ignore
  let _compat_32 = ignore
  let _custom = ignore
  let _dllib = ignore
  let _dllpath = ignore
  let _dtypes = ignore
  let _for_pack = ignore
  let _g = ignore
  let _stop_after = ignore
  let _i = ignore
  let _I = ignore
  let _impl _ = with_impl := true
  let _intf _ = with_intf := true
  let _intf_suffix = ignore
  let _keep_docs = ignore
  let _no_keep_docs = ignore
  let _keep_locs = ignore
  let _no_keep_locs = ignore
  let _labels = ignore
  let _linkall = ignore
  let _make_runtime = ignore
  let _alias_deps = ignore
  let _no_alias_deps = ignore
  let _app_funct = ignore
  let _no_app_funct = ignore
  let _no_check_prims = ignore
  let _noassert = ignore
  let _nolabels = ignore
  let _noautolink = ignore
  let _nostdlib = ignore
  let _o = ignore
  let _opaque = ignore
  let _open = ignore
  let _output_obj = ignore
  let _output_complete_obj = ignore
  let _pack = ignore
  let _plugin = ignore
  let _pp _ = incompatible "-pp"
  let _ppx _ = incompatible "-ppx"
  let _principal = ignore
  let _no_principal = ignore
  let _rectypes = ignore
  let _no_rectypes = ignore
  let _runtime_variant = ignore
  let _safe_string = ignore
  let _short_paths = ignore
  let _strict_sequence = ignore
  let _no_strict_sequence = ignore
  let _strict_formats = ignore
  let _no_strict_formats = ignore
  let _thread = ignore
  let _vmthread = ignore
  let _unboxed_types = ignore
  let _no_unboxed_types = ignore
  let _unsafe = ignore
  let _unsafe_string = ignore
  let _use_prims = ignore
  let _use_runtime = ignore
  let _v = ignore
  let _version = ignore
  let _vnum = ignore
  let _verbose = ignore
  let _w = ignore
  let _warn_error = ignore
  let _warn_help = ignore
  let _color = ignore
  let _error_style = ignore
  let _where = ignore
  let _nopervasives = ignore
  let _match_context_rows = ignore
  let _dump_into_file = ignore
  let _dno_unique_ids = ignore
  let _dunique_ids = ignore
  let _dsource = ignore
  let _dparsetree = ignore
  let _dtypedtree = ignore
  let _drawlambda = ignore
  let _drawlambda_loc = ignore
  let _dlambda = ignore
  let _dlambda_loc = ignore
  let _dflambda = ignore
  let _dinstr = ignore
  let _dcamlprimc = ignore
  let _dtimings = ignore
  let _dprofile = ignore
  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0
  let anonymous = process_file
end);;

let rev_compargs = ref ([] : string list)
let rev_profargs = ref ([] : string list)

let add_profarg s =
  rev_profargs := (Filename.quote s) :: "-m" :: !rev_profargs
;;

let anon filename =
  process_file filename;
  rev_compargs := Filename.quote filename :: !rev_compargs
;;

let optlist =
    ("-P", Arg.String add_profarg,
           "[afilmt]  Profile constructs specified by argument (default fm):\n\
        \032     a  Everything\n\
        \032     f  Function calls and method calls\n\
        \032     i  if ... then ... else\n\
        \032     l  while and for loops\n\
        \032     m  match ... with\n\
        \032     t  try ... with")
    :: ("-p", Arg.String add_profarg, "[afilmt]  Same as option -P")
    :: Main_args.options_with_command_line_syntax Options.list rev_compargs
in
Arg.parse_expand optlist anon usage;
if !with_impl && !with_intf then begin
  fprintf stderr "ocamlcp cannot deal with both \"-impl\" and \"-intf\"\n";
  fprintf stderr "please compile interfaces and implementations separately\n";
  exit 2;
end else if !with_impl && !with_mli then begin
  fprintf stderr "ocamlcp cannot deal with both \"-impl\" and .mli files\n";
  fprintf stderr "please compile interfaces and implementations separately\n";
  exit 2;
end else if !with_intf && !with_ml then begin
  fprintf stderr "ocamlcp cannot deal with both \"-intf\" and .ml files\n";
  fprintf stderr "please compile interfaces and implementations separately\n";
  exit 2;
end;
if !with_impl then rev_profargs := "-impl" :: !rev_profargs;
if !with_intf then rev_profargs := "-intf" :: !rev_profargs;
let status =
  Sys.command
    (Printf.sprintf "ocamlc -pp \"ocamlprof -instrument %s\" %s %s"
        (String.concat " " (List.rev !rev_profargs))
        (if !make_archive then "" else "profiling.cmo")
        (String.concat " " (List.rev !rev_compargs)))
in
exit status
;;
