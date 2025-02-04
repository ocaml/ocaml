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

module type Ocamlcp_args = sig
  val _a : unit -> unit
  val _impl : string -> unit
  val _intf : string -> unit
  val _pp : string -> unit
  val _ppx : string -> unit
  val anonymous : string -> unit
end

module type OCAMLCP = sig
  val bytecode : bool
  module Make_options : Ocamlcp_args -> Main_args.Arg_list
end

module Make(T: OCAMLCP) = struct
  let name = if T.bytecode then "ocamlcp" else "ocamloptp"

  let make_archive = ref false
  let with_impl = ref false
  let with_intf = ref false
  let with_mli = ref false
  let with_ml = ref false

  let process_file filename =
    if Filename.check_suffix filename ".ml" then with_ml := true;
    if Filename.check_suffix filename ".mli" then with_mli := true

  let usage = "Usage: " ^ name ^ " <options> <files>\nOptions are:"

  let incompatible o =
    Printf.eprintf "%s: profiling is incompatible with the %s option\n" name o;
    exit 2

  module Options = T.Make_options(struct
    (* Pre-process the options to ensure that the call to the compiler will
       succeed. Only the affected options are overridden. *)
    let _a () = make_archive := true
    let _impl _ = with_impl := true
    let _intf _ = with_intf := true
    let _pp _ = incompatible "-pp"
    let _ppx _ = incompatible "-ppx"
    let anonymous = process_file
  end)

  let rev_compargs = ref ([] : string list)
  let rev_profargs = ref ([] : string list)

  let add_profarg s =
    rev_profargs := (Filename.quote s) :: "-m" :: !rev_profargs

  let anon filename =
    process_file filename;
    rev_compargs := Filename.quote filename :: !rev_compargs

  let optlist =
    let profarg =
      ("-P", Arg.String add_profarg,
            "[afilmt]  Profile constructs specified by argument (default fm):\n\
          \032     a  Everything\n\
          \032     f  Function calls and method calls\n\
          \032     i  if ... then ... else\n\
          \032     l  while and for loops\n\
          \032     m  match ... with\n\
          \032     t  try ... with") in
    let inherited_options =
      Main_args.options_with_command_line_syntax Options.list rev_compargs in
    if T.bytecode then
      profarg
      (* Add the legacy "-p" option *)
      :: ("-p", Arg.String add_profarg, "[afilmt]  Same as option -P")
      :: inherited_options
    else
      profarg
      :: inherited_options

  let main () =
    begin try
      Arg.parse_expand optlist anon usage
    with Compenv.Exit_with_status n -> exit n
    end;
    let cannot_deal_with a b =
      Printf.eprintf
        "%s cannot deal with both \"%s\" and %s\n\
         please compile interfaces and implementations separately\n" name a b;
      exit 2 in
    if !with_impl && !with_intf then
      cannot_deal_with "-impl" "\"-intf\""
    else if !with_impl && !with_mli then
      cannot_deal_with "-impl" ".mli files"
    else if !with_intf && !with_ml then
      cannot_deal_with "-intf" ".ml files";
    if !with_impl then rev_profargs := "-impl" :: !rev_profargs;
    if !with_intf then rev_profargs := "-intf" :: !rev_profargs;
    begin match !Clflags.keyword_edition with
    | None -> ()
    | Some k ->
       rev_profargs := ("-keywords " ^ k) :: !rev_profargs
    end;
    let status =
      let profiling_object =
        if T.bytecode then "profiling.cmo" else "profiling.cmx" in
      Printf.ksprintf Sys.command
        "%s -pp \"ocamlprof -instrument %s\" -I +profiling %s %s"
          (if T.bytecode then "ocamlc" else "ocamlopt")
          (String.concat " " (List.rev !rev_profargs))
          (if !make_archive then "" else profiling_object)
          (String.concat " " (List.rev !rev_compargs))
    in
    exit status
end
