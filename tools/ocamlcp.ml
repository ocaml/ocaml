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
    include Main_args.Default.Main
    let _a () = make_archive := true
    let _impl _ = with_impl := true
    let _intf _ = with_intf := true
    let _pp _ = incompatible "-pp"
    let _ppx _ = incompatible "-ppx"
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
begin try
  Arg.parse_expand optlist anon usage
with Compenv.Exit_with_status n -> exit n
end;
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
