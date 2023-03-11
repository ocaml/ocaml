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

let preload_objects = ref []

(* Position of the first non expanded argument *)
let first_nonexpanded_pos = ref 0

let current = ref (!Arg.current)

let argv = ref Sys.argv

(* Test whether the option is part of a responsefile *)
let is_expanded pos = pos < !first_nonexpanded_pos

let expand_position pos len =
  if pos < !first_nonexpanded_pos then
    (* Shift the position *)
    first_nonexpanded_pos := !first_nonexpanded_pos + len
  else
    (* New last position *)
    first_nonexpanded_pos := pos + len + 2


let prepare ppf =
  Topcommon.set_paths ();
  try
    let res =
      List.for_all (Topeval.load_file false ppf) (List.rev !preload_objects)
    in
    Topcommon.run_hooks Topcommon.Startup;
    res
  with x ->
    try Location.report_exception ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

let input_argument name =
  let filename = Toploop.filename_of_input name in
  let ppf = Format.err_formatter in
  if Filename.check_suffix filename ".cmxs"
    || Filename.check_suffix filename ".cmx"
    || Filename.check_suffix filename ".cmxa"
  then preload_objects := filename :: !preload_objects
  else if is_expanded !current then begin
    (* Script files are not allowed in expand options because otherwise the
       check in override arguments may fail since the new argv can be larger
       than the original argv.
    *)
    Printf.eprintf "For implementation reasons, the toplevel does not support\
    \ having script files (here %S) inside expanded arguments passed through\
    \ the -args{,0} command-line option.\n" filename;
    raise (Compenv.Exit_with_status 2)
  end else begin
    let newargs = Array.sub !argv !Arg.current
                              (Array.length !argv - !Arg.current)
      in
      Compmisc.read_clflags_from_env ();
      if prepare ppf && Toploop.run_script ppf name newargs
      then raise (Compenv.Exit_with_status 0)
      else raise (Compenv.Exit_with_status 2)
    end

let file_argument x = input_argument (Toploop.File x)

let wrap_expand f s =
  let start = !current in
  let arr = f s in
  expand_position start (Array.length arr);
  arr

module Options = Main_args.Make_opttop_options (struct
    include Main_args.Default.Opttopmain
    let _stdin () = input_argument Toploop.Stdin
    let _args = wrap_expand Arg.read_arg
    let _args0 = wrap_expand Arg.read_arg0
    let anonymous s = file_argument s
    let _eval s = input_argument (Toploop.String s)

end)

let main () =
  let ppf = Format.err_formatter in
  Clflags.native_code := true;
  let program = "ocamlnat" in
  let display_deprecated_script_alert =
    Array.length !argv >= 2 && Topcommon.is_command_like_name !argv.(1)
  in
  Topcommon.update_search_path_from_env ();
  Compenv.readenv ppf Before_args;
  if display_deprecated_script_alert then
    Location.deprecated_script_alert program;
  Clflags.add_arguments __LOC__ Options.list;
  Compenv.parse_arguments ~current argv file_argument program;
  Compmisc.read_clflags_from_env ();
  if not (prepare Format.err_formatter) then raise (Compenv.Exit_with_status 2);
  Compmisc.init_path ();
  Toploop.loop Format.std_formatter

let main () =
  match main () with
  | exception Compenv.Exit_with_status n -> n
  | () -> 0
