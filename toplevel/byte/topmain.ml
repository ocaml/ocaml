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

let usage = "Usage: ocaml <options> <object-files> [script-file [arguments]]\n\
             options are:"

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
  Toploop.set_paths ();
  try
    let res =
      let objects =
        List.rev (!preload_objects @ !Compenv.first_objfiles)
      in
      List.for_all (Topdirs.load_file ppf) objects
    in
    Toploop.run_hooks Toploop.Startup;
    res
  with x ->
    try Location.report_exception ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

(* If [name] is "", then the "file" is stdin treated as a script file. *)
let file_argument name =
  let ppf = Format.err_formatter in
  if Filename.check_suffix name ".cmo" || Filename.check_suffix name ".cma"
  then preload_objects := name :: !preload_objects
  else if is_expanded !current then begin
    (* Script files are not allowed in expand options because otherwise the
       check in override arguments may fail since the new argv can be larger
       than the original argv.
    *)
    Printf.eprintf "For implementation reasons, the toplevel does not support\
   \ having script files (here %S) inside expanded arguments passed through the\
   \ -args{,0} command-line option.\n" name;
    raise (Compenv.Exit_with_status 2)
  end else begin
      let newargs = Array.sub !argv !current
                              (Array.length !argv - !current)
      in
      Compenv.readenv ppf Before_link;
      Compmisc.read_clflags_from_env ();
      if prepare ppf && Toploop.run_script ppf name newargs
      then raise (Compenv.Exit_with_status 0)
      else raise (Compenv.Exit_with_status 2)
    end


let wrap_expand f s =
  let start = !current in
  let arr = f s in
  expand_position start (Array.length arr);
  arr

module Options = Main_args.Make_bytetop_options (struct
    include Main_args.Default.Topmain
    let _stdin () = file_argument ""
    let _args = wrap_expand Arg.read_arg
    let _args0 = wrap_expand Arg.read_arg0
    let anonymous s = file_argument s
end);;

let () =
  let extra_paths =
    match Sys.getenv "OCAMLTOP_INCLUDE_PATH" with
    | exception Not_found -> []
    | s -> Misc.split_path_contents s
  in
  Clflags.include_dirs := List.rev_append extra_paths !Clflags.include_dirs

let main () =
  let ppf = Format.err_formatter in
  Compenv.readenv ppf Before_args;
  let list = ref Options.list in
  begin
    try
      Arg.parse_and_expand_argv_dynamic current argv list file_argument usage;
    with
    | Arg.Bad msg -> Printf.eprintf "%s" msg; raise (Compenv.Exit_with_status 2)
    | Arg.Help msg -> Printf.printf "%s" msg; raise (Compenv.Exit_with_status 0)
  end;
  Compenv.readenv ppf Before_link;
  Compmisc.read_clflags_from_env ();
  if not (prepare ppf) then raise (Compenv.Exit_with_status 2);
  Compmisc.init_path ();
  Toploop.loop Format.std_formatter

let main () =
  match main () with
  | exception Compenv.Exit_with_status n -> n
  | () -> 0
