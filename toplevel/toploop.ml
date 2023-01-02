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

open Format
include Topcommon
include Topeval

type input =
  | Stdin
  | File of string
  | String of string

let use_print_results = ref true

let filename_of_input = function
  | File name -> name
  | Stdin | String _ -> ""

let use_lexbuf ppf ~wrap_in_module lb name filename =
  Warnings.reset_fatal ();
  Location.init lb filename;
  (* Skip initial #! line if any *)
  Lexer.skip_hash_bang lb;
  Misc.protect_refs
    [ R (Location.input_name, filename);
      R (Location.input_lexbuf, Some lb); ]
    (fun () ->
    try
      List.iter
        (fun ph ->
          let ph = preprocess_phrase ppf ph in
          if not (execute_phrase !use_print_results ppf ph) then raise Exit)
        (if wrap_in_module then
           parse_mod_use_file name lb
         else
           !parse_use_file lb);
      true
    with
    | Exit -> false
    | Sys.Break -> fprintf ppf "Interrupted.@."; false
    | x -> Location.report_exception ppf x; false)

let use_output ppf command =
  let fn = Filename.temp_file "ocaml" "_toploop.ml" in
  Misc.try_finally ~always:(fun () ->
      try Sys.remove fn with Sys_error _ -> ())
    (fun () ->
       match
         Printf.ksprintf Sys.command "%s > %s"
           command
           (Filename.quote fn)
       with
       | 0 ->
         let ic = open_in_bin fn in
         Misc.try_finally ~always:(fun () -> close_in ic)
           (fun () ->
            let lexbuf = (Lexing.from_channel ic) in
            use_lexbuf ppf ~wrap_in_module:false lexbuf "" "(command-output)")
       | n ->
         fprintf ppf "Command exited with code %d.@." n;
         false)

let use_input ppf ~wrap_in_module input =
  match input with
  | Stdin ->
    let lexbuf = Lexing.from_channel stdin in
    use_lexbuf ppf ~wrap_in_module lexbuf "" "(stdin)"
  | String value ->
    let lexbuf = Lexing.from_string value in
    use_lexbuf ppf ~wrap_in_module lexbuf "" "(command-line input)"
  | File name ->
    match Load_path.find name with
    | filename ->
      let ic = open_in_bin filename in
      Misc.try_finally ~always:(fun () -> close_in ic)
        (fun () ->
          let lexbuf = Lexing.from_channel ic in
          use_lexbuf ppf ~wrap_in_module lexbuf name filename)
    | exception Not_found ->
      fprintf ppf "Cannot find file %s.@." name;
      false

let mod_use_input ppf name =
  use_input ppf ~wrap_in_module:true name
let use_input ppf name =
  use_input ppf ~wrap_in_module:false name
let use_file ppf name =
  use_input ppf (File name)

let use_silently ppf name =
  Misc.protect_refs
    [ R (use_print_results, false) ]
    (fun () -> use_input ppf name)

let load_file = load_file false

(* Execute a script.  If [name] is "", read the script from stdin. *)

let run_script ppf name args =
  Clflags.debug := true;
  override_sys_argv args;
  let filename = filename_of_input name in
  Compmisc.init_path ~dir:(Filename.dirname filename) ();
                   (* Note: would use [Filename.abspath] here, if we had it. *)
  begin
    try toplevel_env := Compmisc.initial_env()
    with Env.Error _ | Typetexp.Error _ as exn ->
      Location.report_exception ppf exn; raise (Compenv.Exit_with_status 2)
  end;
  Sys.interactive := false;
  run_hooks After_setup;
  let explicit_name =
    match name with
    | File name as filename  -> (
    (* Prevent use_silently from searching in the path. *)
    if name <> "" && Filename.is_implicit name
    then File (Filename.concat Filename.current_dir_name name)
    else filename)
    | (Stdin | String _) as x -> x
  in
  use_silently ppf explicit_name

(* Toplevel initialization. Performed here instead of at the
   beginning of loop() so that user code linked in with ocamlmktop
   can call directives from Topdirs. *)
let _ =
  if !Sys.interactive then (* PR#6108 *)
    invalid_arg "The ocamltoplevel.cma library from compiler-libs \
                 cannot be loaded inside the OCaml toplevel";
  Sys.interactive := true;
  Topeval.init ()

let find_ocamlinit () =
  let ocamlinit = ".ocamlinit" in
  if Sys.file_exists ocamlinit then Some ocamlinit else
  let getenv var = match Sys.getenv var with
    | exception Not_found -> None | "" -> None | v -> Some v
  in
  let exists_in_dir dir file = match dir with
    | None -> None
    | Some dir ->
        let file = Filename.concat dir file in
        if Sys.file_exists file then Some file else None
  in
  let home_dir () = getenv "HOME" in
  let config_dir () =
    if Sys.win32 then None else
    match getenv "XDG_CONFIG_HOME" with
    | Some _ as v -> v
    | None ->
        match home_dir () with
        | None -> None
        | Some dir -> Some (Filename.concat dir ".config")
  in
  let init_ml = Filename.concat "ocaml" "init.ml" in
  match exists_in_dir (config_dir ()) init_ml with
  | Some _ as v -> v
  | None -> exists_in_dir (home_dir ()) ocamlinit

let load_ocamlinit ppf =
  if !Clflags.noinit then ()
  else match !Clflags.init_file with
  | Some f ->
    if Sys.file_exists f then ignore (use_silently ppf (File f) )
    else fprintf ppf "Init file not found: \"%s\".@." f
  | None ->
      match find_ocamlinit () with
      | None -> ()
      | Some file -> ignore (use_silently ppf (File file))

(* The interactive loop *)

exception PPerror

let loop ppf =
  Clflags.debug := true;
  Location.formatter_for_warnings := ppf;
  if not !Clflags.noversion then
    fprintf ppf "OCaml version %s%s%s@.Enter #help;; for help.@.@."
      Config.version
      (if Topeval.implementation_label = "" then "" else " - ")
      Topeval.implementation_label;
  begin
    try initialize_toplevel_env ()
    with Env.Error _ | Typetexp.Error _ as exn ->
      Location.report_exception ppf exn; raise (Compenv.Exit_with_status 2)
  end;
  let lb = Lexing.from_function refill_lexbuf in
  Location.init lb "//toplevel//";
  Location.input_name := "//toplevel//";
  Location.input_lexbuf := Some lb;
  Location.input_phrase_buffer := Some phrase_buffer;
  Sys.catch_break true;
  run_hooks After_setup;
  load_ocamlinit ppf;
  while true do
    let snap = Btype.snapshot () in
    try
      Lexing.flush_input lb;
      (* Reset the phrase buffer when we flush the lexing buffer. *)
      Buffer.reset phrase_buffer;
      Location.reset();
      Warnings.reset_fatal ();
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      let phr = preprocess_phrase ppf phr  in
      Env.reset_cache_toplevel ();
      ignore(execute_phrase true ppf phr)
    with
    | End_of_file -> raise (Compenv.Exit_with_status 0)
    | Sys.Break -> fprintf ppf "Interrupted.@."; Btype.backtrack snap
    | PPerror -> ()
    | x -> Location.report_exception ppf x; Btype.backtrack snap
  done
