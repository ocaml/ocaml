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

let use_lexbuf ppf ~wrap_in_module lb ~modpath ~filename =
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
           parse_mod_use_file modpath lb
         else
           !parse_use_file lb);
      true
    with
    | Exit -> false
    | Sys.Break -> fprintf ppf "Interrupted.@."; false
    | x -> Location.report_exception ppf x; false)

(** [~modpath] is used to determine the module name when [wrap_in_module]
    [~filepath] is the filesystem path to the input,
    [~filename] is the name of the file that should be shown
    to the user. It may differ from [filepath] when using a temporary file. *)
let use_file ppf ~wrap_in_module ~modpath ~filepath ~filename =
  let source = In_channel.with_open_bin filepath In_channel.input_all in
  let lexbuf = Lexing.from_string source in
  use_lexbuf ppf ~wrap_in_module lexbuf ~modpath ~filename

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
         use_file ppf ~wrap_in_module:false ~modpath:""
           ~filepath:fn ~filename:"(command-output)"
       | n ->
         fprintf ppf "Command exited with code %d.@." n;
         false)

let use_input ppf ~wrap_in_module input =
  match input with
  | Stdin ->
    let lexbuf = Lexing.from_channel stdin in
    use_lexbuf ppf ~wrap_in_module lexbuf ~modpath:"" ~filename:"(stdin)"
  | String value ->
    let lexbuf = Lexing.from_string value in
    use_lexbuf ppf ~wrap_in_module lexbuf
      ~modpath:"" ~filename:"(command-line input)"
  | File name ->
    match Load_path.find name with
    | filename ->
      use_file ppf ~wrap_in_module ~modpath:name ~filename ~filepath:filename
    | exception Not_found ->
      fprintf ppf "Cannot find file %s.@." name;
      false

let mod_use_input ppf input =
  use_input ppf ~wrap_in_module:true input
let use_input ppf input =
  use_input ppf ~wrap_in_module:false input
let use_file ppf name =
  use_input ppf (File name)

let use_silently ppf input =
  Misc.protect_refs
    [ R (use_print_results, false) ]
    (fun () -> use_input ppf input)

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

(* Split a PATH-style variable, Windows-style. Entries are separated by
   semicolons. Sections of entries may be double-quoted (which allows
   semicolons in filenames to be quoted). The double-quote characters are
   stripped (i.e. [f"o"o = foo]).
   The Windows behaviour is sparsely documented: the primary source is the
   comment from 1989 at the top of env/getpath.cpp in the Universal C Runtime.
   See also https://devblogs.microsoft.com/oldnewthing/20060929-06/?p=29533 *)
let split_path_win32 path =
  (* Buffer for storing the current segment being scanned *)
  let buf = Buffer.create 256 in
  let get_contents () =
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s
  in
  let add_segment segment_begin i =
    Buffer.add_substring buf path segment_begin (i - segment_begin)
  in
  let len = String.length path in
  let[@tail_mod_cons] rec parse segment_begin terminator i =
    if i >= len then
      (* Done - return the last entry *)
      [get_contents (add_segment segment_begin i)]
    else
      let ch = path.[i] in
      (* terminator is either ';' or '"' *)
      if ch = terminator then begin
        add_segment segment_begin i;
        if ch = ';' then
          (* Return this entry and begin scanning the next *)
          get_contents () :: parse (succ i) ';' (succ i)
        else
          (* Finished scanning '".."' so continue scanning this entry *)
          parse (succ i) ';' (succ i)
      end else if ch = '"' then begin
        (* Encountered the beginning of a quoted segment *)
        add_segment segment_begin i;
        parse (succ i) '"' (succ i)
      end else
        parse segment_begin terminator (succ i)
  in
  parse 0 ';' 0

let split_path =
  if Sys.win32 then
    split_path_win32
  else
    String.split_on_char ':'

external windows_xdg_defaults : unit -> string list = "caml_xdg_defaults"

let find_ocamlinit () =
  let ocamlinit = ".ocamlinit" in
  (* 1. .ocamlinit in the current directory *)
  if Sys.file_exists ocamlinit then Some ocamlinit else
  let init_ml = Filename.concat "ocaml" "init.ml" in
  let getenv var = match Sys.getenv_opt var with Some "" -> None | v -> v in
  let is_absolute = Fun.negate Filename.is_relative in
  let exists_in_dir ~file dir =
    let file = Filename.concat dir file in
    if Sys.file_exists file then Some file else None
  in
  let home_dir () = getenv "HOME" in
  let windows_xdg_defaults = Lazy.from_fun windows_xdg_defaults in
  (* 2. ocaml/init.ml under $XDG_CONFIG_HOME (or $HOME/.config on Unix, if
        $XDG_CONFIG_HOME is unset, empty or not an absolute path) *)
  let check_xdg_config_home () =
    match getenv "XDG_CONFIG_HOME" with
    | Some dir when is_absolute dir ->
        exists_in_dir ~file:init_ml dir
    | _ ->
        let default =
          if Sys.win32 then
            (* The first entry of the list is FOLDERID_LocalAppData (exposed by
               default in the process environment as %LOCALAPPDATA%) *)
            match Lazy.force windows_xdg_defaults with
            | dir::_ -> Some dir
            | [] -> None
          else
            Option.map (fun dir -> Filename.concat dir ".config") (home_dir ())
        in
        Option.bind default (exists_in_dir ~file:init_ml)
  in
  (* 3. ocaml/init.ml under any of $XDG_CONFIG_DIRS (or /etc/xdg on Unix, or
        %LOCALAPPDATA%, %APPDATA%, %PROGRAMDATA% on Windows) *)
  let check_xdg_config_dirs () =
    let dirs_from_env =
      match getenv "XDG_CONFIG_DIRS" with
      | Some entry -> List.filter is_absolute (split_path entry)
      | None -> []
    in
    let search =
      if dirs_from_env = [] then
        if Sys.win32 then
          (* There's a non-zero chance that a user of Cygwin, etc. sets
             XDG_CONFIG_HOME for their Cygwin installation and then starts
             native Windows `ocaml.exe` from within that installation. In this
             scenario, XDG_CONFIG_HOME is very unlikely to be a valid path (as
             Cygwin won't have translated it from Unix notation). To mitigate
             this, the default value we take for XDG_CONFIG_DIRS on Windows
             includes the default for XDG_CONFIG_HOME again. If the Cygwin user
             has set both XDG_CONFIG_HOME and XDG_CONFIG_DIRS then we can't help
             them! *)
          Lazy.force windows_xdg_defaults
        else
          ["/etc/xdg"]
      else
        dirs_from_env
    in
    List.find_map (exists_in_dir ~file:init_ml) search
  in
  (* 4. .ocamlinit in $HOME *)
  let check_home () =
    Option.bind (home_dir ()) (exists_in_dir ~file:ocamlinit)
  in
  List.find_map (fun f -> f ())
                [check_xdg_config_home;
                 check_xdg_config_dirs;
                 check_home]

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

let ends_with_lf lb =
  let open Lexing in
  Bytes.get lb.lex_buffer (lb.lex_buffer_len - 1) = '\n'

(* Without changing the state of [lb], try to see if it contains a token.
   Return [EOF] if there is no token in [lb], a token if there is one,
   or raise a lexer error as appropriate.
   Print lexer warnings or not according to [print_warnings].
*)
let look_ahead ~print_warnings lb =
  let shadow =
    Lexing.{ lb with
      refill_buff = (fun newlb -> newlb.lex_eof_reached <- true);
      lex_buffer = Bytes.copy lb.lex_buffer;
      lex_mem = Array.copy lb.lex_mem;
    }
  in
  Misc.protect_refs [
      R (Lexer.print_warnings, print_warnings);
      Location.(R (report_printer, fun () -> batch_mode_printer));
    ] (fun () -> Lexer.token shadow)
;;

(* Refill the buffer until the next linefeed or end-of-file that is not
   inside a comment and check that its contents can be ignored.
   We do this by adding whole lines to the lexbuf until one of these
   occurs:
   - it contains no tokens and no unterminated comments
   - it contains some token or unterminated string
   - it contains a lexical error
*)
let is_blank_with_linefeed lb =
  let open Lexing in
  if Bytes.get lb.lex_buffer lb.lex_curr_pos = '\n' then
    (* shortcut for the most usual case *)
    true
  else begin
    let rec loop () =
      if not (lb.lex_eof_reached || ends_with_lf lb) then begin
        (* Make sure the buffer does not contain a truncated line. *)
        lb.refill_buff lb;
        loop ()
      end else begin
        (* Check for tokens in the lexbuf. We may have to
           repeat this step, so don't print any warnings yet. *)
        match look_ahead ~print_warnings:false lb with
        | EOF -> true (* no tokens *)
        | _ -> false (* some token *)
        | exception Lexer.(Error ((Unterminated_comment _
                                   | Unterminated_string_in_comment _), _)) ->
            (* In this case we don't know whether there will be a token
               before the next linefeed, so get more chars and continue. *)
            Misc.protect_refs [ R (comment_prompt_override, true) ]
              (fun () -> lb.refill_buff lb);
            loop ()
        | exception _ -> false (* syntax error *)
      end
    in
    loop ()
  end

(* Read and parse toplevel phrases, stop when a complete phrase has been
   parsed and the lexbuf contains and end of line with optional whitespace
   and comments. *)
let rec get_phrases ppf lb phrs =
  match !parse_toplevel_phrase lb with
  | phr ->
    if is_blank_with_linefeed lb then begin
      (* The lexbuf does not contain any tokens. We know it will be
         flushed after the phrases are evaluated, so print warnings now. *)
      ignore (look_ahead ~print_warnings:true lb);
      List.rev (phr :: phrs)
    end else
      get_phrases ppf lb (phr :: phrs)
  | exception Exit -> raise PPerror
  | exception e -> Location.report_exception ppf e; []

(* Type, compile and execute a phrase. *)
let process_phrase ppf snap phr =
  snap := Btype.snapshot ();
  Warnings.reset_fatal ();
  let phr = preprocess_phrase ppf phr in
  Env.reset_cache_toplevel ();
  ignore(execute_phrase true ppf phr)

(* Type, compile and execute a list of phrases, setting the report printer
   to batch mode for all but the first one.
   We have to use batch mode for reporting for two reasons:
   1. we can't underline several parts of the input line(s) in place
   2. the execution of the first phrase may mess up the line count so we
      can't move the cursor back to the correct line
 *)
let process_phrases ppf snap phrs =
  match phrs with
  | [] -> ()
  | phr :: rest ->
    process_phrase ppf snap phr;
    if rest <> [] then begin
      let process ph = Location.reset (); process_phrase ppf snap ph in
      Misc.protect_refs
        Location.[R (report_printer, fun () -> batch_mode_printer)]
        (fun () -> List.iter process rest)
    end

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
    let snap = ref (Btype.snapshot ()) in
    try
      Lexing.flush_input lb;
      (* Reset the phrase buffer when we flush the lexing buffer. *)
      Buffer.reset phrase_buffer;
      Location.reset();
      first_line := true;
      let phrs = get_phrases ppf lb [] in
      process_phrases ppf snap phrs
    with
    | End_of_file -> raise (Compenv.Exit_with_status 0)
    | Sys.Break -> fprintf ppf "Interrupted.@."; Btype.backtrack !snap
    | PPerror -> ()
    | x -> Location.report_exception ppf x; Btype.backtrack !snap
  done
