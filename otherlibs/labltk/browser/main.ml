(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open StdLabels
module Unix = UnixLabels
open Tk

let fatal_error text =
  let top = openTk ~clas:"OCamlBrowser" () in
  let mw = Message.create top ~text ~padx:20 ~pady:10
      ~width:400 ~justify:`Left ~aspect:400 ~anchor:`W
  and b = Button.create top ~text:"OK" ~command:(fun () -> destroy top) in
  pack [mw] ~side:`Top ~fill:`Both;
  pack [b] ~side:`Bottom;
  mainLoop ();
  exit 0

let rec get_incr key = function
    [] -> raise Not_found
  | (k, c, d) :: rem ->
      if k = key then
        match c with Arg.Set _ | Arg.Clear _ | Arg.Unit _ -> false | _ -> true
      else get_incr key rem

let check ~spec argv =
  let i = ref 1 in
  while !i < Array.length argv do
    try
      let a = get_incr argv.(!i) spec in
      incr i; if a then incr i
    with Not_found ->
      i := Array.length argv + 1
  done;
  !i = Array.length argv

open Printf

let print_version () =
  printf "The Objective Caml browser, version %s\n" Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  printf "%s\n" Sys.ocaml_version;
  exit 0;
;;

let usage ~spec errmsg =
  let b = Buffer.create 1024 in
  bprintf b "%s\n" errmsg;
  List.iter (function (key, _, doc) -> bprintf b "  %s %s\n" key doc) spec;
  Buffer.contents b

let _ =
  let is_win32 = Sys.os_type = "Win32" in
  if is_win32 then
    Format.pp_set_formatter_output_functions Format.err_formatter
      (fun _ _ _ -> ()) (fun _ -> ());

  let path = ref [] in
  let st = ref true in
  let spec =
    [ "-I", Arg.String (fun s -> path := s :: !path),
      "<dir>  Add <dir> to the list of include directories";
      "-labels", Arg.Clear Clflags.classic, " <obsolete>";
      "-nolabels", Arg.Set Clflags.classic,
      " Ignore non-optional labels in types";
      "-oldui", Arg.Clear st, " Revert back to old UI";
      "-pp", Arg.String (fun s -> Clflags.preprocessor := Some s),
      "<command>  Pipe sources through preprocessor <command>";
      "-rectypes", Arg.Set Clflags.recursive_types,
      " Allow arbitrary recursive types";
      "-version", Arg.Unit print_version,
        " Print version and exit";
      "-vnum", Arg.Unit print_version_num, " Print version number and exit";
      "-w", Arg.String (fun s -> Shell.warnings := s),
      "<flags>  Enable or disable warnings according to <flags>"; ]
  and errmsg = "Command line: ocamlbrowser <options>" in
  if not (check ~spec Sys.argv) then fatal_error (usage ~spec errmsg);
  Arg.parse spec
    (fun name -> raise(Arg.Bad("don't know what to do with " ^ name)))
    errmsg;
  Config.load_path :=
    Sys.getcwd ()
    :: List.rev_map ~f:(Misc.expand_directory Config.standard_library) !path
    @ [Config.standard_library];
  Warnings.parse_options false !Shell.warnings;
  Unix.putenv "TERM" "noterminal";
  begin
    try Searchid.start_env := Env.open_pers_signature "Pervasives" Env.initial
    with _ ->
      fatal_error
        (Printf.sprintf "%s\nPlease check that %s %s\nCurrent value is `%s'"
           "Couldn't initialize environment."
           (if is_win32 then "%OCAMLLIB%" else "$OCAMLLIB")
           "points to the Objective Caml library."
           Config.standard_library)
  end;

  Searchpos.view_defined_ref := (fun s ~env -> Viewer.view_defined s ~env);
  Searchpos.editor_ref := Editor.f;

  let top = openTk ~clas:"OCamlBrowser" () in
  Jg_config.init ();

  (* bind top ~events:[`Destroy] ~action:(fun _ -> exit 0); *)
  at_exit Shell.kill_all;


  if !st then Viewer.st_viewer ~on:top ()
  else Viewer.f ~on:top ();

  while true do
    try
      if is_win32 then mainLoop ()
      else Printexc.print mainLoop ()
    with Protocol.TkError _ ->
      if not is_win32 then flush stderr
  done
