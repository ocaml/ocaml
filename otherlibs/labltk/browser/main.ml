(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open StdLabels
module Unix = UnixLabels
open Tk

let _ =
  let path = ref [] in
  let st = ref false in
  Arg.parse
    [ "-I", Arg.String (fun s -> path := s :: !path),
      "<dir>  Add <dir> to the list of include directories";
      "-labels", Arg.Clear Clflags.classic,
      " Use commuting label syntax";
      "-rectypes", Arg.Set Clflags.recursive_types,
      " Allow arbitrary recursive types";
      "-st", Arg.Set st, " Smalltalk-like one-box browsing";
      "-w", Arg.String (fun s -> Shell.warnings := s),
      "<flags>  Enable or disable warnings according to <flags>:\n\
        \032    A/a enable/disable all warnings\n\
        \032    C/c enable/disable suspicious comment\n\
        \032    F/f enable/disable partially applied function\n\
        \032    M/m enable/disable overriden method\n\
        \032    P/p enable/disable partial match\n\
        \032    S/s enable/disable non-unit statement\n\
        \032    U/u enable/disable unused match case\n\
        \032    V/v enable/disable hidden instance variable\n\
        \032    X/x enable/disable all other warnings\n\
        \032    default setting is A (all warnings enabled)" ]
    (fun name -> raise(Arg.Bad("don't know what to do with " ^ name)))
    "ocamlbrowser :";
  Config.load_path :=
    List.rev_map ~f:(Misc.expand_directory Config.standard_library) !path
    @ [Config.standard_library];
  Warnings.parse_options false !Shell.warnings;
  Unix.putenv "TERM" "noterminal";
  begin
    try Searchid.start_env := Env.open_pers_signature "Pervasives" Env.initial
    with Env.Error _ -> ()
  end;
  
  Searchpos.view_defined_ref := Viewer.view_defined;
  Searchpos.editor_ref := Editor.f;

  let top = openTk ~clas:"OCamlBrowser" () in
  Jg_config.init ();

  (* bind top ~events:[`Destroy] ~action:(fun _ -> exit 0); *)
  at_exit Shell.kill_all;
  

  if !st then Viewer.st_viewer ~on:top ()
  else Viewer.f ~on:top ();

  while true do
    try
      Printexc.print mainLoop ()
    with Protocol.TkError _ -> ()
  done
