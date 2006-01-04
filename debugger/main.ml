(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Primitives
open Misc
open Input_handling
open Command_line
open Debugger_config
open Checkpoints
open Time_travel
open Parameters
open Program_management
open Frames
open Show_information
open Format

let line_buffer = Lexing.from_function read_user_input

let rec loop ppf =
  line_loop ppf line_buffer;
  if !loaded && (not (yes_or_no "The program is running. Quit anyway")) then
    loop ppf

let rec protect ppf loop =
  try
    loop ppf
  with
  | End_of_file ->
      protect ppf (function ppf ->
        forget_process
          !current_checkpoint.c_fd
          !current_checkpoint.c_pid;
        pp_print_flush ppf ();
        stop_user_input ();
        loop ppf)
  | Toplevel ->
      protect ppf (function ppf ->
        pp_print_flush ppf ();
        stop_user_input ();
        loop ppf)
  | Sys.Break ->
      protect ppf (function ppf ->
        fprintf ppf "Interrupted.@.";
        Exec.protect (function () ->
          stop_user_input ();
          if !loaded then begin
            try_select_frame 0;
            show_current_event ppf;
          end);
        loop ppf)
  | Current_checkpoint_lost ->
      protect ppf (function ppf ->
        fprintf ppf "Trying to recover...@.";
        stop_user_input ();
        recover ();
        try_select_frame 0;
        show_current_event ppf;
        loop ppf)
  | x ->
      kill_program ();
      raise x

let toplevel_loop () = protect Format.std_formatter loop

(* Parsing of command-line arguments *)

exception Found_program_name

let anonymous s =
  program_name := Unix_tools.make_absolute s; raise Found_program_name
let add_include d =
  default_load_path :=
    Misc.expand_directory Config.standard_library d :: !default_load_path
let set_socket s =
  socket_name := s
let set_checkpoints n =
  checkpoint_max_count := n
let set_directory dir =
  Sys.chdir dir
let print_version () =
  printf "The Objective Caml debugger, version %s@." Sys.ocaml_version;
  exit 0;
;;

let speclist = [
   "-c", Arg.Int set_checkpoints,
      "<count>  Set max number of checkpoints kept";
   "-cd", Arg.String set_directory,
      "<dir>  Change working directory";
   "-emacs", Arg.Set emacs,
      "For running the debugger under emacs";
   "-I", Arg.String add_include,
      "<dir>  Add <dir> to the list of include directories";
   "-s", Arg.String set_socket,
      "<filename>  Set the name of the communication socket";
   "-version", Arg.Unit print_version,
      " Print version and exit";
   ]

let main () =
  try
    socket_name := Filename.concat Filename.temp_dir_name
                          ("camldebug" ^ (string_of_int (Unix.getpid ())));
    begin try
      Arg.parse speclist anonymous "";
      Arg.usage speclist
        "No program name specified\n\
         Usage: ocamldebug [options] <program> [arguments]\n\
         Options are:";
      exit 2
    with Found_program_name ->
      for j = !Arg.current + 1 to Array.length Sys.argv - 1 do
        arguments := !arguments ^ " " ^ (Filename.quote Sys.argv.(j))
      done
    end;
    current_prompt := debugger_prompt;
    printf "\tObjective Caml Debugger version %s@.@." Config.version;
    Config.load_path := !default_load_path;
    toplevel_loop ();                   (* Toplevel. *)
    kill_program ();
    exit 0
  with
    Toplevel ->
      exit 2
  | Env.Error e ->
      eprintf "Debugger [version %s] environment error:@ @[@;" Config.version;
      Env.report_error err_formatter e;
      eprintf "@]@.";
      exit 2

let _ =
  Printexc.catch (Unix.handle_unix_error main) ()
