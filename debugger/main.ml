(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Primitives
open Misc
open Input_handling
open Command_line_interpreter
open Debugger_config
open Checkpoints
open Time_travel
open Parameters
open Program_management
open Frames
open Show_information


let line_buffer = Lexing.from_function read_user_input

let rec loop () =
  line_loop line_buffer;
  if !loaded & (not (yes_or_no "The program is running. Quit anyway")) then
    loop ()

let rec protect cont =
  try
    cont ()
  with
    End_of_file ->
      protect (function () ->
        forget_process
          !current_checkpoint.c_fd
          !current_checkpoint.c_pid;
        flush stdout;
        stop_user_input ();
        loop ())
  | Toplevel ->
      protect (function () ->
        flush stdout;
        stop_user_input ();
        loop ())
  | Sys.Break ->
      protect (function () ->
        print_endline "Interrupted.";
        Exec.protected (function () ->
          flush stdout;
          stop_user_input ();
          if !loaded then begin
            try_select_frame 0;
            show_current_event ()
          end);
        loop ())
  | Current_checkpoint_lost ->
      protect (function () ->
        print_endline "Trying to recover...";
        flush stdout;
        stop_user_input ();
        recover ();
        try_select_frame 0;
        show_current_event ();
        loop ())
  | x ->
      kill_program ();
      raise x

let toplevel_loop () = protect loop

let anonymous s =
  if !program_name = ""
  then program_name := s
  else arguments := Printf.sprintf "%s '%s'" !arguments s
let add_include d =
  default_load_path := d :: !default_load_path
let set_socket s =
  socket_name := s
let set_checkpoints n =
  checkpoint_max_count := n
let set_directory dir =
  Sys.chdir dir
let set_emacs () =
  emacs := true

let main () =
  try
    socket_name := "/tmp/camldebug" ^ (string_of_int (Unix.getpid ()));
    Arg.parse
      ["-I", Arg.String add_include,
          "<dir>  Add <dir> to the list of include directories";
       "-s", Arg.String set_socket,
          "<filename>  Set the name of the communication socket";
       "-c", Arg.Int set_checkpoints,
          "<count>  Set max number of checkpoints kept";
       "-cd", Arg.String set_directory,
          "<dir>  Change working directory";
       "-emacs", Arg.Unit set_emacs,
          "For running the debugger under emacs"]
      anonymous
      "";
    current_prompt := debugger_prompt;
    print_string "\tObjective Caml Debugger version ";
    print_string Config.version;
    print_newline(); print_newline();
    Config.load_path := !default_load_path;
    toplevel_loop ();                   (* Toplevel. *)
    kill_program ();
    exit 0
  with Toplevel ->
    exit 2

let _ =
  Printexc.catch (Unix.handle_unix_error main) ()
