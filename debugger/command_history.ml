(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Joel Reymont                                      *)
(*                                                                        *)
(*   Copyright 2025 Joel Reymont                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Command history management with line editing capabilities

    Integrated with Input_handling event loop for non-blocking operation *)

open Unix

(** Result of processing a character *)
type line_result =
  | NeedMore         (** Need more characters to complete the line *)
  | LineComplete of string  (** Line is complete, return it *)
  | LineCancelled    (** Line was cancelled (Ctrl+C) *)
  | EndOfFile        (** EOF (Ctrl+D on empty line) *)
  | SignalRaised of int  (** Terminal-generated signal to re-emit (SIGTSTP/SIGQUIT) *)
  | ShowCompletions of string list  (** Display completion matches, keep editing *)

(* Configuration *)
let enabled = ref true
let is_a_tty = ref false

(* Terminal state *)
let saved_termios = ref None

(* Initialize terminal in raw mode for line editing *)
let setup_terminal () =
  if !is_a_tty && Sys.os_type = "Unix" then begin
    try
      let termios = tcgetattr stdin in
      saved_termios := Some termios;
      let raw_termios = { termios with
        c_icanon = false;
        c_echo = false;
        c_isig = false;
        c_vmin = 1;
        c_vtime = 0;
      } in
      tcsetattr stdin TCSANOW raw_termios;
      true
    with _ -> false
  end else
    false

(* Restore terminal to original state *)
let restore_terminal () =
  if Sys.os_type = "Unix" then
    match !saved_termios with
    | Some termios ->
        (try tcsetattr stdin TCSANOW termios with _ -> ());
        saved_termios := None
    | None -> ()

(* Detect if we're on a TTY *)
let is_tty () = !is_a_tty

(* Enable/disable line editing *)
let set_enabled value =
  enabled := value

(* Initialize the history system *)
let init () =
  (* Check if stdin is a TTY *)
  is_a_tty := (try isatty stdin with _ -> false);

  (* Register cleanup on exit *)
  at_exit (fun () -> restore_terminal ())

(* Save history (stub - will be implemented in history persistence feature) *)
let save_history () = ()
