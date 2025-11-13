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

(** Command history management with line editing capabilities *)

(** Result of processing a single character during line editing *)
type line_result =
  | NeedMore         (** Need more characters to complete the line *)
  | LineComplete of string  (** Line is complete, return it *)
  | LineCancelled    (** Line was cancelled (Ctrl+C) *)
  | EndOfFile        (** EOF (Ctrl+D on empty line) *)
  | SignalRaised of int  (** Terminal-generated signal to re-emit (SIGTSTP/SIGQUIT) *)
  | ShowCompletions of string list  (** Display completion matches, keep editing *)

(** {1 Configuration and initialization} *)

val enabled : bool ref
(** Enable/disable line editing features. When disabled, falls back to standard input. *)

val set_enabled : bool -> unit
(** Enable or disable line editing *)

val is_tty : unit -> bool
(** Check if stdin is a TTY *)

val init : unit -> unit
(** Initialize the command history system.
    Detects TTY, sets up terminal state. *)

val setup_terminal : unit -> bool
(** Switch terminal to raw mode for character-by-character input.
    Returns true on success. *)

val restore_terminal : unit -> unit
(** Restore terminal to original state. *)

val save_history : unit -> unit
(** Save command history to ~/.ocamldebug_history file. *)

(** {1 Line editing} *)

val start_line : string -> bool
(** Start line editing with the given prompt.
    Returns true if line editing is active, false if fallback to lexer. *)

val process_char : char -> line_result
(** Process a single character during line editing.
    Returns the result of processing (NeedMore, LineComplete, etc.). *)

val current_line : unit -> string
(** Get the current line being edited. *)

(** {1 History management} *)

val add_to_history : string -> unit
(** Add a command to the history (automatically removes duplicates). *)

val set_max_history_size : int -> unit
(** Set maximum history size. *)

(** {1 Completion and hints} *)

type completion_callback = string -> int -> string list
(** Completion callback: takes current line and cursor position,
    returns list of possible completions. *)

type hints_callback = string -> string option
(** Hints callback: takes current line, returns optional hint text
    to display in gray after the cursor. *)

val set_completion_callback : completion_callback option -> unit
(** Set the completion callback for Tab completion. *)

val set_hints_callback : hints_callback option -> unit
(** Set the hints callback for inline suggestions. *)

val show_completion_choices : string list -> unit
(** Display completion choices to the user. *)
