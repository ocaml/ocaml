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
open Printf

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
let max_history_size = ref 100
let history_file = ref ""

(* History storage - most recent first *)
let history = ref ([] : string list)
let history_position = ref 0

let trim_history_to_max () =
  let limit = max 0 !max_history_size in
  if limit = 0 then
    history := []
  else if List.length !history > limit then
    history := List.take limit !history

(* Current line editing state *)
let current_line_buffer = ref ""
let cursor_pos = ref 0
let current_prompt_str = ref ""
let line_editing_active = ref false

(* Escape sequence parsing state *)
type escape_state =
  | Normal
  | EscapeReceived
  | BracketReceived
  | OReceived
  | DeleteSeqReceived  (* After ESC[3 *)

let escape_parser_state = ref Normal

(* Completion state *)
type completion_callback = string -> int -> string list
let completion_callback = ref (None : completion_callback option)

(* Hints callback *)
type hints_callback = string -> string option
let hints_callback = ref (None : hints_callback option)

(* Hints cache - avoid recomputing on every keystroke *)
let hints_cache : (string, string option) Hashtbl.t = Hashtbl.create 32

(* Terminal state *)
let saved_termios = ref None

(* ANSI escape sequences *)
let clear_line = "\027[2K"
let move_cursor_to_start = "\r"
let move_cursor_left n = sprintf "\027[%dD" n

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

  (* Set history file path *)
  history_file := begin
    try
      Filename.concat (Sys.getenv "HOME") ".ocamldebug_history"
    with Not_found ->
      ".ocamldebug_history"
  end;

  (* Set max history size from Debugger_config if available *)
  (try
     max_history_size := !Debugger_config.history_size
   with _ -> ());

  (* Load history from file *)
  if Sys.file_exists !history_file then begin
    try
      let ic = open_in !history_file in
      try
        history := In_channel.fold_lines (fun acc line -> line :: acc) [] ic;
        close_in ic
      with e ->
        close_in ic;
        raise e
    with _ -> ()
  end;
  trim_history_to_max ();

  (* Register cleanup on exit *)
  at_exit (fun () -> restore_terminal ())

(* Save history to file *)
let save_history () =
  try
    let oc = open_out !history_file in
    try
      (* Save in reverse order (oldest first) for easier loading *)
      List.iter (fprintf oc "%s\n") (List.rev !history);
      close_out oc
    with e ->
      close_out oc;
      raise e
  with _ -> ()

(* Display current line with prompt *)
let display_line () =
  printf "%s%s" clear_line move_cursor_to_start;

  (* Only show prompt and hints if prompt is not empty *)
  let show_prompt = !current_prompt_str <> "" in
  if show_prompt then
    printf "%s" !current_prompt_str;

  printf "%s" !current_line_buffer;

  (* Show hint if available and track its length *)
  let hint_len =
    if show_prompt then
      match !hints_callback with
      | Some f ->
          (* Use cache to avoid recomputing hints *)
          let hint_opt =
            try Hashtbl.find hints_cache !current_line_buffer
            with Not_found ->
              let h = f !current_line_buffer in
              (* Limit cache size to avoid unbounded growth *)
              if Hashtbl.length hints_cache > 64 then
                Hashtbl.clear hints_cache;
              Hashtbl.add hints_cache !current_line_buffer h;
              h
          in
          (match hint_opt with
           | Some hint when hint <> "" ->
               printf "\027[90m%s\027[0m" hint; (* Gray color *)
               String.length hint
           | _ -> 0)
      | None -> 0
    else
      0
  in

  (* Move cursor to correct position, accounting for hint text *)
  let line_len = String.length !current_line_buffer in
  let chars_after_cursor = (line_len - !cursor_pos) + hint_len in
  if chars_after_cursor > 0 then
    printf "%s" (move_cursor_left chars_after_cursor);
  flush Stdlib.stdout

(* Handle history navigation *)
let history_prev () =
  if !history_position < List.length !history then begin
    let cmd = List.nth !history !history_position in
    history_position := !history_position + 1;
    current_line_buffer := cmd;
    cursor_pos := String.length cmd;
    true
  end else
    false

let history_next () =
  if !history_position > 0 then begin
    history_position := !history_position - 1;
    if !history_position = 0 then begin
      current_line_buffer := "";
      cursor_pos := 0
    end else begin
      let cmd = List.nth !history (!history_position - 1) in
      current_line_buffer := cmd;
      cursor_pos := String.length cmd
    end;
    true
  end else
    false

(* Insert character at cursor position *)
let insert_char c =
  let len = String.length !current_line_buffer in
  let before = String.sub !current_line_buffer 0 !cursor_pos in
  let after = String.sub !current_line_buffer !cursor_pos (len - !cursor_pos) in
  current_line_buffer := before ^ (String.make 1 c) ^ after;
  cursor_pos := !cursor_pos + 1

(* Delete character before cursor *)
let delete_char_before () =
  if !cursor_pos > 0 then begin
    let len = String.length !current_line_buffer in
    let before = String.sub !current_line_buffer 0 (!cursor_pos - 1) in
    let after = String.sub !current_line_buffer !cursor_pos (len - !cursor_pos) in
    current_line_buffer := before ^ after;
    cursor_pos := !cursor_pos - 1
  end

(* Delete character at cursor *)
let delete_char_at () =
  let len = String.length !current_line_buffer in
  if !cursor_pos < len then begin
    let before = String.sub !current_line_buffer 0 !cursor_pos in
    let after = String.sub !current_line_buffer (!cursor_pos + 1) (len - !cursor_pos - 1) in
    current_line_buffer := before ^ after
  end

(* Move cursor *)
let move_left () =
  if !cursor_pos > 0 then cursor_pos := !cursor_pos - 1

let move_right () =
  if !cursor_pos < String.length !current_line_buffer then
    cursor_pos := !cursor_pos + 1

let move_home () =
  cursor_pos := 0

let move_end () =
  cursor_pos := String.length !current_line_buffer

(* Kill line from cursor to end *)
let kill_line () =
  current_line_buffer := String.sub !current_line_buffer 0 !cursor_pos

let trigger_terminal_signal signal =
  printf "\n";
  flush Stdlib.stdout;
  line_editing_active := false;
  current_line_buffer := "";
  restore_terminal ();
  (try Unix.kill (Unix.getpid ()) signal with _ -> ());
  SignalRaised signal

(* Add command to history *)
let add_to_history line =
  if line <> "" then begin
    (* Remove duplicates - if this line is already in history, remove old occurrence *)
    history := line :: (List.filter (fun h -> h <> line) !history);

    trim_history_to_max ()
  end

let set_max_history_size size =
  max_history_size := max 0 size;
  trim_history_to_max ()

(* Handle tab completion *)
let handle_completion () =
  match !completion_callback with
  | None -> None
  | Some f ->
      let completions = f !current_line_buffer !cursor_pos in
      match completions with
      | [] -> None
      | [single] ->
          (* Single completion - replace the current word *)
          let rec find_word_start pos =
            if pos = 0 then 0
            else
              let c = !current_line_buffer.[pos - 1] in
              if c = ' ' || c = '\t' then pos
              else find_word_start (pos - 1)
          in
          let rec find_word_end pos =
            if pos >= String.length !current_line_buffer then pos
            else
              let c = !current_line_buffer.[pos] in
              if c = ' ' || c = '\t' then pos
              else find_word_end (pos + 1)
          in
          let word_start = find_word_start !cursor_pos in
          let word_end = find_word_end !cursor_pos in
          let prefix = String.sub !current_line_buffer 0 word_start in
          let suffix = String.sub !current_line_buffer word_end
                         (String.length !current_line_buffer - word_end) in
          current_line_buffer := prefix ^ single ^ suffix;
          cursor_pos := word_start + String.length single;
          display_line ();
          None
      | multiple ->
          Some multiple

let show_completion_choices choices =
  (* Print completions without leaving raw mode - keeps editor active *)
  printf "\n";
  List.iter (fun c -> printf "%s  " c) choices;
  printf "\n";
  flush Stdlib.stdout;

  (* Redisplay the current line - terminal is still in raw mode *)
  if !line_editing_active then
    display_line ()

(* Set callbacks *)
let set_completion_callback callback =
  completion_callback := callback

let set_hints_callback callback =
  hints_callback := callback

(* Start line editing for a new line *)
let start_line prompt =
  if not (!enabled && !is_a_tty) then
    false
  else begin
    (* Initialize state for new line *)
    current_line_buffer := "";
    cursor_pos := 0;
    current_prompt_str := prompt;
    history_position := 0;
    escape_parser_state := Normal;

    (* Setup terminal - returns false if failed *)
    if not (setup_terminal ()) then
      false
    else begin
      line_editing_active := true;

      (* Display initial prompt *)
      display_line ();

      true
    end
  end

let current_line () = !current_line_buffer

(* Process a single character - main state machine *)
let process_char c =
  if not !line_editing_active then
    NeedMore  (* Should not be called *)
  else
    try
      match !escape_parser_state with
      | EscapeReceived ->
          (* Second character of escape sequence *)
          (match c with
           | '[' ->
               escape_parser_state := BracketReceived;
               NeedMore
           | 'O' ->
               escape_parser_state := OReceived;
               NeedMore
           | _ ->
               escape_parser_state := Normal;
               NeedMore)

      | BracketReceived ->
          (* Third character after ESC[ *)
          (match c with
           | 'A' -> (* Up arrow *)
               escape_parser_state := Normal;
               if history_prev () then display_line ();
               NeedMore
           | 'B' -> (* Down arrow *)
               escape_parser_state := Normal;
               if history_next () then display_line ();
               NeedMore
           | 'C' -> (* Right arrow *)
               escape_parser_state := Normal;
               move_right ();
               display_line ();
               NeedMore
           | 'D' -> (* Left arrow *)
               escape_parser_state := Normal;
               move_left ();
               display_line ();
               NeedMore
           | 'H' -> (* Home *)
               escape_parser_state := Normal;
               move_home ();
               display_line ();
               NeedMore
           | 'F' -> (* End *)
               escape_parser_state := Normal;
               move_end ();
               display_line ();
               NeedMore
           | '3' -> (* Possible Delete *)
               escape_parser_state := DeleteSeqReceived;
               NeedMore
           | _ ->
               escape_parser_state := Normal;
               NeedMore)

      | DeleteSeqReceived ->
          (* Fourth character after ESC[3 *)
          escape_parser_state := Normal;
          if c = '~' then begin
            delete_char_at ();
            display_line ()
          end;
          NeedMore

      | OReceived ->
          (* Third character after ESC O *)
          escape_parser_state := Normal;
          (match c with
           | 'H' -> (* Home *)
               move_home ();
               display_line ();
               NeedMore
           | 'F' -> (* End *)
               move_end ();
               display_line ();
               NeedMore
           | _ ->
               NeedMore)

      | Normal ->
          (* Normal character processing *)
          match c with
          | '\n' | '\r' ->
              printf "\n";
              flush Stdlib.stdout;
              line_editing_active := false;
              restore_terminal ();
              LineComplete !current_line_buffer

          | '\004' -> (* Ctrl+D *)
              if !current_line_buffer = "" then begin
                printf "\n";
                flush Stdlib.stdout;
                line_editing_active := false;
                restore_terminal ();
                EndOfFile
              end else begin
                delete_char_at ();
                display_line ();
                NeedMore
              end

          | '\027' -> (* ESC *)
              escape_parser_state := EscapeReceived;
              NeedMore

          | '\t' -> (* Tab *)
              (match handle_completion () with
               | None -> NeedMore
               | Some choices -> ShowCompletions choices)

          | '\127' | '\008' -> (* Backspace/Delete *)
              delete_char_before ();
              display_line ();
              NeedMore

          | '\001' -> (* Ctrl+A *)
              move_home ();
              display_line ();
              NeedMore

          | '\005' -> (* Ctrl+E *)
              move_end ();
              display_line ();
              NeedMore

          | '\x0B' -> (* Ctrl+K - Kill line *)
              kill_line ();
              display_line ();
              NeedMore

          | '\026' -> (* Ctrl+Z -> SIGTSTP *)
              trigger_terminal_signal Sys.sigtstp

          | '\028' -> (* Ctrl+\ -> SIGQUIT *)
              trigger_terminal_signal Sys.sigquit

          | '\003' -> (* Ctrl+C *)
              printf "\n";
              flush Stdlib.stdout;
              line_editing_active := false;
              current_line_buffer := "";
              restore_terminal ();
              LineCancelled

          | c when c >= ' ' ->
              insert_char c;
              display_line ();
              NeedMore

          | _ ->
              NeedMore
    with e ->
      (* On any exception, restore terminal and re-raise *)
      line_editing_active := false;
      restore_terminal ();
      raise e
