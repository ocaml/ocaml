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

(* Low-level communication with the debuggee *)

open Primitives

(* The current connection with the debuggee *)

let conn = ref Primitives.std_io

let set_current_connection io_chan =
  conn := io_chan

(* Modify the program code *)

let set_event pos =
  output_char !conn.io_out 'e';
  output_binary_int !conn.io_out pos;
  flush !conn.io_out

let set_breakpoint pos =
  output_char !conn.io_out 'B';
  output_binary_int !conn.io_out pos;
  flush !conn.io_out

let reset_instr pos =
  output_char !conn.io_out 'i';
  output_binary_int !conn.io_out pos;
  flush !conn.io_out

(* Basic commands for flow control *)

type execution_summary =
    Event
  | Breakpoint
  | Exited
  | Trap_barrier
  | Uncaught_exc

type report = {
  rep_type : execution_summary;
  rep_event_count : int;
  rep_stack_pointer : int;
  rep_program_pointer : int
}

type checkpoint_report =
    Checkpoint_done of int
  | Checkpoint_failed

(* Run the debuggee for N events *)

let do_go n =
  output_char !conn.io_out 'g';
  output_binary_int !conn.io_out n;
  flush !conn.io_out;
  let summary =
    match input_char !conn.io_in with
      'e' -> Event
    | 'b' -> Breakpoint
    | 'x' -> Exited
    | 's' -> Trap_barrier
    | 'u' -> Uncaught_exc
    |  _  -> Misc.fatal_error "Debugcom.do_go" in
  let event_counter = input_binary_int !conn.io_in in
  let stack_pos = input_binary_int !conn.io_in in
  let pc = input_binary_int !conn.io_in in
  { rep_type = summary;
    rep_event_count = event_counter;
    rep_stack_pointer = stack_pos;
    rep_program_pointer = pc }

(* Perform a checkpoint *)

let do_checkpoint () =
  output_char !conn.io_out 'c';
  flush !conn.io_out;
  let pid = input_binary_int !conn.io_in in
  if pid = -1 then Checkpoint_failed else Checkpoint_done pid

(* Kill the given process. *)
let stop chan =
  try
    try
      output_char chan.io_out 's';
      flush chan.io_out
    with
      Sys_error _ -> ()
  with
    End_of_file -> ()

(* Ask a process to wait for its child which has been killed. *)
(* (so as to eliminate zombies). *)
let wait_child chan =
  try
    try
      output_char chan.io_out 'w';
      flush chan.io_out
    with
      Sys_error _ -> ()
  with
    End_of_file -> ()

(* Move to initial frame (that of current function). *)
(* Return stack position and current pc *)

let initial_frame () =
  output_char !conn.io_out '0';
  flush !conn.io_out;
  let stack_pos = input_binary_int !conn.io_in in
  let pc = input_binary_int !conn.io_in in
  (stack_pos, pc)

(* Move up one frame *)
(* Return stack position and current pc.
   If there's no frame above, return (-1, 0). *)

let up_frame stacksize =
  output_char !conn.io_out 'U';
  output_binary_int !conn.io_out stacksize;
  flush !conn.io_out;
  let stack_pos = input_binary_int !conn.io_in in
  let pc = if stack_pos = -1 then 0 else input_binary_int !conn.io_in in
  (stack_pos, pc)

(* Get and set the current frame position *)

let get_frame () =
  output_char !conn.io_out 'f';
  flush !conn.io_out;
  let stack_pos = input_binary_int !conn.io_in in
  let pc = input_binary_int !conn.io_in in
  (stack_pos, pc)

let set_frame stack_pos =
  output_char !conn.io_out 'S';
  output_binary_int !conn.io_out stack_pos;
  flush !conn.io_out

(* Set the trap barrier to given stack position. *)

let set_trap_barrier pos =
  output_char !conn.io_out 'b';
  output_binary_int !conn.io_out pos;
  flush !conn.io_out

(* Handling of remote values *)

type remote_value = string

let value_size = if 1 lsl 31 = 0 then 4 else 8

let input_remote_value ic =
  let v = String.create value_size in
  really_input ic v 0 value_size; v

let output_remote_value ic v =
  output ic v 0 value_size

let remote_value_is_int v =
  not(Obj.is_block(Array.unsafe_get (Obj.magic v : Obj.t array) 0))

let int_value v =
  Array.unsafe_get (Obj.magic v : int array) 0

let value_int n =
  let v = String.create value_size in
  Array.unsafe_set (Obj.magic v : int array) 0 n;
  v

let get_local pos =
  output_char !conn.io_out 'L';
  output_binary_int !conn.io_out pos;
  flush !conn.io_out;
  input_remote_value !conn.io_in

let get_environment pos =
  output_char !conn.io_out 'E';
  output_binary_int !conn.io_out pos;
  flush !conn.io_out;
  input_remote_value !conn.io_in

let get_global pos =
  output_char !conn.io_out 'G';
  output_binary_int !conn.io_out pos;
  flush !conn.io_out;
  input_remote_value !conn.io_in

let get_accu () =
  output_char !conn.io_out 'A';
  flush !conn.io_out;
  input_remote_value !conn.io_in

let get_obj v =
  output_char !conn.io_out 'O';
  output_remote_value !conn.io_out v;
  flush !conn.io_out;
  let header = input_binary_int !conn.io_in in
  let size = header lsr 10 in
  let fields = Array.create size "" in
  for i = 0 to size - 1 do fields.(i) <- input_remote_value !conn.io_in done;
  (header land 0xFF, fields)

let get_header v =
  output_char !conn.io_out 'H';
  output_remote_value !conn.io_out v;
  flush !conn.io_out;
  let header = input_binary_int !conn.io_in in
  (header land 0xFF, header lsr 10)

let get_field v n =
  output_char !conn.io_out 'F';
  output_remote_value !conn.io_out v;
  output_binary_int !conn.io_out n;
  flush !conn.io_out;
  input_remote_value !conn.io_in

let marshal_obj v =
  output_char !conn.io_out 'M';
  output_remote_value !conn.io_out v;
  flush !conn.io_out;
  input_value !conn.io_in

let get_closure_code v =
  output_char !conn.io_out 'C';
  output_remote_value !conn.io_out v;
  flush !conn.io_out;
  input_binary_int !conn.io_in

