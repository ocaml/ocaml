(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Low-level communication with the debuggee *)

open Int64ops
open Primitives

(* The current connection with the debuggee *)

let conn = ref Primitives.std_io

(* Set which process the debugger follows on fork. *)

type follow_fork_mode =
    Fork_child
  | Fork_parent

let fork_mode = ref Fork_parent

let update_follow_fork_mode () =
  let a = match !fork_mode with Fork_child -> 0 | Fork_parent -> 1 in
  output_char !conn.io_out 'K';
  output_binary_int !conn.io_out a

(* Set the current connection, and update the fork mode in case it has
 * changed. *)

let set_current_connection io_chan =
  conn := io_chan;
  update_follow_fork_mode ()

(* Modify the program code *)

type pc =
  { frag : int;
    pos : int; }

let set_event {frag; pos} =
  output_char !conn.io_out 'e';
  output_binary_int !conn.io_out frag;
  output_binary_int !conn.io_out pos

let set_breakpoint {frag; pos} =
  output_char !conn.io_out 'B';
  output_binary_int !conn.io_out frag;
  output_binary_int !conn.io_out pos

let reset_instr {frag; pos} =
  output_char !conn.io_out 'i';
  output_binary_int !conn.io_out frag;
  output_binary_int !conn.io_out pos

(* Basic commands for flow control *)

type execution_summary =
    Event
  | Breakpoint
  | Exited
  | Trap_barrier
  | Uncaught_exc
  | Debug_info of Instruct.debug_event list array
  | Code_loaded of int
  | Code_unloaded of int

type report = {
  rep_type : execution_summary;
  rep_event_count : int64;
  rep_stack_pointer : int;
  rep_program_pointer : pc
}

type checkpoint_report =
    Checkpoint_done of int
  | Checkpoint_failed

(* Run the debuggee for N events *)

let do_go_smallint n =
  output_char !conn.io_out 'g';
  output_binary_int !conn.io_out n;
  flush !conn.io_out;
  Input_handling.execute_with_other_controller
    Input_handling.exit_main_loop
    !conn
    (function () ->
       Input_handling.main_loop ();
       let summary =
         match input_char !conn.io_in with
           'e' -> Event
         | 'b' -> Breakpoint
         | 'x' -> Exited
         | 's' -> Trap_barrier
         | 'u' -> Uncaught_exc
         | 'D' -> Debug_info (input_value !conn.io_in :
                                Instruct.debug_event list array)
         | 'L' -> Code_loaded (input_binary_int !conn.io_in)
         | 'U' -> Code_unloaded (input_binary_int !conn.io_in)
         |  c  -> Misc.fatal_error (Printf.sprintf "Debugcom.do_go %c" c)
       in
       let event_counter = input_binary_int !conn.io_in in
       let stack_pos = input_binary_int !conn.io_in in
       let frag = input_binary_int !conn.io_in in
       let pos = input_binary_int !conn.io_in in
       { rep_type = summary;
         rep_event_count = Int64.of_int event_counter;
         rep_stack_pointer = stack_pos;
         rep_program_pointer = {frag; pos} })

let rec do_go n =
  assert (n >= _0);
  if n > max_small_int then
    begin match do_go_smallint max_int with
    | { rep_type = Event } ->
      do_go (n -- max_small_int)
    | report ->
      { report with
        rep_event_count = report.rep_event_count ++ (n -- max_small_int) }
    end
  else
    do_go_smallint (Int64.to_int n)

(* Perform a checkpoint *)

let do_checkpoint () =
  match Sys.os_type with
    "Win32" -> failwith "do_checkpoint"
  | _ ->
      output_char !conn.io_out 'c';
      flush !conn.io_out;
      let pid = input_binary_int !conn.io_in in
      if pid = -1 then Checkpoint_failed else Checkpoint_done pid

(* Kill the given process. *)
let stop chan =
  try
    output_char chan.io_out 's';
    flush chan.io_out
  with
    Sys_error _ | End_of_file -> ()

(* Ask a process to wait for its child which has been killed. *)
(* (so as to eliminate zombies). *)
let wait_child chan =
  try
    output_char chan.io_out 'w'
  with
    Sys_error _ | End_of_file -> ()

(* Move to initial frame (that of current function). *)
(* Return stack position and current pc *)

let initial_frame () =
  output_char !conn.io_out '0';
  flush !conn.io_out;
  let stack_pos = input_binary_int !conn.io_in in
  let frag = input_binary_int !conn.io_in in
  let pos = input_binary_int !conn.io_in in
  (stack_pos, {frag; pos})

let set_initial_frame () =
  ignore(initial_frame ())

(* Move up one frame *)
(* Return stack position and current pc.
   If there's no frame above, return (-1, 0). *)

let up_frame stacksize =
  output_char !conn.io_out 'U';
  output_binary_int !conn.io_out stacksize;
  flush !conn.io_out;
  let stack_pos = input_binary_int !conn.io_in in
  let frag, pos =
    if stack_pos = -1
    then 0, 0
    else let frag = input_binary_int !conn.io_in in
         let pos = input_binary_int !conn.io_in in
         frag, pos
  in
  (stack_pos, { frag; pos })

(* Get and set the current frame position *)

let get_frame () =
  output_char !conn.io_out 'f';
  flush !conn.io_out;
  let stack_pos = input_binary_int !conn.io_in in
  let frag = input_binary_int !conn.io_in in
  let pos = input_binary_int !conn.io_in in
  (stack_pos, {frag; pos})

let set_frame stack_pos =
  output_char !conn.io_out 'S';
  output_binary_int !conn.io_out stack_pos

(* Set the trap barrier to given stack position. *)

let set_trap_barrier pos =
  output_char !conn.io_out 'b';
  output_binary_int !conn.io_out pos

(* Handling of remote values *)

let value_size = if 1 lsl 31 = 0 then 4 else 8

let input_remote_value ic =
  really_input_string ic value_size

let output_remote_value ic v =
  output_substring ic v 0 value_size

exception Marshalling_error

module Remote_value =
  struct
    type t = Remote of string | Local of Obj.t

    let repr x = Local (Obj.repr x)

    let obj = function
    | Local obj -> Obj.obj obj
    | Remote v ->
        output_char !conn.io_out 'M';
        output_remote_value !conn.io_out v;
        flush !conn.io_out;
        try
          input_value !conn.io_in
        with End_of_file | Failure _ ->
          raise Marshalling_error

    let is_block = function
    | Local obj -> Obj.is_block obj
    | Remote v -> Obj.is_block (Array.unsafe_get (Obj.magic v : Obj.t array) 0)

    let tag obj =
      if not (is_block obj) then Obj.int_tag
      else match obj with
      | Local obj -> Obj.tag obj
      | Remote v ->
          output_char !conn.io_out 'H';
          output_remote_value !conn.io_out v;
          flush !conn.io_out;
          let header = input_binary_int !conn.io_in in
          header land 0xFF

    let size = function
    | Local obj -> Obj.size obj
    | Remote v ->
        output_char !conn.io_out 'H';
        output_remote_value !conn.io_out v;
        flush !conn.io_out;
        let header = input_binary_int !conn.io_in in
        if header land 0xFF = Obj.double_array_tag && Sys.word_size = 32
        then header lsr 11
        else header lsr 10

    let field v n =
      match v with
      | Local obj -> Local(Obj.field obj n)
      | Remote v ->
          output_char !conn.io_out 'F';
          output_remote_value !conn.io_out v;
          output_binary_int !conn.io_out n;
          flush !conn.io_out;
          if input_byte !conn.io_in = 0 then
            Remote(input_remote_value !conn.io_in)
          else begin
            let buf = really_input_string !conn.io_in 8 in
            let floatbuf = float n (* force allocation of a new float *) in
            String.unsafe_blit buf 0 (Obj.magic floatbuf) 0 8;
            Local(Obj.repr floatbuf)
          end

    let double_field v n =
      match v with
      | Local obj -> Obj.double_field obj n
      | Remote v ->
          output_char !conn.io_out 'F';
          output_remote_value !conn.io_out v;
          output_binary_int !conn.io_out n;
          flush !conn.io_out;
          if input_byte !conn.io_in = 0 then
            raise Marshalling_error
          else begin
            let buf = really_input_string !conn.io_in 8 in
            let floatbuf = float n (* force allocation of a new float *) in
            String.unsafe_blit buf 0 (Obj.magic floatbuf) 0 8;
            floatbuf
          end

    let double_array_tag = Obj.double_array_tag

    let of_int n =
      Local(Obj.repr n)

    let local pos =
      output_char !conn.io_out 'L';
      output_binary_int !conn.io_out pos;
      flush !conn.io_out;
      Remote(input_remote_value !conn.io_in)

    let from_environment pos =
      output_char !conn.io_out 'E';
      output_binary_int !conn.io_out pos;
      flush !conn.io_out;
      Remote(input_remote_value !conn.io_in)

    let global pos =
      output_char !conn.io_out 'G';
      output_binary_int !conn.io_out pos;
      flush !conn.io_out;
      Remote(input_remote_value !conn.io_in)

    let accu () =
      output_char !conn.io_out 'A';
      flush !conn.io_out;
      Remote(input_remote_value !conn.io_in)

    let closure_code = function
    | Local _ -> assert false
    | Remote v ->
        output_char !conn.io_out 'C';
        output_remote_value !conn.io_out v;
        flush !conn.io_out;
        let frag = input_binary_int !conn.io_in in
        let pos = input_binary_int !conn.io_in in
        {frag;pos}

    let same rv1 rv2 =
      match (rv1, rv2) with
        (Local obj1, Local obj2) -> obj1 == obj2
      | (Remote v1, Remote v2) -> v1 = v2
           (* string equality -> equality of remote pointers *)
      | (_, _) -> false

    let pointer rv =
      match rv with
      | Remote v ->
        let bytes = ref [] in
        String.iter (fun c -> bytes := c :: !bytes) v;
        let obytes = if Sys.big_endian then List.rev !bytes else !bytes in
        let to_hex c = Printf.sprintf "%02x" (Char.code c) in
        String.concat "" (List.map to_hex obytes)
      | Local _ -> ""

  end
