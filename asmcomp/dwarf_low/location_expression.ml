(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Mark Shinwell, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright and licence information to be added.                     *)
(*                                                                     *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

open Std_internal

type t = Simple_location_expression.t  (* will do for the moment *)

let in_register ~reg_number =
  (* CR mshinwell: this remapping needs to be somewhere else *)
  let reg_number =
    match reg_number with
    | 0 -> 0     (* %rax *)
    | 1 -> 3     (* %rbx *)
    | 2 -> 5     (* %rdi *)
    | 3 -> 4     (* %rsi *)
    | 4 -> 1     (* %rdx *)
    | 5 -> 2     (* %rcx *)
    | 6 -> 8     (* %r8 *)
    | 7 -> 9     (* %r9 *)
    | 8 -> 12    (* %r12 *)
    | 9 -> 13    (* %r13 *)
    | 10 -> 6    (* %rbp *)
    | 11 -> 10   (* %r10 *)
    | 12 -> 11   (* %r11 *)
    | r -> r     (* CR mshinwell: fixme *)
  in
  Simple_location_expression.in_register ~reg_number

let size t =
  Simple_location_expression.size t

let emit t ~emitter =
  Simple_location_expression.emit t ~emitter
