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

(* Register name operators only for the moment.  There are other
   cases (p23-24) *)
type t = Operator.t

let in_register ~reg_number =
  Operator.register ~reg_number ~offset:0

let size t =
  Operator.size t

let emit t ~emitter =
  Operator.emit t ~emitter
