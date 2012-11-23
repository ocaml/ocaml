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

type t = Value.t

exception Bad_abbreviation_code of int

let of_int i =
  if i < 1 then raise (Bad_abbreviation_code i);
  Value.as_uleb128 i

let null () =
  Value.as_uleb128 0

let emit t ~emitter =
  Value.emit t ~emitter

let size t =
  Value.size t
