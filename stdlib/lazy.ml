(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Lazy]: deferred computations *)


(*
   WARNING: some purple magic is going on here.  Do not take this file
   as an example of how to program in Objective Caml.
*)


(* We make use of two special tags provided by the runtime:
   [lazy_tag] and [forward_tag].

   A value of type ['a Lazy.t] can be one of three things:
   1. A block of size 1 with tag [lazy_tag].  Its field is a closure of
      type [unit -> 'a] that computes the value.
   2. A block of size 1 with tag [forward_tag].  Its field is the value
      of type ['a] that was computed.
   3. Anything else.  This has type ['a] and is the value that was computed.
   Exceptions are stored in format (1).
   The GC will magically change things from (2) to (3) according to its
   fancy.

   We have to use the built-in type constructor [lazy_t] to
   let the compiler implement the special typing and compilation
   rules for the [lazy] keyword.
*)

type 'a t = 'a lazy_t;;
exception Undefined;;

let raise_undefined = Obj.repr (fun () -> raise Undefined);;

let force (l : 'arg t) =
  let x = Obj.repr l in
  if Obj.is_int x then (Obj.obj x : 'arg)
  else if Obj.tag x = Obj.forward_tag then (Obj.obj (Obj.field x 0) : 'arg)
  else if Obj.tag x <> Obj.lazy_tag then (Obj.obj x : 'arg)
  else begin
    let closure = (Obj.obj (Obj.field x 0) : unit -> 'arg) in
    Obj.set_field x 0 raise_undefined;
    try
      let result = closure () in
      Obj.set_field x 0 (Obj.repr result);  (* do set_field BEFORE set_tag *)
      Obj.set_tag x (Obj.forward_tag);
      result
    with e ->
      Obj.set_field x 0 (Obj.repr (fun () -> raise e));
      raise e;
  end
;;

let force_val (l : 'arg t) =
  let x = Obj.repr l in
  if Obj.is_int x then (Obj.obj x : 'arg)
  else if Obj.tag x = Obj.forward_tag then (Obj.obj (Obj.field x 0) : 'arg)
  else if Obj.tag x <> Obj.lazy_tag then (Obj.obj x : 'arg)
  else begin
    let closure = (Obj.obj (Obj.field x 0) : unit -> 'arg) in
    Obj.set_field x 0 raise_undefined;
    let result = closure () in
    Obj.set_field x 0 (Obj.repr result);  (* do set_field BEFORE set_tag *)
    Obj.set_tag x (Obj.forward_tag);
    result
  end
;;

let lazy_from_fun (f : unit -> 'arg) =
  let x = Obj.new_block Obj.lazy_tag 1 in
  Obj.set_field x 0 (Obj.repr f);
  (Obj.obj x : 'arg t)
;;

let lazy_from_val (v : 'arg) = (Obj.magic v : 'arg t);;

let lazy_is_val (l : 'arg t) =
  let x = Obj.repr l in
  Obj.is_int x || Obj.tag x <> Obj.lazy_tag
;;
