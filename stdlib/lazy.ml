(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Lazy]: deferred computations *)


(*
   WARNING: some purple magic is going on here.  Do not take this file
   as an example of how to program in OCaml.
*)


(* We make use of two special tags provided by the runtime:
   [lazy_tag] and [forward_tag].

   A value of type ['a Lazy.t] can be one of three things:
   1. A block of size 1 with tag [lazy_tag].  Its field is a closure of
      type [unit -> 'a] that computes the value.
   2. A block of size 1 with tag [forward_tag].  Its field is the value
      of type ['a] that was computed.
   3. Anything else except a float.  This has type ['a] and is the value
      that was computed.
   Exceptions are stored in format (1).
   The GC will magically change things from (2) to (3) according to its
   fancy.

   If OCaml was configured with the -flat-float-array option (which is
   currently the default), the following is also true:
   We cannot use representation (3) for a [float Lazy.t] because
   [caml_array_make] assumes that only a [float] value can have tag
   [Double_tag].

   We have to use the built-in type constructor [lazy_t] to
   let the compiler implement the special typing and compilation
   rules for the [lazy] keyword.
*)

type 'a t = 'a CamlinternalLazy.t

exception Undefined = CamlinternalLazy.Undefined
external make_forward : 'a -> 'a lazy_t = "caml_lazy_make_forward"
external force : 'a t -> 'a = "%lazy_force"

let force_val l = CamlinternalLazy.force_gen ~only_val:true l

let from_fun (f : unit -> 'arg) =
  let x = Obj.new_block Obj.lazy_tag 1 in
  Obj.set_field x 0 (Obj.repr f);
  (Obj.obj x : 'arg t)

let from_val (v : 'arg) =
  let t = Obj.tag (Obj.repr v) in
  if t = Obj.forward_tag || t = Obj.lazy_tag ||
     t = Obj.forcing_tag || t = Obj.double_tag then begin
    make_forward v
  end else begin
    (Obj.magic v : 'arg t)
  end

let is_val (l : 'arg t) = Obj.tag (Obj.repr l) <> Obj.lazy_tag

let map f x =
  lazy (f (force x))

let map_val f x =
  if is_val x
  then from_val (f (force x))
  else lazy (f (force x))



module Mutexed = struct
  (* we define these as primitives to avoid a dependency on Printexc *)
  type raw_backtrace
  external get_raw_backtrace:
    unit -> raw_backtrace = "caml_get_exception_raw_backtrace"
  external raise_with_backtrace: exn -> raw_backtrace -> 'a
    = "%raise_with_backtrace"

  (* micro-module to avoid a dependency on Mutex *)
  module Mutex = struct
    type t
    external create: unit -> t = "caml_ml_mutex_new"
    external lock: t -> unit = "caml_ml_mutex_lock"
    external unlock: t -> unit = "caml_ml_mutex_unlock"
  end

  type 'a state =
    | Thunk of (unit -> 'a)
    | Forcing of Mutex.t
    | Result of ('a, exn * raw_backtrace) result

  type 'a t = 'a state Atomic.t

  let from_val v = Atomic.make (Result (Ok v))
  let from_fun f = Atomic.make (Thunk f)

  let is_val th =
    match Atomic.get th with
    | Result (Ok _) -> true
    | _ -> false

  let rec force th =
    match Atomic.get th with
    | Result (Ok v) -> v
    | Result (Error (exn, bt)) ->
      raise_with_backtrace exn bt
    | Forcing mut ->
        (* Taking the lock may fail if our domain already owns
           it. This can happen if the thunk forces itself recursively,
           or if two fibers on the same domain try to force
           concurrently. We propagate the lower-level [Sys_error]
           exception in this case. *)
        Mutex.lock mut;
        Mutex.unlock mut;
        force th
    | Thunk f as thunk ->
        let mut = Mutex.create () in
        Mutex.lock mut;
        if not (Atomic.compare_and_set th thunk (Forcing mut)) then begin
          Mutex.unlock mut;
          force th (* retry *)
        end else begin
          begin match f () with
          | v -> Atomic.set th (Result (Ok v))
          | exception exn ->
            let bt = get_raw_backtrace () in
            Atomic.set th (Result (Error (exn, bt)))
          end;
          Mutex.unlock mut;
          force th
        end
end
