(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The "trace" facility *)

open Format
open Misc
open Longident
open Typedtree
open Printval
open Toploop

type traced_function =
  { path: Path.t;
    closure: Obj.t;
    initial_closure: Obj.t;
    instrumented_fun: Obj.t }

let traced_functions = ref ([] : traced_function list)

(* Check if a function is already traced *)

let is_traced clos =
  let rec is_traced = function
      [] -> None
    | tf :: rem -> if tf.closure == clos then Some tf.path else is_traced rem
  in is_traced !traced_functions

(* Make a copy of a closure *)

let copy_closure cls =
  let sz = Obj.size cls in
  let nw = Obj.new_block 250 sz in
  for i = 0 to sz - 1 do Obj.set_field nw i (Obj.field cls i) done;
  nw

(* Overwrite the code field of a closure by another *)

let overwrite_closure dst src =
  Obj.set_field dst 0 (Obj.field src 0)

(* Return a closure that performs as the given closure, but also
   traces its execution. *)

let rec instrument_closure name clos_typ =
  match (Ctype.repr clos_typ).desc with
    Tarrow(t1, t2) ->
      let starred_name =
        match name with
          Lident s -> Lident(s ^ "*")
        | Ldot(lid, s) -> Ldot(lid, s ^ "*")
        | Lapply(l1, l2) -> fatal_error "Trace.instrument_closure" in
      let trace_res = instrument_closure starred_name t2 in
      (fun clos_val ->
        Obj.repr(fun arg ->
          open_hovbox 2;
          Printtyp.longident name; print_string " <--"; print_space();
          print_value !toplevel_env arg t1; close_box(); print_newline();
          try
            let res = (Obj.magic clos_val : Obj.t -> Obj.t)(arg) in
            open_hovbox 2;
            Printtyp.longident name; print_string " -->"; print_space();
            print_value !toplevel_env res t2; close_box(); print_newline();
            trace_res res
          with exn ->
            open_hovbox 2;
            Printtyp.longident name; print_string " raises"; print_space();
            print_exception (Obj.repr exn); close_box(); print_newline();
            raise exn))
  | _ ->
      (fun v -> v)

(* Given the address of a closure, find its instrumented version
   and call it *)

let rec find_traced_closure clos = function
    [] ->
      fatal_error "Trace.find_traced_closure"
  | f :: rem ->
      if f.closure == clos then f else find_traced_closure clos rem

let print_trace clos arg =
  (Obj.magic
     (find_traced_closure clos !traced_functions).instrumented_fun :
     Obj.t -> Obj.t)
  arg

