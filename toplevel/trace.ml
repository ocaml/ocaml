(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The "trace" facility *)

open Format
open Misc
open Longident
open Types
open Printval
open Toploop

type codeptr = Obj.t

type traced_function =
  { path: Path.t;                       (* Name under which it is traced *)
    closure: Obj.t;                     (* Its function closure (patched) *)
    actual_code: codeptr;               (* Its original code pointer *)
    instrumented_fun: codeptr -> Obj.t -> Obj.t -> Obj.t }
                                        (* Printing function *)

let traced_functions = ref ([] : traced_function list)

(* Check if a function is already traced *)

let is_traced clos =
  let rec is_traced = function
      [] -> None
    | tf :: rem -> if tf.closure == clos then Some tf.path else is_traced rem
  in is_traced !traced_functions

(* Get or overwrite the code pointer of a closure *)

let get_code_pointer cls = Obj.field cls 0

let set_code_pointer cls ptr = Obj.set_field cls 0 ptr

(* Call a traced function (use old code pointer, but new closure as
   environment so that recursive calls are also traced).
   It is necessary to wrap Meta.invoke_traced_function in an ML function
   so that the RETURN at the end of the ML wrapper takes us to the
   code of the function. *)

let invoke_traced_function codeptr env arg =
  Meta.invoke_traced_function codeptr env arg

(* If a function returns a functional value, wrap it into a trace code *)

let rec instrument_result env name clos_typ =
  match (Ctype.repr(Ctype.expand_head env clos_typ)).desc with
    Tarrow(t1, t2) ->
      let starred_name =
        match name with
          Lident s -> Lident(s ^ "*")
        | Ldot(lid, s) -> Ldot(lid, s ^ "*")
        | Lapply(l1, l2) -> fatal_error "Trace.instrument_result" in
      let trace_res = instrument_result env starred_name t2 in
      (fun clos_val ->
        Obj.repr (fun arg ->
          open_box 2;
          Printtyp.longident starred_name;
          print_string " <--"; print_space();
          print_value !toplevel_env arg t1;
          close_box(); print_newline();
          try
            let res = (Obj.magic clos_val : Obj.t -> Obj.t) arg in
            open_box 2;
            Printtyp.longident starred_name;
            print_string " -->"; print_space();
            print_value !toplevel_env res t2;
            close_box(); print_newline();
            trace_res res
          with exn ->
            open_box 2;
            Printtyp.longident starred_name; print_string " raises";
            print_space(); print_exception (Obj.repr exn); close_box();
            print_newline();
            raise exn))
  | _ -> (fun v -> v)

(* Same as instrument_result, but for a toplevel closure (modified in place) *)

let instrument_closure env name clos_typ =
  match (Ctype.repr(Ctype.expand_head env clos_typ)).desc with
    Tarrow(t1, t2) ->
      let trace_res = instrument_result env name t2 in
      (fun actual_code closure arg ->
        open_box 2;
        Printtyp.longident name; print_string " <--"; print_space();
        print_value !toplevel_env arg t1;
        close_box(); print_newline();
        try
          let res = invoke_traced_function actual_code closure arg in
          open_box 2;
          Printtyp.longident name; print_string " -->"; print_space();
          print_value !toplevel_env res t2;
          close_box(); print_newline();
          trace_res res
        with exn ->
          open_box 2;
          Printtyp.longident name; print_string " raises";
          print_space(); print_exception (Obj.repr exn); close_box();
          print_newline();
          raise exn)
  | _ -> assert false

(* Given the address of a closure, find its tracing info *)

let rec find_traced_closure clos = function
    [] -> fatal_error "Trace.find_traced_closure"
  | f :: rem -> if f.closure == clos then f else find_traced_closure clos rem

(* Trace the application of an (instrumented) closure to an argument *)

let print_trace clos arg =
  let f = find_traced_closure clos !traced_functions in
  f.instrumented_fun f.actual_code clos arg
