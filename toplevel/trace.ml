(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The "trace" facility *)

open Format
open Misc
open Longident
open Types
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

let print_label ppf l =
  if l <> Asttypes.Nolabel then fprintf ppf "%s:" (Printtyp.string_of_label l)

(* If a function returns a functional value, wrap it into a trace code *)

let rec instrument_result env name ppf clos_typ =
  match (Ctype.repr(Ctype.expand_head env clos_typ)).desc with
  | Tarrow(l, t1, t2, _) ->
      let starred_name =
        match name with
        | Lident s -> Lident(s ^ "*")
        | Ldot(lid, s) -> Ldot(lid, s ^ "*")
        | Lapply _ -> fatal_error "Trace.instrument_result" in
      let trace_res = instrument_result env starred_name ppf t2 in
      (fun clos_val ->
        Obj.repr (fun arg ->
          if not !may_trace then
            (Obj.magic clos_val : Obj.t -> Obj.t) arg
          else begin
            may_trace := false;
            try
              fprintf ppf "@[<2>%a <--@ %a%a@]@."
                Printtyp.longident starred_name
                print_label l
                (print_value !toplevel_env arg) t1;
              may_trace := true;
              let res = (Obj.magic clos_val : Obj.t -> Obj.t) arg in
              may_trace := false;
              fprintf ppf "@[<2>%a -->@ %a@]@."
                Printtyp.longident starred_name
                (print_value !toplevel_env res) t2;
              may_trace := true;
              trace_res res
            with exn ->
              may_trace := false;
              fprintf ppf "@[<2>%a raises@ %a@]@."
                Printtyp.longident starred_name
                (print_value !toplevel_env (Obj.repr exn)) Predef.type_exn;
              may_trace := true;
              raise exn
          end))
  | _ -> (fun v -> v)

(* Same as instrument_result, but for a toplevel closure (modified in place) *)

exception Dummy
let _ = Dummy

let instrument_closure env name ppf clos_typ =
  match (Ctype.repr(Ctype.expand_head env clos_typ)).desc with
  | Tarrow(l, t1, t2, _) ->
      let trace_res = instrument_result env name ppf t2 in
      (fun actual_code closure arg ->
        if not !may_trace then begin
          try invoke_traced_function actual_code closure arg
          with Dummy -> assert false
          (* do not remove handler, prevents tail-call to invoke_traced_ *)
        end else begin
          may_trace := false;
          try
            fprintf ppf "@[<2>%a <--@ %a%a@]@."
              Printtyp.longident name
              print_label l
              (print_value !toplevel_env arg) t1;
            may_trace := true;
            let res = invoke_traced_function actual_code closure arg in
            may_trace := false;
            fprintf ppf "@[<2>%a -->@ %a@]@."
              Printtyp.longident name
              (print_value !toplevel_env res) t2;
            may_trace := true;
            trace_res res
          with exn ->
            may_trace := false;
            fprintf ppf "@[<2>%a raises@ %a@]@."
              Printtyp.longident name
              (print_value !toplevel_env (Obj.repr exn)) Predef.type_exn;
            may_trace := true;
            raise exn
        end)
  | _ -> assert false

(* Given the address of a closure, find its tracing info *)

let rec find_traced_closure clos = function
  | [] -> fatal_error "Trace.find_traced_closure"
  | f :: rem -> if f.closure == clos then f else find_traced_closure clos rem

(* Trace the application of an (instrumented) closure to an argument *)

let print_trace clos arg =
  let f = find_traced_closure clos !traced_functions in
  f.instrumented_fun f.actual_code clos arg

external current_environment: unit -> Obj.t = "caml_get_current_environment"

let tracing_function_ptr =
  get_code_pointer
    (Obj.repr (fun arg -> print_trace (current_environment()) arg))

let dir_trace ppf lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    (* Check if this is a primitive *)
    match desc.val_kind with
    | Val_prim _ ->
        fprintf ppf "%a is an external function and cannot be traced.@."
        Printtyp.longident lid
    | _ ->
        let clos = eval_path !toplevel_env path in
        (* Nothing to do if it's not a closure *)
        if Obj.is_block clos
        && (Obj.tag clos = Obj.closure_tag || Obj.tag clos = Obj.infix_tag)
        && (match Ctype.(repr (expand_head !toplevel_env desc.val_type))
            with {desc=Tarrow _} -> true | _ -> false)
        then begin
        match is_traced clos with
        | Some opath ->
            fprintf ppf "%a is already traced (under the name %a).@."
            Printtyp.path path
            Printtyp.path opath
        | None ->
            (* Instrument the old closure *)
            traced_functions :=
              { path = path;
                closure = clos;
                actual_code = get_code_pointer clos;
                instrumented_fun =
                  instrument_closure !toplevel_env lid ppf desc.val_type }
              :: !traced_functions;
            (* Redirect the code field of the closure to point
               to the instrumentation function *)
            set_code_pointer clos tracing_function_ptr;
            fprintf ppf "%a is now traced.@." Printtyp.longident lid
        end else fprintf ppf "%a is not a function.@." Printtyp.longident lid
  with
  | Not_found -> fprintf ppf "Unbound value %a.@." Printtyp.longident lid

let dir_untrace ppf lid =
  try
    let (path, _desc) = Env.lookup_value lid !toplevel_env in
    let rec remove = function
    | [] ->
        fprintf ppf "%a was not traced.@." Printtyp.longident lid;
        []
    | f :: rem ->
        if Path.same f.path path then begin
          set_code_pointer f.closure f.actual_code;
          fprintf ppf "%a is no longer traced.@." Printtyp.longident lid;
          rem
        end else f :: remove rem in
    traced_functions := remove !traced_functions
  with
  | Not_found -> fprintf ppf "Unbound value %a.@." Printtyp.longident lid

let dir_untrace_all ppf () =
  List.iter
    (fun f ->
      set_code_pointer f.closure f.actual_code;
      fprintf ppf "%a is no longer traced.@." Printtyp.path f.path)
    !traced_functions;
  traced_functions := []

let std_out = std_formatter
let section_trace = "Tracing"

let _ = add_directive "trace"
    (Directive_ident (dir_trace std_out))
    {
      section = section_trace;
      doc = "All calls to the function \
          named function-name will be traced.";
    }

let _ = add_directive "untrace"
    (Directive_ident (dir_untrace std_out))
    {
      section = section_trace;
      doc = "Stop tracing the given function.";
    }

let _ = add_directive "untrace_all"
    (Directive_none (dir_untrace_all std_out))
    {
      section = section_trace;
      doc = "Stop tracing all functions traced so far.";
    }

