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

(* To print values *)

open Misc
open Obj
open Format
open Parser_aux
open Path
open Types

(* To name printed and ellipsed values *)

let named_values =
  (Hashtbl.create 29 : (int, Debugcom.Remote_value.t * type_expr) Hashtbl.t)
let next_name = ref 1

let reset_named_values () =
  Hashtbl.clear named_values;
  next_name := 1

let name_value v ty =
  let name = !next_name in
  incr next_name;
  Hashtbl.add named_values name (v, ty);
  name

let find_named_value name =
  Hashtbl.find named_values name

let check_depth depth obj ty =
  if depth <= 0 then begin
    let n = name_value obj ty in
    print_char '$'; print_int n;
    false
  end else true

module Printer = Genprintval.Make(Debugcom.Remote_value)

let install_printer path ty fn =
  Printer.install_printer path ty
    (function remote_val ->
       try
         fn (Obj.repr (Debugcom.Remote_value.obj remote_val))
       with
         Debugcom.Marshalling_error ->
           print_string "<cannot fetch remote object>")

let remove_printer = Printer.remove_printer

let max_printer_depth = ref 20
let max_printer_steps = ref 300

let print_value max_depth obj ty env =
  Printer.print_value !max_printer_steps max_depth
    check_depth env obj ty

let print_named_value max_depth exp obj ty env =
  open_box 2;
  begin match exp with
    E_ident lid ->
      Printtyp.longident lid
  | E_name n ->
      print_char '$'; print_int n
  | _ ->
      let n = name_value obj ty in
      print_char '$'; print_int n
  end;
  Printtyp.reset (); Printtyp.mark_loops ty;
  print_string " :"; print_space(); Printtyp.type_expr ty;
  print_space(); print_string "="; print_space();
  print_value max_depth obj ty env;
  close_box();
  print_newline()
