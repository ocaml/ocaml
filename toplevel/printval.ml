(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Misc
open Path

(* Return the value referred to by a path *)

let rec eval_path = function
  | Pident id -> Symtable.get_global_value id
  | Pdot(p, s, pos) -> Obj.field (eval_path p) pos
  | Papply(p1, p2) -> fatal_error "Topdirs.eval_path"

(* To print values *)

module EvalPath = struct
  type value = Obj.t
  exception Error
  let eval_path p = try eval_path p with Symtable.Error _ -> raise Error
end

module Printer = Genprintval.Make(Obj)(EvalPath)

let max_printer_depth = ref 100
let max_printer_steps = ref 300

let print_untyped_exception = Printer.print_untyped_exception
let print_value env obj ty =
  Printer.print_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> true) env obj ty

let install_printer = Printer.install_printer
let remove_printer = Printer.remove_printer
