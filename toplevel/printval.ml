(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* To print values *)

module Printer = Genprintval.Make(Obj)

let max_printer_depth = ref 100
let max_printer_steps = ref 300

let print_exception = Printer.print_exception
let print_value env obj ty =
  Printer.print_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> true) env obj ty

let install_printer = Printer.install_printer
let remove_printer = Printer.remove_printer
