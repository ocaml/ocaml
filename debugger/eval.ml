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

open Debugger_config
open Misc
open Path
open Instruct

let rec path env sz = function
    Pident id ->
      if Ident.global id then
        Debugcom.get_global (Symtable.get_global_position id)
      else begin
        try
          let pos = Ident.find_same id env.ce_stack in
          Debugcom.get_local (sz - pos)
        with Not_found ->
        try
          let pos = Ident.find_same id env.ce_heap in
          Debugcom.get_environment pos
        with Not_found ->
          Format.print_string "Cannot evaluate ";
          Printtyp.ident id;
          Format.print_newline();
          raise Toplevel
      end
  | Pdot(root, fieldname, pos) ->
      let v = path env sz root in
      if Debugcom.remote_value_is_int v then begin
        Printtyp.path root;
        Format.print_string " has not yet been initialized";
        Format.print_newline();
        raise Toplevel
      end;
      Debugcom.get_field v pos
  | Papply(p1, p2) ->
      fatal_error "Eval.path: Papply"
