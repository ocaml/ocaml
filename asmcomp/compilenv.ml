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

(* Compilation environments for compilation units *)

open Config
open Misc
open Clambda

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string

exception Error of error

(* Each .o file has a matching .cmx file that provides the following infos
   on the compilation unit:
     - list of other units imported, with CRCs of their .cmx files
     - approximation of the structure implemented
       (includes descriptions of known functions: arity and direct entry
        points)
     - list of currying functions and application functions needed
   The .cmx file contains these infos (as an externed record) plus a CRC
   of these infos *)

type unit_infos =
  { mutable ui_name: string;                    (* Name of unit implemented *)
    mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
    mutable ui_imports_cmx: (string * Digest.t) list; (* Infos imported *)
    mutable ui_approx: value_approximation;     (* Approx of the structure *)
    mutable ui_curry_fun: int list;             (* Currying functions needed *)
    mutable ui_apply_fun: int list;             (* Apply functions needed *)
    mutable ui_force_link: bool }               (* Always linked *)

let global_approx_table =
  (Hashtbl.create 17 : (string, value_approximation) Hashtbl.t)

let current_unit =
  { ui_name = "";
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_approx = Value_unknown;
    ui_curry_fun = [];
    ui_apply_fun = [];
    ui_force_link = false }

let reset name =
  Hashtbl.clear global_approx_table;
  current_unit.ui_name <- name;
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_force_link <- false

let current_unit_name () =
  current_unit.ui_name

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length cmx_magic_number) in
    really_input ic buffer 0 (String.length cmx_magic_number);
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let ui = (input_value ic : unit_infos) in
    let crc = Digest.input ic in
    close_in ic;
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

(* Return the approximation of a global identifier *)

let cmx_not_found_crc =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let global_approx global_ident =
  let modname = Ident.name global_ident in
  try
    Hashtbl.find global_approx_table modname
  with Not_found ->
    let (approx, crc) =
      try
        let filename =
          find_in_path !load_path (String.uncapitalize modname ^ ".cmx") in
        let (ui, crc) = read_unit_info filename in
        if ui.ui_name <> modname then
          raise(Error(Illegal_renaming(modname, filename)));
        (ui.ui_approx, crc)
      with Not_found ->
        (Value_unknown, cmx_not_found_crc) in
    current_unit.ui_imports_cmx <-
      (modname, crc) :: current_unit.ui_imports_cmx;
    Hashtbl.add global_approx_table modname approx;
    approx

(* Register the approximation of the module being compiled *)

let set_global_approx approx =
  current_unit.ui_approx <- approx

(* Record that a currying function or application function is needed *)

let need_curry_fun n =
  if not (List.mem n current_unit.ui_curry_fun) then
    current_unit.ui_curry_fun <- n :: current_unit.ui_curry_fun

let need_apply_fun n =
  if not (List.mem n current_unit.ui_apply_fun) then
    current_unit.ui_apply_fun <- n :: current_unit.ui_apply_fun

(* Write the description of the current unit *)

let save_unit_info filename =
  current_unit.ui_imports_cmi <- Env.imported_units();
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc current_unit;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

(* Error report *)

open Format

let report_error = function
    Not_a_unit_info filename ->
      print_string filename; print_space();
      print_string "is not a compilation unit description."
  | Corrupted_unit_info filename ->
      print_string "Corrupted compilation unit description"; print_space();
      print_string filename
  | Illegal_renaming(modname, filename) ->
      print_string filename; print_space();
      print_string "contains the description for unit"; print_space();
      print_string modname

