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
  { mutable ui_name: string;
    mutable ui_interfaces: (string * int) list;
    mutable ui_imports: (string * int) list;
    mutable ui_approx: value_approximation;
    mutable ui_curry_fun: int list;
    mutable ui_apply_fun: int list }

let global_approx_table =
  (Hashtbl.new 17 : (string, value_approximation) Hashtbl.t)

let current_unit =
  { ui_name = "";
    ui_interfaces = [];
    ui_imports = [];
    ui_approx = Value_unknown;
    ui_curry_fun = [];
    ui_apply_fun = [] }

let reset name crc_intf =
  Hashtbl.clear global_approx_table;
  current_unit.ui_name <- name;
  current_unit.ui_interfaces <- [name, crc_intf];
  current_unit.ui_imports <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- []

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
    let crc = input_binary_int ic in
    close_in ic;
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

(* Return the approximation of a global identifier *)

let global_approx global_ident =
  let modname = Ident.name global_ident in
  try
    Hashtbl.find global_approx_table modname
  with Not_found ->
    let approx =
      try
        let filename =
          find_in_path !load_path (lowercase modname ^ ".cmx") in
        let (ui, crc) = read_unit_info filename in
        if ui.ui_name <> modname then
          raise(Error(Illegal_renaming(modname, filename)));
        current_unit.ui_imports <-
          (modname, crc) :: current_unit.ui_imports;
        ui.ui_approx
      with Not_found ->
        Value_unknown in
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
  current_unit.ui_interfaces <-
    current_unit.ui_interfaces @ Env.imported_units();
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc current_unit;
  let pos = pos_out oc in
  flush oc;
  let ic = open_in_bin filename in
  let crc = Crc.for_channel ic pos in
  close_in ic;
  output_binary_int oc crc;
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

