(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Dump a .cmx file *)

open Config
open Format
open Clambda
open Compilenv

let print_digest ppf d =
  for i = 0 to String.length d - 1 do
    print_string(Printf.sprintf "%02x" (Char.code d.[i]))
  done

let rec print_approx ppf = function
    Value_closure(fundesc, approx) ->
      printf "@[<2>function %s@ arity %i" fundesc.fun_label fundesc.fun_arity;
      if fundesc.fun_closed then begin
        printf "@ (closed)"
      end;
      if fundesc.fun_inline <> None then begin
        printf "@ (inline)"
      end;
      printf "@ -> @ %a@]" print_approx approx
  | Value_tuple approx ->
      let tuple ppf approx =
        for i = 0 to Array.length approx - 1 do
          if i > 0 then printf ";@ ";
          printf "%i: %a" i print_approx approx.(i)
        done in
      printf "@[<hov 1>(%a)@]" tuple approx
  | Value_unknown ->
      print_string "_"
  | Value_integer n ->
      print_int n
  | Value_constptr n ->
      print_int n; print_string "p"

let print_name_crc (name, crc) =
  printf "@ %s (%a)" name print_digest crc

let print_infos (ui, crc) =
  printf "Name: %s@." ui.ui_name;
  printf "CRC of implementation: %a@." print_digest crc;
  printf "@[<hov 2>Globals defined:";
  List.iter (fun s -> printf "@ %s" s) ui.ui_defines;
  printf "@]@.";
  let pr_imports ppf imps = List.iter print_name_crc imps in
  printf "@[<v 2>Interfaces imported:%a@]@." pr_imports ui.ui_imports_cmi;
  printf "@[<v 2>Implementations imported:%a@]@." pr_imports ui.ui_imports_cmx;
  printf "@[<v 2>Approximation:@ %a@]@." print_approx ui.ui_approx;
  let pr_funs ppf fns =
    List.iter (fun arity -> printf "@ %i" arity) fns in
  printf "@[<2>Currying functions:%a@]@." pr_funs ui.ui_curry_fun;
  printf "@[<2>Apply functions:%a@]@." pr_funs ui.ui_apply_fun

let print_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length cmx_magic_number) in
    really_input ic buffer 0 (String.length cmx_magic_number);
    if buffer = cmx_magic_number then begin
      let ui = (input_value ic : unit_infos) in
      let crc = Digest.input ic in
      close_in ic;
      print_infos (ui, crc)
    end else if buffer = cmxa_magic_number then begin
      let li = (input_value ic : library_infos) in
      close_in ic;
      List.iter print_infos li.lib_units
    end else begin
      close_in ic;
      prerr_endline "Wrong magic number";
      exit 2
    end
  with End_of_file | Failure _ ->
    close_in ic;
    prerr_endline "Error reading file";
    exit 2

let main () =
  print_unit_info Sys.argv.(1);
  exit 0

let _ = main ()


    
