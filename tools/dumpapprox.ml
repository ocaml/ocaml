(* Dump a .cmx file *)

open Config
open Format
open Clambda
open Compilenv

let rec print_approx = function
    Value_closure(fundesc, approx) ->
      open_hovbox 2;
      print_string "function "; print_string fundesc.fun_label;
      print_space(); print_string "arity "; print_int fundesc.fun_arity;
      if fundesc.fun_closed then begin
        print_space(); print_string "(closed)"
      end;
      print_space(); print_string "->"; print_space();
      print_approx approx;
      close_box()
  | Value_tuple approx ->
      open_hvbox 1;
      print_string "[";
      for i = 0 to Array.length approx - 1 do
        if i > 0 then (print_string ";"; print_space());
        print_int i; print_string ": "; print_approx approx.(i)
      done;
      print_string "]";
      close_box()
  | Value_unknown ->
      print_string "_"

let print_name_crc (name, crc) =
  print_space(); print_string name;
  print_string " ("; print_int crc; print_string ")"

let print_infos (ui, crc) =
  print_string "Name: "; print_string ui.ui_name; print_newline();
  print_string "CRC: "; print_int crc; print_newline();
  open_hovbox 2;
  print_string "Interfaces imported:";
  List.iter print_name_crc ui.ui_interfaces;
  close_box(); print_newline();
  open_hovbox 2;
  print_string "Implementations imported:";
  List.iter print_name_crc ui.ui_imports;
  close_box(); print_newline();
  open_hovbox 2;
  print_string "Approximation:"; print_space(); print_approx ui.ui_approx;
  close_box(); print_newline();
  open_hovbox 2;
  print_string "Currying functions:";
  List.iter
    (fun arity -> print_space(); print_int arity)
    ui.ui_curry_fun;
  close_box(); print_newline();
  open_hovbox 2;
  print_string "Apply functions:";
  List.iter
    (fun arity -> print_space(); print_int arity)
    ui.ui_apply_fun;
  close_box(); print_newline()

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length cmx_magic_number) in
    really_input ic buffer 0 (String.length cmx_magic_number);
    if buffer <> cmx_magic_number then begin
      close_in ic;
      prerr_endline "Wrong magic number";
      exit 2
    end;
    let ui = (input_value ic : unit_infos) in
    let crc = input_binary_int ic in
    close_in ic;
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    prerr_endline "Error reading file";
    exit 2

let main () =
  print_infos(read_unit_info Sys.argv.(1));
  exit 0

let _ = main ()


    
