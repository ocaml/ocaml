(* $Id$ *)

open Unix
open Protocol

external add_file_input : file_descr -> cbid -> unit
        =  "camltk_add_file_input"
external rem_file_input : file_descr -> unit
        =  "camltk_rem_file_input"
external add_file_output : file_descr -> cbid -> unit
        =  "camltk_add_file_output"
external rem_file_output : file_descr -> unit
        =  "camltk_rem_file_output"

(* File input handlers *)

let fd_table = Hashtbl.create 37 (* Avoid space leak in callback table *)

let add_fileinput :fd callback:f =
  let id = new_function_id () in
  Hashtbl.add callback_naming_table key:id data:(fun _ -> f());
  Hashtbl.add fd_table key:(fd, 'r') data:id;
  if !Protocol.debug then begin
    Protocol.prerr_cbid id; prerr_endline " for fileinput"
  end;
  add_file_input fd id

let remove_fileinput :fd =
  try
    let id = Hashtbl.find fd_table key:(fd, 'r') in
    clear_callback id;
    Hashtbl.remove fd_table key:(fd, 'r');
    if !Protocol.debug then begin
      prerr_string "clear "; 
      Protocol.prerr_cbid id;
      prerr_endline " for fileinput"
    end;
    rem_file_input fd
  with
    Not_found -> ()

let add_fileoutput :fd callback:f =
  let id = new_function_id () in
  Hashtbl.add callback_naming_table key:id data:(fun _ -> f());
  Hashtbl.add fd_table key:(fd, 'w') data:id;
  if !Protocol.debug then begin
    Protocol.prerr_cbid id; prerr_endline " for fileoutput"
  end;
  add_file_output fd id

let remove_fileoutput :fd =
  try
    let id = Hashtbl.find fd_table key:(fd, 'w') in
    clear_callback id;
    Hashtbl.remove fd_table key:(fd, 'w');
    if !Protocol.debug then begin
      prerr_string "clear "; 
      Protocol.prerr_cbid id;
      prerr_endline " for fileoutput"
    end;
    rem_file_output fd
  with
    Not_found -> ()

