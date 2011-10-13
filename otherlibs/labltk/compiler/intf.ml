(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of OCaml                     *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the OCaml source tree.          *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open StdLabels

(* Write .mli for widgets *)

open Tables
open Compile

let labltk_write_create_p ~w wname =
  w "val create :\n  ?name:string ->\n";
  begin
    try
      let option = Hashtbl.find types_table "options" in
      let classdefs = List.assoc wname option.subtypes in
      let tklabels = List.map ~f:gettklabel classdefs in
      let l = List.map classdefs ~f:
        begin fun fc ->
          begin let p = gettklabel fc in
            if count ~item:p tklabels > 1 then small fc.var_name else p
          end,
          fc.template
        end in
      w (String.concat ~sep:" ->\n"
         (List.map l ~f:
          begin fun (s, t) ->
            "  ?" ^ s ^ ":"
            ^(ppMLtype
             (match types_of_template t with
              | [t] -> labeloff t ~at:"write_create_p"
              | [] -> fatal_error "multiple"
              | l -> Product (List.map ~f:(labeloff ~at:"write_create_p") l)))
          end))
    with Not_found -> fatal_error "in write_create_p"
  end;
  w (" ->\n  'a widget -> " ^ wname ^ " widget\n");
  w "(** [create ?name parent options...] creates a new widget with\n";
  w "    parent [parent] and new patch component [name], if specified. *)\n\n"
;;

let camltk_write_create_p ~w wname =
  w "val create : ?name: string -> widget -> options list -> widget \n";
  w "(** [create ?name parent options] creates a new widget with\n";
  w "    parent [parent] and new patch component [name] if specified.\n";
  w "    Options are restricted to the widget class subset, and checked\n";
  w "    dynamically. *)\n\n"
;;

let camltk_write_named_create_p ~w wname =
  w "val create_named : widget -> string -> options list -> widget \n";
  w "(** [create_named parent name options] creates a new widget with\n";
  w "    parent [parent] and new patch component [name].\n";
  w "    This function is now obsolete and unified with [create]. *)\n\n";
;;

(* Unsafe: write special comment *)
let labltk_write_function_type ~w def =
  if not def.safe then w "(* unsafe *)\n";
  w "val "; w def.ml_name; w " : ";
  let us, ls, os =
    let tys = types_of_template def.template in
    let rec replace_args ~u ~l ~o = function
        [] -> u, l, o
      | (_, List(Subtype _) as x)::ls ->
          replace_args ~u ~l ~o:(x::o) ls
      | ("", _ as x)::ls ->
          replace_args ~u:(x::u) ~l ~o  ls
      | (p, _ as x)::ls when p.[0] = '?' ->
          replace_args ~u ~l ~o:(x::o) ls
      | x::ls ->
          replace_args ~u ~l:(x::l) ~o ls
    in
      replace_args ~u:[] ~l:[] ~o:[] (List.rev tys)
  in
  let counter = ref 0 in
  let params =
    if os = [] then us @ ls else ls @ os @ us in
  List.iter params ~f:
    begin fun (l, t) ->
      if l <> "" then w (l ^ ":");
      w (ppMLtype t ~counter);
      w " -> "
    end;
  if (os <> [] || ls = []) && us = [] then w "unit -> ";
  w (ppMLtype ~any:true ~return:true def.result); (* RETURN TYPE !!! *)
  w " \n";
(*  w "(* tk invocation: "; w (doc_of_template def.template); w " *)"; *)
  if def.safe then w "\n"
  else w "\n(* /unsafe *)\n"

let camltk_write_function_type ~w def =
  if not def.safe then w "(* unsafe *)\n";
  w "val "; w def.ml_name; w " : ";
  let us, os =
    let tys = types_of_template def.template in
    let rec replace_args ~u ~o = function
        [] -> u, o
      | ("", _ as x)::ls ->
          replace_args ~u:(x::u) ~o  ls
      | (p, _ as x)::ls when p.[0] = '?' ->
          replace_args ~u ~o:(x::o) ls
      | x::ls ->
          replace_args ~u:(x::u) ~o ls
    in
      replace_args ~u:[] ~o:[] (List.rev tys)
  in
  let counter = ref 0 in
  let params =
    if os = [] then us else os @ us in
  List.iter params ~f:
    begin fun (l, t) ->
      if l <> "" then if l.[0] = '?' then w (l ^ ":");
      w (ppMLtype t ~counter);
      w " -> "
    end;
  if us = [] then w "unit -> ";
  w (ppMLtype ~any:true ~return:true def.result); (* RETURN TYPE !!! *)
  w " \n";
(*  w "(* tk invocation: "; w (doc_of_template def.template); w " *)"; *)
  if def.safe then w "\n"
  else w "\n(* /unsafe *)\n"

(*
  if not def.safe then w "(* unsafe *)\n";
  w "val "; w def.ml_name; w " : ";
  let tys = types_of_template def.template in
  let counter = ref 0 in
  let have_normal_arg = ref false in
  List.iter tys ~f:
    begin fun (l, t) ->
      if l <> "" then
        if l.[0] = '?' then w (l^":")
        else begin
          have_normal_arg := true;
          w (" (* " ^ l ^ ":*)")
        end
      else have_normal_arg := true;
      w (ppMLtype t ~counter);
      w " -> "
    end;
  if not !have_normal_arg then w "unit -> ";
  w (ppMLtype ~any:true ~return:true def.result); (* RETURN TYPE !!! *)
  w " \n";
  if def.safe then w "\n"
  else w "\n(* /unsafe *)\n"
*)

let write_function_type ~w def =
  if !Flags.camltk then camltk_write_function_type ~w def
  else labltk_write_function_type ~w def

let write_external_type ~w def =
  match def.template with
  | StringArg fname ->
      begin try
        let realname = find_in_path !search_path (fname ^ ".mli") in
        let ic = open_in_bin realname in
        try
          let code_list = Ppparse.parse_channel ic in
          close_in ic;
          if not def.safe then w "(* unsafe *)\n";
          List.iter (Ppexec.exec (fun _ -> ()) w)
            (if !Flags.camltk then
              Code.Define "CAMLTK" :: code_list else code_list );
          if def.safe then w "\n\n"
          else w "\n(* /unsafe *)\n\n"
        with
        | Ppparse.Error s ->
            close_in ic;
            raise (Compiler_Error (Printf.sprintf "Preprocess error: %s" s))
      with
      | Not_found ->
          raise (Compiler_Error ("can't find external file: " ^ fname))
      end
  | _ -> raise (Compiler_Error "invalid external definition")
