(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Francois Rouaix, Francois Pessaux and Jun Furuse              *)
(*               projet Cristal, INRIA Rocquencourt                      *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open StdLabels

(* Write .mli for widgets *)

open Tables
open Compile

let write_create_p ~w wname =
  w "val create :\n  ?name:string ->\n";
  begin
    try 
      let option = Hashtbl.find types_table "options" in
      let classdefs = List.assoc wname ~map:option.subtypes in
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
  w "             (* [create p options ?name] creates a new widget with\n";
  w "                parent p and new patch component name.\n";
  w "                Options are restricted to the widget class subset,\n";
  w "                and checked dynamically. *)\n"

(* Unsafe: write special comment *)
let write_function_type ~w def =
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
  if def.safe then w "\n\n"
  else w "\n(* /unsafe *)\n\n"

let write_external_type ~w def =
  match def.template with
  | StringArg fname ->
      begin try
        let realname = find_in_path !search_path (fname ^ ".mli") in
        let ic = open_in_bin realname in
        if not def.safe then w "(* unsafe *)\n";
        begin try
         while true do
           w (input_line ic);
           w "\n"
         done
        with
        | End_of_file -> 
            close_in ic;
            if def.safe then w "\n\n"
            else w "\n(* /unsafe *)\n\n"
        end
      with
      | Not_found ->
          raise (Compiler_Error ("can't find external file: " ^ fname))
      end
  | _ -> raise (Compiler_Error "invalid external definition")
