(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open StdLabels
open Support
open Tables
open Printer
open Compile
open Intf

let flag_verbose = ref false
let verbose_string s = 
  if !flag_verbose then prerr_string s
let verbose_endline s =
  if !flag_verbose then prerr_endline s

let input_name = ref "Widgets.src"
let output_dir = ref ""
let destfile f = Filename.concat !output_dir f

let usage () = 
  prerr_string "Usage: tkcompiler input.src\n";
  flush stderr;
  exit 1


let prerr_error_header () =
  prerr_string "File \""; prerr_string !input_name;
  prerr_string "\", line ";
  prerr_string (string_of_int !Lexer.current_line);
  prerr_string ": "

(* parse Widget.src config file *)
let parse_file filename =
  let ic = open_in_bin filename in
  let lexbuf =
    try
      let code_list = Ppparse.parse_channel ic in
      close_in ic;
      let buf = Buffer.create 50000 in
      List.iter (Ppexec.exec 
                   (fun l -> Buffer.add_string buf
                       (Printf.sprintf "##line %d\n" l))
                   (Buffer.add_string buf))
        (if !Flags.camltk then Code.Define "CAMLTK" :: code_list 
        else code_list);
      Lexing.from_string (Buffer.contents buf)
    with
    | Ppparse.Error s -> 
        close_in ic;
        raise (Compiler_Error (Printf.sprintf "Preprocess error: %s" s))
  in
  try
    while true do
      Parser.entry Lexer.main lexbuf
    done
  with
  | Parsing.Parse_error ->
      prerr_error_header();
      prerr_string "Syntax error \n";
      exit 1
  | Lexer.Lexical_error s ->
      prerr_error_header();
      prerr_string "Lexical error (";
      prerr_string s;
      prerr_string ")\n";
      exit 1
  | Duplicate_Definition (s,s') ->
      prerr_error_header();
      prerr_string s; prerr_string " "; prerr_string s';
      prerr_string " is defined twice.\n";
      exit 1
  | Compiler_Error s ->
      prerr_error_header();
      prerr_string "Internal error: "; prerr_string s; prerr_string "\n";
      prerr_string "Please report bug\n";
      exit 1
  | End_of_file ->
      ()

(* The hack to provoke the production of cCAMLtoTKoptions_constrs *)

(* Auxiliary function: the list of all the elements associated to keys
   in an hash table. *)
let elements t =
 let elems = ref [] in
 Hashtbl.iter (fun _ d -> elems := d :: !elems) t;
 !elems;;

(* Verifies that duplicated clauses are semantically equivalent and
   returns a unique set of clauses. *)
let uniq_clauses = function
  | [] -> []
  | l ->
     let check_constr constr1 constr2 =
       if constr1.template <> constr2.template then
       begin
        let code1, vars11, vars12, opts1 = 
         code_of_template ~context_widget:"dummy" constr1.template in
        let code2, vars12, vars22, opts2 = 
         code_of_template ~context_widget:"dummy" constr2.template in
        let err =
         Printf.sprintf
          "uncompatible redondant clauses for variant %s:\n %s\n and\n %s"
          constr1.var_name code1 code2 in
        Format.print_newline();
        print_fullcomponent constr1;
        Format.print_newline();
        print_fullcomponent constr2;
        Format.print_newline();
        prerr_endline err;
        fatal_error err
       end in
     let t = Hashtbl.create 11 in
     List.iter l
      ~f:(fun constr ->
       let c = constr.var_name in
       if Hashtbl.mem t c
       then (check_constr constr (Hashtbl.find t c))
       else Hashtbl.add t c constr);
     elements t;;

let option_hack oc =
  if Hashtbl.mem types_table "options" then
   let typdef = Hashtbl.find types_table "options" in
   let hack =
     { parser_arity = OneToken;
       constructors = begin
         let constrs = 
           List.map typdef.constructors ~f:
             begin fun c -> 
               { component = Constructor;
                 ml_name = (if !Flags.camltk then "C" ^ c.ml_name 
                            else c.ml_name);
                 var_name = c.var_name; (* as variants *)
                 template = 
                 begin match c.template with
                   ListArg (x :: _) -> x
                 | _ -> fatal_error "bogus hack"
                 end;
                 result = UserDefined "options_constrs";
                 safe = true }
             end in
         if !Flags.camltk then constrs else uniq_clauses constrs (* JPF ?? *)
       end;
       subtypes = [];
       requires_widget_context = false; 
       variant = false }
   in
   write_CAMLtoTK
     ~w:(output_string oc) ~def:hack ~safetype:false "options_constrs"

let realname name = 
  (* module name fix for camltk *)
  if !Flags.camltk then "c" ^ String.capitalize name
  else name
;;

(* analize the parsed Widget.src and output source files *)
let compile () = 
  verbose_endline "Creating _tkgen.ml ...";
  let oc = open_out_bin (destfile "_tkgen.ml") in
  let oc' = open_out_bin (destfile "_tkigen.ml") in
  let oc'' = open_out_bin (destfile "_tkfgen.ml") in
  let sorted_types = Tsort.sort types_order in
  verbose_endline "  writing types ...";
  List.iter sorted_types ~f:
  begin fun typname ->
  verbose_string ("    " ^ typname ^ " ");
  try
    let typdef = Hashtbl.find types_table typname in
    verbose_string "type ";
    write_type ~intf:(output_string oc)
               ~impl:(output_string oc')
               typname ~def:typdef;
    verbose_string "C2T ";
    write_CAMLtoTK ~w:(output_string oc') typname ~def:typdef;
    verbose_string "T2C ";
    if List.mem typname !types_returned then
    write_TKtoCAML ~w:(output_string oc') typname ~def:typdef;
    verbose_string "CO ";
    if not !Flags.camltk then (* only for LablTk *)
      write_catch_optionals ~w:(output_string oc') typname ~def:typdef;
    verbose_endline "."
  with Not_found -> 
    if not (List.mem_assoc typname !types_external) then
    begin
      verbose_string "Type ";
      verbose_string typname;
      verbose_string " is undeclared external or undefined\n"
    end
    else verbose_endline "."
  end;
  verbose_endline "  option hacking ...";
  option_hack oc';
  verbose_endline "  writing functions ...";
  List.iter ~f:(write_function ~w:(output_string oc'')) !function_table;
  close_out oc;
  close_out oc';
  close_out oc'';
  (* Write the interface for public functions *)
  (* this interface is used only for documentation *)
  verbose_endline "Creating _tkgen.mli ...";
  let oc = open_out_bin (destfile "_tkgen.mli") in
  List.iter (sort_components !function_table)
    ~f:(write_function_type ~w:(output_string oc));
  close_out oc;
  verbose_endline "Creating other ml, mli ...";
  let write_module wname wdef =
    verbose_endline ("  "^wname);
    let modname = realname wname in
    let oc = open_out_bin (destfile (modname ^ ".ml")) 
    and oc' = open_out_bin (destfile (modname ^ ".mli")) in 
    Copyright.write ~w:(output_string oc);
    Copyright.write ~w:(output_string oc');
    begin match wdef.module_type with
      Widget -> output_string oc' ("(* The "^wname^" widget *)\n")
    | Family -> output_string oc' ("(* The "^wname^" commands  *)\n")
    end;
    List.iter ~f:(fun s -> output_string oc s; output_string oc' s)
      begin
        if !Flags.camltk then
          [ "open CTk\n";
            "open Tkintf\n";
            "open Widget\n";
            "open Textvariable\n\n" ]
        else
          [ "open StdLabels\n";
            "open Tk\n";
            "open Tkintf\n";
            "open Widget\n";
            "open Textvariable\n\n" ]
      end;
    output_string oc "open Protocol\n";
    begin match wdef.module_type with
      Widget ->
        if !Flags.camltk then begin
          camltk_write_create ~w:(output_string oc) wname;
          camltk_write_named_create ~w:(output_string oc) wname;
          camltk_write_create_p ~w:(output_string oc') wname;
          camltk_write_named_create_p ~w:(output_string oc') wname;
        end else begin
          labltk_write_create ~w:(output_string oc) wname;
          labltk_write_create_p ~w:(output_string oc') wname
        end
    | Family -> ()
    end;
    List.iter ~f:(write_function ~w:(output_string oc)) 
          (sort_components wdef.commands);
    List.iter ~f:(write_function_type ~w:(output_string oc'))
          (sort_components wdef.commands);
    List.iter ~f:(write_external ~w:(output_string oc)) 
           (sort_components wdef.externals);
    List.iter ~f:(write_external_type ~w:(output_string oc'))
           (sort_components wdef.externals);
    close_out oc;
    close_out oc'
  in Hashtbl.iter write_module module_table;

  (* wrapper code camltk.ml and labltk.ml *)
  if !Flags.camltk then begin
    let oc = open_out_bin (destfile "camltk.ml") in
    Copyright.write ~w:(output_string oc);
    output_string oc 
"(** This module Camltk provides the module name spaces of the CamlTk API.

  The users of the CamlTk API should open this module first to access
  the types, functions and modules of the CamlTk API easier. 
  For the documentation of each sub modules such as [Button] and [Toplevel],
  refer to its defintion file,  [cButton.mli], [cToplevel.mli], etc. 
 *)

";
    output_string oc "include CTk\n";
    output_string oc "module Tk = CTk\n";
    Hashtbl.iter (fun name _ ->
      let cname = realname name in
      output_string oc (Printf.sprintf "module %s = %s;;\n"
                          (String.capitalize name)
                          (String.capitalize cname))) module_table;
    close_out oc
  end else begin
    let oc = open_out_bin (destfile "labltk.ml") in
    Copyright.write ~w:(output_string oc);
    output_string oc 
"(** This module Labltk provides the module name spaces of the LablTk API,
  useful to call LablTk functions inside CamlTk programs. 100% LablTk users
  do not need to use this. *)

";
    output_string oc "module Widget = Widget;;
module Protocol = Protocol;;
module Textvariable = Textvariable;;
module Fileevent = Fileevent;;
module Timer = Timer;;
";
    Hashtbl.iter (fun name _ ->
      let cname = realname name in
      output_string oc (Printf.sprintf "module %s = %s;;\n"
                          (String.capitalize name)
                          (String.capitalize name))) module_table;
    (* widget typer *)
    output_string oc "\n(** Widget typers *)\n\nopen Widget\n\n";
    Hashtbl.iter (fun name def ->
      match def.module_type with
      | Widget ->
          output_string oc (Printf.sprintf 
              "let %s (w : any widget) =\n" name);
          output_string oc (Printf.sprintf 
              "  Rawwidget.check_class w widget_%s_table;\n" name);
          output_string oc (Printf.sprintf
              "  (Obj.magic w : %s widget);;\n\n" name);
      | _ -> () ) module_table;
    close_out oc
  end;

  (* write the module list for the Makefile *)
  (* and hack to death until it works *)
  let oc = open_out_bin (destfile "modules") in
  if !Flags.camltk then output_string oc "CWIDGETOBJS="
  else output_string oc "WIDGETOBJS=";
  Hashtbl.iter
    (fun name _ ->
      let name = realname name in
      output_string oc name;
      output_string oc ".cmo ")
    module_table;
  output_string oc "\n";
  Hashtbl.iter
    (fun name _ ->
      let name = realname name in
      output_string oc name;
      output_string oc ".ml ")
    module_table;
  output_string oc ": _tkgen.ml\n\n";
  Hashtbl.iter
    (fun name _ ->
      let name = realname name in
      output_string oc name;
      output_string oc ".cmo : ";
      output_string oc name;
      output_string oc ".ml\n";
      output_string oc name;
      output_string oc ".cmi : ";
      output_string oc name;
      output_string oc ".mli\n")
    module_table;

  (* for camltk.ml wrapper *)
  if !Flags.camltk then begin
    output_string oc "camltk.cmo : cTk.cmo ";
    Hashtbl.iter
      (fun name _ ->
        let name = realname name in
        output_string oc name;
        output_string oc ".cmo ") module_table;
    output_string oc "\n"
  end;
  close_out oc

let main () =
  Arg.parse
    [ "-verbose",  Arg.Unit (fun () -> flag_verbose := true),
      "Make output verbose";
      "-camltk", Arg.Unit (fun () -> Flags.camltk := true),
      "Make CamlTk interface";
      "-outdir", Arg.String (fun s -> output_dir := s),
      "output directory";
      "-debugpp", Arg.Unit (fun () -> Ppexec.debug := true),
      "debug preprocessor"
    ]
    (fun filename -> input_name := filename)
    "Usage: tkcompiler <source file>" ;
  if !output_dir = "" then begin
    prerr_endline "specify -outdir option";
    exit 1
  end;
  try
    verbose_endline "Parsing...";
    parse_file !input_name;
    verbose_endline "Compiling...";
    compile ();
    verbose_endline "Finished";
    exit 0
  with
  | Lexer.Lexical_error s ->
      prerr_string "Invalid lexical character: ";
      prerr_endline s;
      exit 1
  | Duplicate_Definition (s, s') ->
      prerr_string s; prerr_string " "; prerr_string s';
      prerr_endline " is redefined illegally";
      exit 1
  | Invalid_implicit_constructor c ->
      prerr_string "Constructor ";
      prerr_string c;
      prerr_endline " is used implicitly before defined";
      exit 1
  | Tsort.Cyclic ->
      prerr_endline "Cyclic dependency of types";
      exit 1

let () = Printexc.catch main ()
