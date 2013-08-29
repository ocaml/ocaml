(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let autoopen_modules = ["Parsetree"; "Asttypes"; "Longident"; "Location"; "Lexing"]
let autoopen = ref true

let typ ty =
  let ty = Parse.core_type (Lexing.from_string ty) in
  let ty = Typetexp.transl_simple_type Env.initial false ty in
  ty.Typedtree.ctyp_type

let show_value ty v =
  let env = Env.initial in
  let env = if !autoopen then List.fold_right Env.open_pers_signature autoopen_modules env else env in
  Format.printf "%a@." (Toploop.print_value env (Obj.repr v)) (typ ty)

let dump_file fn =
  let ic = open_in fn in
  let buf = Lexing.from_channel ic in
  if Filename.check_suffix fn ".mli"
  then show_value "Parsetree.signature" (Parse.interface buf)
  else show_value "Parsetree.structure" (Parse.implementation buf);
  close_in ic

let dump_expr s =
  show_value "Parsetree.expression" (Parse.expression (Lexing.from_string s))

let dump_type s =
  show_value "Parsetree.core_type" (Parse.core_type (Lexing.from_string s))

let dump_pattern s =
  show_value "Parsetree.pattern" (Parse.pattern (Lexing.from_string s))

(* Filtering of output *)

module Filter = struct
  open Outcometree

  let hidelocs = ref true
  let hideattrs = ref true

  let map_oval f = function
    | Oval_array l -> Oval_array (List.map f l)
    | Oval_constr (i, l) -> Oval_constr (i, List.map f l)
    | Oval_list l -> Oval_list (List.map f l)
    | Oval_record l -> Oval_record (List.map (fun (s, x) -> (s, f x)) l)
    | Oval_tuple l -> Oval_tuple (List.map f l)
    | Oval_variant (s, Some x) -> Oval_variant (s, Some (f x))
    | x -> x

  let ends_with s l =
    let ll = String.length l and ls = String.length s in
    ll >= ls && String.sub l (ll - ls) ls = s

  let filter_field = function
    | (Oide_ident l, Oval_list [])
      when !hideattrs && ends_with "_attributes" l -> false
    | (Oide_ident l, _)
      when !hidelocs && (ends_with "_loc" l || l = "loc") -> false
    | _ -> true

  let rec filter_val x =
    match map_oval filter_val x with
    | Oval_record l -> Oval_record (List.filter filter_field l)
    | x -> x

  let () =
    let old = !Oprint.out_value in
    Oprint.out_value := (fun ppf v -> old ppf (filter_val v))
end

(* Command-line parsing *)

let args =
  let open Arg in
  [
   "-noopen", Clear autoopen,
   " Don't assume that default modules are opened";

   "-locs", Clear Filter.hidelocs,
   " Keep locations";

   "-emptyattrs", Clear Filter.hideattrs,
   " Keep empty attributes";

   "-e", String dump_expr,
   "<expr> Dump the AST for <expr>";

   "-t", String dump_type,
   "<type> Dump the AST for <type>";

   "-p", String dump_pattern,
   "<pattern> Dump the AST for <pattern>";

   "-w", Int Format.set_margin,
   "<width> Define the width (in characters) of the output"
  ]

let usage = "dump_ast [options] <.ml/.mli source files>\n"

let () =
  let dir = Filename.dirname Sys.argv.(0) in
  let c0 = Filename.concat dir in
  let c1 = Filename.concat Config.standard_library in

  if Sys.file_exists (c1 "compiler-libs/parsetree.cmi")
  then Config.load_path := [c1 "compiler-libs"; Config.standard_library]
  else if Sys.file_exists (c0 "../parsing/parsetree.cmi")
  (* Running from a source tree. *)
  then Config.load_path := [c0 "../parsing"; c0 "../stdlib"]
  else (prerr_endline "Cannot locate parsetree.cmi"; exit 2);

  Toploop.initialize_toplevel_env ();
  Toploop.max_printer_depth := max_int;
  Toploop.max_printer_steps := max_int;

  try Arg.parse (Arg.align args) dump_file usage
  with exn -> Errors.report_error Format.err_formatter exn
