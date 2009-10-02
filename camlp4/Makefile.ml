(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)
open YaM
open Format

let getenv var default =
  try Sys.getenv var
  with Not_found ->
    default

let libdir_camlp4 = (getenv "LIBDIR" Camlp4_config.libdir) ^ "/camlp4/."

let bindir = (getenv "BINDIR" Camlp4_config.bindir) ^ "/."

(**
let unixlib =
  match Sys.os_type with
  | "Win32" -> "../otherlibs/win32unix"
  | _       -> "../otherlibs/unix"
**)
let ocamlrun = "../boot/ocamlrun" (* " -I " ^ unixlib *)
let ocamlrun_os =
  Filename.concat Filename.parent_dir_name
                  (Filename.concat "boot" "ocamlrun")
(*  ^ " -I " ^ unixlib *)

let ocaml = ocamlrun ^ " ../ocaml -I ../stdlib" (* "-I " ^ unixlib *)

let debug_mode =
  (* true *)
  false

let camlp4_modules =
  [
    ocamlrun_os;
    "./boot/camlp4boot";
  ]
let camlp4_modules =
  if debug_mode then "env STATIC_CAMLP4_DEBUG=\\*" :: camlp4_modules
  else camlp4_modules

let debug_opt x = if debug_mode && Sys.file_exists x then [x] else []
let filter_opt x = if Sys.file_exists x then [x] else []

let camlp4boot = "'" ^ String.concat " " camlp4_modules ^ "'"
let camlp4boot_may_debug mods =
  let camlp4_modules = camlp4_modules @
    debug_opt "./boot/ExceptionTracer.cmo" @
    filter_opt "./boot/Profiler.cmo" @ mods
  in "'" ^ String.concat " " camlp4_modules ^ "'"

let ocamlc =
  best ["../ocamlc.opt", "../ocamlc.opt";
        "../ocamlc",     ocamlrun ^^ "../ocamlc";
        "",              "echo no byte compiler available && false"]
let ocamlopt =
  best ["../ocamlopt.opt", "../ocamlopt.opt";
        "../ocamlopt",     ocamlrun ^^ "../ocamlopt";
        "",                "echo no native compiler available && false"]

let () =
  !options.ocamlc       := ocamlc ^^ " -nostdlib -I ../stdlib";
  !options.ocamlopt     := ocamlopt ^^ " -nostdlib -I ../stdlib";
  !options.ocamldoc     := ocamlrun ^^ "../ocamldoc/ocamldoc";
  !options.ocamlyacc    := ocamlrun ^^ "../boot/ocamlyacc";
  !options.ocamllex     := ocamlrun ^^ "../boot/ocamllex";
  !options.ocamldep     := ocamlrun ^^ "../tools/ocamldep";
  ()

let options_without_camlp4 = new_scope (lazy !options)

let windows = Sys.os_type = "Win32"

let may_define_unix = if windows then [] else ["-D UNIX"]

let () =
  !options.ocaml_Flags ^= "-w Ale -warn-error Ale"^^
                            (if getenv "DTYPES" "" <> "" then "-annot"
                             else "");
  !options.ocaml_P4     := camlp4boot_may_debug may_define_unix;
  !options.ocaml_P4_opt := camlp4boot_may_debug ("-D OPT" :: may_define_unix);
  ()

let options_without_debug () = { (!options) with ocaml_P4     = ref camlp4boot
                                               ; ocaml_P4_opt = ref camlp4boot }

let parsing = "../parsing"
and typing = "../typing"
and toplevel = "../toplevel"
and utils = "../utils"
and dynlink = "../otherlibs/dynlink"
and unix =
  match Sys.os_type with
  | "Win32" -> "../otherlibs/win32unix"
  | _       -> "../otherlibs/unix"
and build = "build"

let ocaml_Module_with_genmap =
  generic_ocaml_Module_extension ".genmap.ml"
   (fun _ i o ->
      "if test ! -e"^^o^^
         "|| ( test -e ./camlp4boot.run"^^
             "&& test -e Camlp4Filters/GenerateMap.cmo"^^
             "&& test -e Camlp4Filters/GenerateFold.cmo"^^
             "&& test -e Camlp4Filters/MetaGenerator.cmo"^^
             "&& test -e Camlp4Filters/RemoveTrashModule.cmo ) ;"^^
      "then ( echo 'module Camlp4FiltersTrash = struct' ;"^^
             "cat Camlp4/Sig/Camlp4Ast.ml ; echo 'end;' ) > Camlp4/Struct/Camlp4Ast.tmp.ml ;"^^
             "( echo '(* Generated file! Do not edit by hand *)' ;"^^
                "../boot/ocamlrun ./camlp4boot.run"^^
                   "./Camlp4Filters/GenerateMap.cmo"^^
                   "./Camlp4Filters/GenerateFold.cmo"^^
                   "./Camlp4Filters/MetaGenerator.cmo"^^
                   "./Camlp4Filters/RemoveTrashModule.cmo -printer OCamlr"^^
                   i^^" -no_comments ) >"^^o^^"; else : ; fi")

let camlp4_package_as_one_dir =
  ocaml_PackageDir "Camlp4" (lazy [
    ocaml_IModule ~includes:[build] "Config";
    ocaml_IModule ~o:(options_without_debug ()) "Debug";
    ocaml_IModule "Options";
    ocaml_PackageDir "Sig" (lazy [
      ocaml_Interface "Id";
      ocaml_Interface ~ext_includes:[parsing] "Loc";
      ocaml_Interface "Error";
      ocaml_Interface "Warning";
      ocaml_Interface "Type";
      ocaml_Interface "Token";
      ocaml_Interface "Lexer";
      ocaml_PackageDir "Grammar" (lazy [
        ocaml_Interface "Action";
        ocaml_Interface "Structure";
        ocaml_Interface "Dynamic";
        ocaml_Interface "Static"
      ]);
      ocaml_IModule "Mapper";
      ocaml_Interface "Ast";
      ocaml_Module "Camlp4Ast";
      ocaml_Interface "Quotation";
      ocaml_Interface "Camlp4Token";
      ocaml_Interface "DynLoader";
      ocaml_Interface "AntiquotSyntax";
      ocaml_Interface "Parser";
      ocaml_Interface "Printer";
      ocaml_Interface "Syntax";
      ocaml_Interface "Camlp4Syntax";
      ocaml_Interface "AstFilters";
      ocaml_Interface "SyntaxExtension";
    ]);
    ocaml_IModule "ErrorHandler";
    ocaml_PackageDir "Struct" (lazy [
      ocaml_IModule ~ext_includes:[parsing] "Loc";
      ocaml_Module  "Warning";
      ocaml_IModule "EmptyError";
      ocaml_IModule "EmptyPrinter";
      ocaml_IModule "Token";
      ocaml_Lexer ~includes:[utils] ~ext_includes:[parsing] ~pp:"" "Lexer";
      ocaml_PackageDir "Grammar" (lazy [
        ocaml_Module "Context";
        ocaml_Module "Structure";
        ocaml_Module "Search";
        ocaml_Module "Tools";
        ocaml_IModule "Print";
        ocaml_Module "Failed";
        ocaml_Module "Parser";
        ocaml_IModule "Fold";
        ocaml_Module "Insert";
        ocaml_Module "Delete";
        ocaml_Module "Entry";
        ocaml_Module "Find";
        ocaml_Module "Dynamic";
        ocaml_Module "Static"
      ]);
      ocaml_Module "Quotation";
      ocaml_IModule ~ext_includes:[dynlink] "DynLoader";
      ocaml_Module_with_genmap ~flags:"-w z -warn-error z" "Camlp4Ast";
      ocaml_IModule "FreeVars";
      ocaml_Module "AstFilters";
      ocaml_IModule ~ext_includes:[parsing] "Camlp4Ast2OCamlAst";
      ocaml_Module "CleanAst";
      ocaml_IModule "CommentFilter";
    ]);
    ocaml_Module "OCamlInitSyntax";
    ocaml_PackageDir "Printers" (lazy [
      ocaml_IModule "Null";
      ocaml_IModule "DumpOCamlAst";
      ocaml_IModule "DumpCamlp4Ast";
      ocaml_IModule "OCaml";
      ocaml_IModule "OCamlr" ~flags:"-w v -warn-error v";
      (* ocaml_IModule "OCamlrr"; *)
    ]);
    ocaml_IModule "PreCast";
    ocaml_IModule "Register"
  ])

let camlp4_parsers =
  ocaml_PackageDir "Camlp4Parsers" (lazy [
    ocaml_Module "OCamlr";
    ocaml_Module "OCaml";
    (* ocaml_Module "OCamlrr"; *)
    ocaml_Module "OCamlQuotationBase";
    ocaml_Module "OCamlQuotation";
    ocaml_Module "OCamlRevisedQuotation";
    ocaml_Module "OCamlOriginalQuotation";
    ocaml_Module "OCamlRevisedParser";
    ocaml_Module "OCamlParser";
    ocaml_Module "Grammar";
    ocaml_Module "Macro";
    ocaml_Module ~o:(options_without_debug ()) "Debug";
    ocaml_Module "LoadCamlp4Ast";
  ])

let camlp4_printers =
  ocaml_PackageDir "Camlp4Printers" (lazy [
    ocaml_Module "DumpOCamlAst";
    ocaml_Module "DumpCamlp4Ast";
    ocaml_Module "OCaml";
    ocaml_Module "OCamlr";
    (* ocaml_Module "OCamlrr"; *)
    ocaml_Module "Null";
    ocaml_Module ~ext_includes:[unix] "Auto";
  ])

let camlp4_filters =
  ocaml_PackageDir "Camlp4Filters" (lazy [
    ocaml_Module "ExceptionTracer";
    ocaml_Module "Tracer";
    ocaml_Module "StripLocations";
    ocaml_Module "LiftCamlp4Ast";
    ocaml_Module "GenerateMap";
    ocaml_Module "GenerateFold";
    ocaml_Module "MetaGenerator";
    ocaml_Module "RemoveTrashModule";
    ocaml_Module "Profiler";
  ])

let camlp4_top =
  ocaml_PackageDir "Camlp4Top" (lazy [
    ocaml_Module ~ext_includes:[toplevel; typing; parsing] "Rprint";
    ocaml_Module ~ext_includes:[toplevel; parsing; utils] "Camlp4Top";
  ])

let split c s =
  let rec self acc s =
    try
      let pos = String.rindex s c in
      let x = String.sub s 0 pos
      and y = String.sub s (pos + 1) (String.length s - pos - 1)
      in self (y :: acc) x
    with Not_found -> s :: acc
  in self [] s
let chop_end c s =
  let pos = String.rindex s c in
  String.sub s (pos + 1) (String.length s - pos - 1)
let file ppf f =
  let cin = open_in f in
  let rec loop () =
    pp_print_string ppf (input_line cin);
    fprintf ppf "@\n";
    loop ()
  in try loop () with End_of_file -> ()
let ext_split f = split '.' f
  
  
let print_packed_sources ppf ?(skip = fun _ -> false) package_dir =
  let _ =
  fold_units_sources [package_dir] (fun name sources k (skip, inside) ->
    eprintf "%s: [%s] (%b)@." name (String.concat "; " sources) inside;
    let name = try chop_end '/' name with Not_found -> name in
    let k () = ignore (k (skip, true)) in
    if not inside then k () else (
      if skip name then fprintf ppf "(**/**)@\n" else fprintf ppf "(** *)@\n";
      fprintf ppf "@[<2>module %s " name;
      let (mli, ml, mll, k) =
        List.fold_right
          (fun x (mli, ml, mll, k) ->
            match ext_split x with
            | [_; "mli"] -> (Some x, ml, mll, k)
            | [_; "ml"]  -> (mli, Some x, mll, k)
            | [_; "mll"] -> (mli, ml, Some x, k)
            | [x; "meta"; "ml"] -> (mli, Some (x^".ml"), mll, fun () -> ())
            | [x; "genmap"; "ml"] -> (mli, Some (x^".ml"), mll, fun () -> ())
            | [_; ext] -> failwith ("unknown extension " ^ ext)
            | _ -> failwith ("bad file "^x))
          sources (None, None, None, k) in
      (match (ml, mll, mli) with
      | (None, None, Some mli) -> fprintf ppf "=@ @[<2>struct@\n"
      | (_, _, Some mli) -> fprintf ppf ":@,@[<2>sig@\n%a@]@\nend@\n" file mli;
                            fprintf ppf "=@ @[<2>struct@\n"
      | _ -> fprintf ppf "=@ @[<2>struct@\n");
      (match (ml, mll, mli) with
      | (_, Some mll, _) ->
            fprintf ppf "(*___CAMLP4_LEXER___ %a ___CAMLP4_LEXER___*)@\n();"
            file (String.sub mll 0 (String.length mll - 1))
      | (Some ml, _, _) -> k (); fprintf ppf "%a" file ml
      | (None, _, Some mli) -> k (); fprintf ppf "%a" file mli
      | _ -> if sources <> [] then () else k ());
      fprintf ppf "@]@\nend;@]@\n";
      if skip name then fprintf ppf "(**/**)@\n";
    );
    (skip, inside)
  ) (skip, false) in fprintf ppf "@."

let run l =
  let cmd = String.concat " " l in
  let () = Format.printf "%s@." cmd in
  let st = YaM.call cmd in
  if st <> 0 then failwith ("Exit: " ^ string_of_int st)

let sed re str file =
  run ["sed"; "-i"; "-e"; "'s/"^re^"/"^str^"/'"; file]

let pack () =
  let revised_to_ocaml f =
    run ["./boot/camlp4boot -printer OCaml -o "^f^".ml -impl "^f^".ml4"] in
  let ppf_of_file f = formatter_of_out_channel (open_out f) in
  let skip_struct = function "Struct" -> true | _ -> false in
  print_packed_sources (ppf_of_file "Camlp4.ml4")
                       ~skip:skip_struct camlp4_package_as_one_dir;
  print_packed_sources (ppf_of_file "Camlp4Parsers.ml4") camlp4_parsers;
  print_packed_sources (ppf_of_file "Camlp4Printers.ml4") camlp4_printers;
  print_packed_sources (ppf_of_file "Camlp4Filters.ml4") camlp4_filters;
  print_packed_sources (ppf_of_file "Camlp4Top.ml4") camlp4_top;
  revised_to_ocaml "Camlp4";
  sed "(\\*___CAMLP4_LEXER___" "" "Camlp4.ml";
  sed "___CAMLP4_LEXER___\\*)" "" "Camlp4.ml";
  sed "^ *# [0-9]\\+.*$" "" "Camlp4.ml";
  revised_to_ocaml "Camlp4Parsers";
  revised_to_ocaml "Camlp4Printers";
  revised_to_ocaml "Camlp4Filters";
  revised_to_ocaml "Camlp4Top"

let just_doc () =
  run ["cd doc && ../../ocamldoc/ocamldoc";
       "-v -short-functors -html";
       "-I ../../parsing -I ../build -I ../../utils -I ..";
       "-dump ocamldoc.out";
       "-t 'Camlp4 a Pre-Processor-Pretty-Printer for Objective Caml'";
       "../Camlp4.ml"; "../Camlp4Parsers.ml"; "../Camlp4Printers.ml";
       "../Camlp4Filters.ml"; "../Camlp4Top.ml"]

let doc () =
  pack (); just_doc ()

