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
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

module PP = Printers;
open PreCast;

type parser_fun 'a =
  ?directive_handler:('a -> option 'a) -> PreCast.Loc.t -> Stream.t char -> 'a;

type printer_fun 'a =
  ?input_file:string -> ?output_file:string -> 'a -> unit;

value sig_item_parser = ref (fun ?directive_handler:(_) _ _ -> failwith "No interface parser");
value str_item_parser = ref (fun ?directive_handler:(_) _ _ -> failwith "No implementation parser");

value sig_item_printer = ref (fun ?input_file:(_) ?output_file:(_) _ -> failwith "No interface printer");
value str_item_printer = ref (fun ?input_file:(_) ?output_file:(_) _ -> failwith "No implementation printer");

value callbacks = Queue.create ();

value loaded_modules = ref [];

value iter_and_take_callbacks f =
  let rec loop () = loop (f (Queue.take callbacks)) in
  try loop () with [ Queue.Empty -> () ];

value declare_dyn_module m f =
  begin
    (* let () = Format.eprintf "declare_dyn_module: %s@." m in *)
    loaded_modules.val := [ m :: loaded_modules.val ];
    Queue.add (m, f) callbacks;
  end;

value register_str_item_parser f = str_item_parser.val := f;
value register_sig_item_parser f = sig_item_parser.val := f;
value register_parser f g =
  do { str_item_parser.val := f; sig_item_parser.val := g };

value register_str_item_printer f = str_item_printer.val := f;
value register_sig_item_printer f = sig_item_printer.val := f;
value register_printer f g =
  do { str_item_printer.val := f; sig_item_printer.val := g };

module Plugin (Id : Sig.Id) (Maker : functor (Unit : sig end) -> sig end) = struct
  declare_dyn_module Id.name (fun _ -> let module M = Maker (struct end) in ());
end;

module SyntaxExtension (Id : Sig.Id) (Maker : Sig.SyntaxExtension) = struct
  declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
end;

module OCamlSyntaxExtension
  (Id : Sig.Id) (Maker : functor (Syn : Sig.Camlp4Syntax) -> Sig.Camlp4Syntax) =
struct
  declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
end;

module SyntaxPlugin (Id : Sig.Id) (Maker : functor (Syn : Sig.Syntax) -> sig end) = struct
  declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
end;

module Printer
  (Id : Sig.Id) (Maker : functor (Syn : Sig.Syntax)
                                -> (Sig.Printer Syn.Ast).S) =
struct
  declare_dyn_module Id.name (fun _ ->
    let module M = Maker Syntax in
    register_printer M.print_implem M.print_interf);
end;

module OCamlPrinter
  (Id : Sig.Id) (Maker : functor (Syn : Sig.Camlp4Syntax)
                                -> (Sig.Printer Syn.Ast).S) =
struct
  declare_dyn_module Id.name (fun _ ->
    let module M = Maker Syntax in
    register_printer M.print_implem M.print_interf);
end;

module OCamlPreCastPrinter
  (Id : Sig.Id) (P : (Sig.Printer PreCast.Ast).S) =
struct
  declare_dyn_module Id.name (fun _ ->
    register_printer P.print_implem P.print_interf);
end;

module Parser
  (Id : Sig.Id) (Maker : functor (Ast : Sig.Ast)
                                -> (Sig.Parser Ast).S) =
struct
  declare_dyn_module Id.name (fun _ ->
    let module M = Maker PreCast.Ast in
    register_parser M.parse_implem M.parse_interf);
end;

module OCamlParser
  (Id : Sig.Id) (Maker : functor (Ast : Sig.Camlp4Ast)
                                -> (Sig.Parser Ast).S) =
struct
  declare_dyn_module Id.name (fun _ ->
    let module M = Maker PreCast.Ast in
    register_parser M.parse_implem M.parse_interf);
end;

module OCamlPreCastParser
  (Id : Sig.Id) (P : (Sig.Parser PreCast.Ast).S) =
struct
  declare_dyn_module Id.name (fun _ ->
    register_parser P.parse_implem P.parse_interf);
end;

module AstFilter
  (Id : Sig.Id) (Maker : functor (F : Sig.AstFilters) -> sig end) =
struct
  declare_dyn_module Id.name (fun _ -> let module M = Maker AstFilters in ());
end;

sig_item_parser.val := Syntax.parse_interf;
str_item_parser.val := Syntax.parse_implem;

module CurrentParser = struct
  module Ast = Ast;
  value parse_interf ?directive_handler loc strm =
    sig_item_parser.val ?directive_handler loc strm;
  value parse_implem ?directive_handler loc strm =
    str_item_parser.val ?directive_handler loc strm; 
end;

module CurrentPrinter = struct
  module Ast = Ast;
  value print_interf ?input_file ?output_file ast =
    sig_item_printer.val ?input_file ?output_file ast;
  value print_implem ?input_file ?output_file ast =
    str_item_printer.val ?input_file ?output_file ast; 
end;

value enable_ocaml_printer () =
  let module M = OCamlPrinter PP.OCaml.Id PP.OCaml.MakeMore in ();

value enable_ocamlr_printer () =
  let module M = OCamlPrinter PP.OCamlr.Id PP.OCamlr.MakeMore in ();

(* value enable_ocamlrr_printer () =
  let module M = OCamlPrinter PP.OCamlrr.Id PP.OCamlrr.MakeMore in ();    *)

value enable_dump_ocaml_ast_printer () =
  let module M = OCamlPrinter PP.DumpOCamlAst.Id PP.DumpOCamlAst.Make in ();

value enable_dump_camlp4_ast_printer () =
  let module M = Printer PP.DumpCamlp4Ast.Id PP.DumpCamlp4Ast.Make in ();

value enable_null_printer () =
  let module M = Printer PP.Null.Id PP.Null.Make in ();

