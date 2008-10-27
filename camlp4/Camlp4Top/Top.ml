(* camlp4r q_MLast.cmo *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
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



(* There is a few Obj.magic due to the fact that we no longer have compiler
   files like Parsetree, Location, Longident but Camlp4_import that wrap them to
   avoid name clashing. *)
module Toploop : sig
  value print_location :
    Format.formatter -> Camlp4_import.Location.t -> unit;
  value print_warning :
    Camlp4_import.Location.t -> Format.formatter -> Camlp4_import.Warnings.t -> unit;
  value parse_toplevel_phrase :
    ref (Lexing.lexbuf -> Camlp4_import.Parsetree.toplevel_phrase);
  value parse_use_file :
    ref (Lexing.lexbuf -> list Camlp4_import.Parsetree.toplevel_phrase);
end = struct
  value print_location fmt loc =
    Toploop.print_location fmt (Obj.magic loc);
  value parse_toplevel_phrase =
    Obj.magic Toploop.parse_toplevel_phrase;
  value parse_use_file =
    Obj.magic Toploop.parse_use_file;
  value print_warning loc fmt w =
    Toploop.print_warning (Obj.magic loc) fmt (Obj.magic w);
end;

open Camlp4_import.Parsetree;
open Lexing;
open Camlp4;
open PreCast;
open Syntax;
open Camlp4.Sig;
module Ast2pt = Camlp4.Struct.Camlp4Ast2OCamlAst.Make Ast;
module Lexer = Camlp4.Struct.Lexer.Make Token;

external not_filtered : 'a -> Gram.not_filtered 'a = "%identity";

value initialization = lazy begin
  if Sys.interactive.val
    then Format.printf "\tCamlp4 Parsing version %s\n@." Camlp4_config.version
    else ()
end;

value lookup x xs = try Some (List.assq x xs) with [ Not_found -> None ];

value wrap parse_fun =
  let token_streams = ref [] in
  let cleanup lb =
    try token_streams.val := List.remove_assq lb token_streams.val
    with [ Not_found -> () ]
  in
  fun lb ->
    let () = Lazy.force initialization in
    let () = Register.iter_and_take_callbacks (fun (_, f) -> f ()) in
    let token_stream =
      match lookup lb token_streams.val with
      [ None ->
        let not_filtered_token_stream = Lexer.from_lexbuf lb in
        let token_stream = Gram.filter (not_filtered not_filtered_token_stream) in
        do { token_streams.val := [ (lb,token_stream) :: token_streams.val ]; token_stream }
      | Some token_stream -> token_stream ]
    in try
      match token_stream with parser
      [ [: `(EOI, _) :] -> raise End_of_file
      | [: :] -> parse_fun token_stream ]
    with
    [ End_of_file | Sys.Break | (Loc.Exc_located _ (End_of_file | Sys.Break))
        as x -> (cleanup lb; raise x)
    | x ->
        let x =
          match x with
          [ Loc.Exc_located loc x -> do { 
            Toploop.print_location Format.err_formatter
              (Loc.to_ocaml_location loc);
            x }
          | x -> x ]
        in
        do {
          cleanup lb;
          Format.eprintf "@[<0>%a@]@." Camlp4.ErrorHandler.print x;
          raise Exit
        } ];

value toplevel_phrase token_stream =
  match Gram.parse_tokens_after_filter Syntax.top_phrase token_stream with
    [ Some str_item ->
	let str_item =
	  AstFilters.fold_topphrase_filters (fun t filter -> filter t) str_item
	in
	Ast2pt.phrase str_item

    | None -> raise End_of_file ];

value use_file token_stream =
  let (pl0, eoi) =
    loop () where rec loop () =
      let (pl, stopped_at_directive) =
        Gram.parse_tokens_after_filter Syntax.use_file token_stream
      in
      if stopped_at_directive <> None then
        match pl with
        [ [ <:str_item< #load $str:s$ >> ] ->
            do { Topdirs.dir_load Format.std_formatter s; loop () }
        | [ <:str_item< #directory $str:s$ >> ] ->
            do { Topdirs.dir_directory s; loop () }
        | _ -> (pl, False) ]
      else (pl, True)
  in
  let pl =
    if eoi then []
    else
      loop () where rec loop () =
        let (pl, stopped_at_directive) =
          Gram.parse_tokens_after_filter Syntax.use_file token_stream
        in
        if stopped_at_directive <> None then pl @ loop () else pl
  in List.map Ast2pt.phrase (pl0 @ pl);

Toploop.parse_toplevel_phrase.val := wrap toplevel_phrase;

Toploop.parse_use_file.val := wrap use_file;

current_warning.val :=
  fun loc txt ->
    Toploop.print_warning (Loc.to_ocaml_location loc) Format.err_formatter
      (Camlp4_import.Warnings.Camlp4 txt);

Register.iter_and_take_callbacks (fun (_, f) -> f ());
