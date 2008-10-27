(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007   Institut National de Recherche  en  Informatique et   *)
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

module Make (Structure : Structure.S) : sig
  open Structure;
  open Context;
  value add_loc :
    Context.t -> Loc.t -> (Context.t -> 'a -> 'b) -> 'a -> ('b * Loc.t);
  value level_number : internal_entry -> string -> int;
  value strict_parsing : ref bool;
  value strict_parsing_warning : ref bool;
  value top_symb :
    internal_entry -> symbol -> symbol;
  value top_tree :
    internal_entry -> tree -> tree;
  value entry_of_symb :
    internal_entry -> symbol -> internal_entry;
  value continue :
    internal_entry -> Loc.t -> Action.t -> symbol -> Context.t -> tree ->
    (Stream.t (Token.t * Loc.t) -> Action.t) -> Stream.t (Token.t * Loc.t) -> Action.t;
  value do_recover :
    (internal_entry -> 'a -> 'b -> tree -> Context.t -> Stream.t (Token.t * Loc.t) -> Action.t) -> internal_entry ->
    'a -> 'b -> Loc.t -> Action.t -> symbol -> Context.t -> tree -> Stream.t (Token.t * Loc.t) -> Action.t;
  value recover :
    (internal_entry -> 'a -> 'b -> tree -> Context.t -> Stream.t (Token.t * Loc.t) -> Action.t) -> internal_entry ->
    'a -> 'b -> Loc.t -> Action.t -> symbol -> Context.t -> tree -> Stream.t (Token.t * Loc.t) -> Action.t;
  value parser_of_tree :
    internal_entry -> int -> int -> tree -> Context.t -> Stream.t (Token.t * Loc.t) -> Action.t;
  value parser_cont :
    (Context.t -> Stream.t (Token.t * Loc.t) -> Action.t) -> internal_entry -> int -> int -> symbol -> tree ->
    Context.t -> Loc.t -> Action.t -> Stream.t (Token.t * Loc.t) -> Action.t;
  value parser_of_token_list :
    (Context.t -> Loc.t -> Action.t -> Stream.t (Token.t * Loc.t) -> Action.t) -> list symbol -> Context.t -> Stream.t (Token.t * Loc.t) -> Action.t;
  value parser_of_symbol :
    internal_entry -> int -> symbol -> Context.t -> Stream.t (Token.t * Loc.t) -> Action.t;
  value parse_top_symb' :
    internal_entry -> symbol -> Context.t -> Stream.t (Token.t * Loc.t) -> Action.t;
  value parse_top_symb :
    internal_entry -> symbol -> Stream.t (Token.t * Loc.t) -> Action.t;
  value start_parser_of_levels :
    internal_entry -> int -> list level -> int -> Context.t -> Stream.t (Token.t * Loc.t) -> Action.t;
  value start_parser_of_entry :
    internal_entry -> int -> Context.t -> Stream.t (Token.t * Loc.t) -> Action.t;
  value continue_parser_of_levels :
    internal_entry -> int -> list level -> Context.t -> int -> Loc.t -> 'a -> Stream.t (Token.t * Loc.t) -> Action.t;
  value continue_parser_of_entry :
    internal_entry -> int -> Loc.t -> Action.t -> Context.t -> Stream.t (Token.t * Loc.t) -> Action.t;
end;

