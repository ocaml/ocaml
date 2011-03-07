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
  value add_loc :
    Loc.t -> (token_stream -> 'b) -> token_stream -> ('b * Loc.t);
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
    internal_entry -> Loc.t -> Action.t -> symbol -> tree -> efun -> efun;
  value do_recover :
    (internal_entry -> 'a -> 'b -> tree -> efun) -> internal_entry ->
    'a -> 'b -> Loc.t -> Action.t -> symbol -> tree -> efun;
  value recover :
    (internal_entry -> 'a -> 'b -> tree -> efun) -> internal_entry ->
    'a -> 'b -> Loc.t -> Action.t -> symbol -> tree -> efun;
  value parser_of_tree :
    internal_entry -> int -> int -> tree -> efun;
  value parser_cont :
    efun -> internal_entry -> int -> int -> symbol -> tree -> Loc.t -> Action.t -> efun;
  value parser_of_token_list :
    (Loc.t -> Action.t -> efun) -> list symbol -> efun;
  value parser_of_symbol :
    internal_entry -> int -> symbol -> efun;
  value parse_top_symb :
    internal_entry -> symbol -> efun;
  value start_parser_of_levels :
    internal_entry -> int -> list level -> int -> efun;
  value start_parser_of_entry :
    internal_entry -> int -> efun;
  value continue_parser_of_levels :
    internal_entry -> int -> list level -> int -> Loc.t -> 'a -> efun;
  value continue_parser_of_entry :
    internal_entry -> int -> Loc.t -> Action.t -> efun;
end;
