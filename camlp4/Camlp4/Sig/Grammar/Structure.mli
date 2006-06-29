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

type assoc =
  [ NonA
  | RightA
  | LeftA ];

type position =
  [ First
  | Last
  | Before of string
  | After of string
  | Level of string ];

(** Common signature for {!Sig.Grammar.Static} and {!Sig.Grammar.Dynamic}. *)
module type S = sig
  module Loc    : Loc.S;
  module Action : Action.S;
  module Token  : Token.S with module Loc = Loc;

  type gram = 'abstract;
  type internal_entry = 'abstract;
  type tree = 'abstract;

  type token_pattern = ((Token.t -> bool) * string);

  type symbol =
    [ Smeta of string and list symbol and Action.t
    | Snterm of internal_entry
    | Snterml of internal_entry and string
    | Slist0 of symbol
    | Slist0sep of symbol and symbol
    | Slist1 of symbol
    | Slist1sep of symbol and symbol
    | Sopt of symbol
    | Sself
    | Snext
    | Stoken of token_pattern
    | Skeyword of string
    | Stree of tree ];

  type production_rule = (list symbol * Action.t);
  type single_extend_statment =
    (option string * option assoc * list production_rule);
  type extend_statment =
    (option position * list single_extend_statment);
  type delete_statment = list symbol;

  type fold 'a 'b 'c =
    internal_entry -> list symbol ->
      (Stream.t 'a -> 'b) -> Stream.t 'a -> 'c;

  type foldsep 'a 'b 'c =
    internal_entry -> list symbol ->
      (Stream.t 'a -> 'b) -> (Stream.t 'a -> unit) -> Stream.t 'a -> 'c;

end;
