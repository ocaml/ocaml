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

(** Signature for Camlp4 grammars. Here the static means that there is only
    one grammar value by grammar module. If you do not need to store the grammar
    value it's preferable to use a static one. *)
module type S = sig
  include Structure.S;

  module Entry : sig
    (** The abstract type of grammar entries. The type parameter is the type
        of the semantic actions that are associated with this entry. *)
    type t 'a = 'abstract;

    (** Make a new entry from the given name. *)
    value mk : string -> t 'a;

    (** Make a new entry from a name and an hand made token parser. *)
    value of_parser :
      string -> (Stream.t (Token.t * Loc.t) -> 'a) -> t 'a;

    (** Clear the entry and setup this parser instead. *)
    value setup_parser :
      t 'a -> (Stream.t (Token.t * Loc.t) -> 'a) -> unit;

    (** Get the entry name. *)
    value name : t 'a -> string;

    (** Print the given entry into the given formatter. *)
    value print : Format.formatter -> t 'a -> unit;

    (** Same as {!print} but show the left-factorization. *)
    value dump : Format.formatter -> t 'a -> unit;

    (*/*)
    value obj : t 'a -> internal_entry;
    value clear : t 'a -> unit;
  end;

  (** Get the {!Token.Filter} associated to the grammar module. *)
  value get_filter : unit -> Token.Filter.t;

  type not_filtered 'a = 'abstract;

  (** This function is called by the EXTEND ... END syntax. *)
  value extend      : Entry.t 'a -> extend_statment -> unit;

  (** The delete rule. *)
  value delete_rule : Entry.t 'a -> delete_statment -> unit;
  value srules      : Entry.t 'a -> list (list symbol * Action.t) -> symbol;

  (** Use the lexer to produce a non filtered token stream from a char stream. *)
  value lex : Loc.t -> Stream.t char
                    -> not_filtered (Stream.t (Token.t * Loc.t));
  (** Token stream from string. *)
  value lex_string : Loc.t -> string
                           -> not_filtered (Stream.t (Token.t * Loc.t));

  (** Filter a token stream using the {!Token.Filter} module *)
  value filter : not_filtered (Stream.t (Token.t * Loc.t))
                            -> Stream.t (Token.t * Loc.t);

  (** Lex, filter and parse a stream of character. *)
  value parse : Entry.t 'a -> Loc.t -> Stream.t char -> 'a;

  (** Same as {!parse} but from a string. *)
  value parse_string : Entry.t 'a -> Loc.t -> string -> 'a;

  (** Parse a token stream that is not filtered yet. *)
  value parse_tokens_before_filter :
    Entry.t 'a -> not_filtered (Stream.t (Token.t * Loc.t)) -> 'a;

  (** Parse a token stream that is already filtered. *)
  value parse_tokens_after_filter :
    Entry.t 'a -> Stream.t (Token.t * Loc.t) -> 'a;

end;
