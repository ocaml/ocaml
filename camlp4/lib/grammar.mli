(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Grammar]: extensible grammars *)

(* This module implements the Camlp4 extensible grammars system.
   Grammars entries can be extended using the [EXTEND] statement,
   added by loading the Camlp4 [pa_extend.cmo] file. *)

type g = 'x;
    (* The type for grammars, holding entries. *)
value gcreate : Token.glexer Token.t -> g;
    (* Create a new grammar, without keywords, using the lexer given
       as parameter. *)
value tokens : g -> string -> list (string * int);
    (* Given a grammar and a token pattern constructor, returns the list of
       the corresponding values currently used in all entries of this grammar.
       The integer is the number of times this pattern value is used.

       Examples:
-      If the associated lexer uses ("", xxx) to represent a keyword
       (what is represented by then simple string xxx in an [EXTEND]
       statement rule), the call [Grammar.token g ""] returns the keywords
       list.
-      The call [Grammar.token g "IDENT"] returns the list of all usages
       of the pattern "IDENT" in the [EXTEND] statements. *)

module Entry :
  sig
    type e 'a = 'x;
    value create : g -> string -> e 'a;
    value parse : e 'a -> Stream.t char -> 'a;
    value parse_token : e 'a -> Stream.t Token.t -> 'a;
    value name : e 'a -> string;
    value of_parser : g -> string -> (Stream.t Token.t -> 'a) -> e 'a;
    value print : e 'a -> unit;
    value find : e 'a -> string -> e Obj.t;
    external obj : e 'a -> Gramext.g_entry Token.t = "%identity";
  end
;
    (* Module to handle entries.
-      * [Entry.e] is the type for entries returning values of type ['a].
-      * [Entry.create g n] creates a new entry named [n] in the grammar [g].
-      * [Entry.parse e] returns the stream parser of the entry [e].
-      * [Entry.parse_token e] returns the token parser of the entry [e].
-      * [Entry.name e] returns the name of the entry [e].
-      * [Entry.of_parser g n p] makes an entry from a token stream parser.
-      * [Entry.print e] displays the entry [e] using [Format].
-      * [Entry.find e s] finds the entry named [s] in [e]'s rules.
-      * [Entry.obj e] converts an entry into a [Gramext.g_entry] allowing
-        to see what it holds ([Gramext] is visible, but not documented). *)

value of_entry : Entry.e 'a -> g;
    (* Return the grammar associated with an entry. *)

value error_verbose : ref bool;
    (* Flag for displaying more information in case of parsing error;
       default = [False] *)

value warning_verbose : ref bool;
    (* Flag for displaying warnings while extension; default = [True] *)

value strict_parsing : ref bool;
    (* Flag to apply strict parsing, without trying to recover errors;
       default = [False] *)

(*** Clearing grammars and entries *)

module Unsafe :
  sig
    value gram_reinit : g -> Token.glexer Token.t -> unit;
    value clear_entry : Entry.e 'a -> unit;
    (* deprecated since version 3.04+6; use rather function gram_reinit *)
    value reinit_gram : g -> Token.lexer -> unit;
  end
;
    (* Module for clearing grammars and entries. To be manipulated with
       care, because: 1) reinitializing a grammar destroys all tokens
       and there may have problems with the associated lexer if it has
       a notion of keywords; 2) clearing an entry does not destroy the
       tokens used only by itself.
-      * [Unsafe.reinit_gram g lex] removes the tokens of the grammar
-        and sets [lex] as a new lexer for [g]. Warning: the lexer
-        itself is not reinitialized.
-      * [Unsafe.clear_entry e] removes all rules of the entry [e]. *)

(*** Functorial interface *)

    (* Alternative for grammars use. Grammars are no more Ocaml values:
       there is no type for them. Modules generated preserve the
       rule "an entry cannot call an entry of another grammar" by
       normal Ocaml typing. *)

module type GLexerType =
  sig
    type te = 'x;
    value lexer : Token.glexer te;
  end;
    (* The input signature for the functor [Grammar.GMake]: [te] is the
       type of the tokens, [tematch] is the way a token is matched against
       a pattern. Must raise [Stream.Failure] if not matched. Warning:
       write the function [tematch] as a function of pattern returning
       a function to each pattern case, not a function of two parameters:
       it may have some performance importance. *)

module type S =
  sig
    type te = 'x;
    type parsable = 'x;
    value parsable : Stream.t char -> parsable;
    value tokens : string -> list (string * int);
    module Entry :
      sig
        type e 'a = 'y;
        value create : string -> e 'a;
        value parse : e 'a -> parsable -> 'a;
        value parse_token : e 'a -> Stream.t te -> 'a;
        value name : e 'a -> string;
        value of_parser : string -> (Stream.t te -> 'a) -> e 'a;
        value print : e 'a -> unit;
        external obj : e 'a -> Gramext.g_entry te = "%identity";
      end
    ;
    module Unsafe :
      sig
        value gram_reinit : Token.glexer te -> unit;
        value clear_entry : Entry.e 'a -> unit;
        (* deprecated since version 3.04+6; use rather gram_reinit *)
        (* warning: reinit_gram fails if used with GMake *)
        value reinit_gram : Token.lexer -> unit;
      end
    ;
    value extend :
      Entry.e 'a -> option Gramext.position ->
        list
          (option string * option Gramext.g_assoc *
           list (list (Gramext.g_symbol te) * Gramext.g_action)) ->
        unit;
    value delete_rule : Entry.e 'a -> list (Gramext.g_symbol te) -> unit;
  end
;
    (* Signature type of the functor [Grammar.Make]. The types and
       functions are almost the same than in generic interface, but:
-      * Grammars are not values. Functions holding a grammar as parameter
-        do not have this parameter yet.
-      * The type [parsable] is used in function [parse] instead of
-        the char stream, avoiding the possible loss of tokens.
-      * The type of tokens (expressions and patterns) can be any
-        type (instead of (string * string)); the module parameter
-        must specify a way to show them as (string * string) *)

module GMake (L : GLexerType) : S with type te = L.te;

value print_entry : Format.formatter -> Gramext.g_entry 'te -> unit;
    (* General printer for all kinds of entries (obj entries) *)

(*--*)

(*** deprecated since version 3.04+6; use rather the functor GMake *)
module type LexerType = sig value lexer : Token.lexer; end;
module Make (L : LexerType) : S with type te = Token.t;
(*** deprecated since version 3.04+6; use rather the function gcreate *)
value create : Token.lexer -> g;

(*** For system use *)

value loc_of_token_interval : int -> int -> (int * int);
value extend :
  list
    (Gramext.g_entry 'te * option Gramext.position *
     list
       (option string * option Gramext.g_assoc *
        list (list (Gramext.g_symbol 'te) * Gramext.g_action))) ->
    unit;
value delete_rule :
  Entry.e 'a -> list (Gramext.g_symbol Token.t) -> unit;
