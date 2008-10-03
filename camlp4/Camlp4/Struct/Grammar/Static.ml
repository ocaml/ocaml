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

value uncurry f (x,y) = f x y;
value flip f x y = f y x;

module Make (Lexer : Sig.Lexer)
: Sig.Grammar.Static with module Loc = Lexer.Loc
                        and module Token = Lexer.Token
= struct
  module Structure = Structure.Make Lexer;
  module Delete = Delete.Make Structure;
  module Insert = Insert.Make Structure;
  module Fold = Fold.Make Structure;
  include Structure;

  value gram =
    let gkeywords = Hashtbl.create 301 in
    {
      gkeywords = gkeywords;
      gfilter = Token.Filter.mk (Hashtbl.mem gkeywords);
      glexer = Lexer.mk ();
      warning_verbose = ref True; (* FIXME *)
      error_verbose = Camlp4_config.verbose
    };

  module Entry = struct
    module E = Entry.Make Structure;
    type t 'a = E.t 'a;
    value mk = E.mk gram;
    value of_parser name strm = E.of_parser gram name strm;
    value setup_parser = E.setup_parser;
    value name = E.name;
    value print = E.print;
    value clear = E.clear;
    value dump = E.dump;
    value obj x = x;
  end;

  value get_filter () = gram.gfilter;

  value lex loc cs = gram.glexer loc cs;

  value lex_string loc str = lex loc (Stream.of_string str);

  value filter ts = Token.Filter.filter gram.gfilter ts;

  value parse_tokens_after_filter entry ts = Entry.E.parse_tokens_after_filter entry ts;

  value parse_tokens_before_filter entry ts = parse_tokens_after_filter entry (filter ts);

  value parse entry loc cs = parse_tokens_before_filter entry (lex loc cs);

  value parse_string entry loc str = parse_tokens_before_filter entry (lex_string loc str);

  value delete_rule = Delete.delete_rule;

  value srules e rl =
    Stree (List.fold_left (flip (uncurry (Insert.insert_tree e))) DeadEnd rl);
  value sfold0 = Fold.sfold0;
  value sfold1 = Fold.sfold1;
  value sfold0sep = Fold.sfold0sep;
  (* value sfold1sep = Fold.sfold1sep; *)

  value extend = Insert.extend;

end;
