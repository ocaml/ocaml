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
module Make (Lexer : Sig.Lexer)
: Sig.Grammar.Dynamic with module Loc = Lexer.Loc
                         and module Token = Lexer.Token
= struct
  module Structure = Structure.Make Lexer;
  module Delete    = Delete.Make    Structure;
  module Insert    = Insert.Make    Structure;
  module Entry     = Entry.Make     Structure;
  module Fold      = Fold.Make Structure;
  include Structure;

  value mk () =
    let gkeywords = Hashtbl.create 301 in
    {
      gkeywords = gkeywords;
      gfilter = Token.Filter.mk (Hashtbl.mem gkeywords);
      glexer = Lexer.mk ();
      warning_verbose = ref True; (* FIXME *)
      error_verbose = Camlp4_config.verbose
    };

  value get_filter g = g.gfilter;

  value lex g loc cs = g.glexer loc cs;

  value lex_string g loc str = lex g loc (Stream.of_string str);

  value filter g ts = Token.Filter.filter g.gfilter ts;

  value parse_tokens_after_filter entry ts = Entry.parse_tokens_after_filter entry ts;

  value parse_tokens_before_filter entry ts = parse_tokens_after_filter entry (filter entry.egram ts);

  value parse entry loc cs = parse_tokens_before_filter entry (lex entry.egram loc cs);

  value parse_string entry loc str =
    parse_tokens_before_filter entry (lex_string entry.egram loc str);

  value delete_rule = Delete.delete_rule;

  value srules e rl =
    let t =
      List.fold_left
      (fun tree (symbols, action) -> Insert.insert_tree e symbols action tree)
      DeadEnd rl
    in
    Stree t;
  value sfold0 = Fold.sfold0;
  value sfold1 = Fold.sfold1;
  value sfold0sep = Fold.sfold0sep;
  (* value sfold1sep = Fold.sfold1sep; *)

  value extend = Insert.extend;
end;
