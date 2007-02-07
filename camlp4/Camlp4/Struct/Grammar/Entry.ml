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

module Make (Structure : Structure.S) = struct
  module Dump  = Print.MakeDump Structure;
  module Print = Print.Make Structure;
  module Tools = Tools.Make Structure;
  open Format;
  open Structure;

  type t 'a = internal_entry;

  value name e = e.ename;

  value print ppf e = fprintf ppf "%a@\n" Print.entry e;
  value dump ppf e = fprintf ppf "%a@\n" Dump.entry e;

  (* value find e s = Find.entry e s; *)

  value mk g n =
    { egram = g;
      ename = n;
      estart = Tools.empty_entry n;
      econtinue _ _ _ _ = parser [];
      edesc = Dlevels [] };

  value action_parse entry ts : Action.t =
    Context.call_with_ctx ts
      (fun c ->
         try entry.estart 0 c (Context.stream c) with
         [ Stream.Failure ->
             Loc.raise (Context.loc_ep c)
               (Stream.Error ("illegal begin of " ^ entry.ename))
         | Loc.Exc_located _ _ as exc -> raise exc
         | exc -> Loc.raise (Context.loc_ep c) exc ]);

  value lex entry loc cs = entry.egram.glexer loc cs;

  value lex_string entry loc str = lex entry loc (Stream.of_string str);

  value filter entry ts = Token.Filter.filter (get_filter entry.egram) ts;

  value parse_tokens_after_filter entry ts = Action.get (action_parse entry ts);

  value parse_tokens_before_filter entry ts = parse_tokens_after_filter entry (filter entry ts);

  value parse entry loc cs = parse_tokens_before_filter entry (lex entry loc cs);

  value parse_string entry loc str =
    parse_tokens_before_filter entry (lex_string entry loc str);

  value of_parser g n (p : Stream.t (Token.t * Loc.t) -> 'a) : t 'a =
    { egram = g;
      ename = n;
      estart _ _ ts = Action.mk (p ts);
      econtinue _ _ _ _ = parser [];
      edesc = Dparser (fun ts -> Action.mk (p ts)) };

  value setup_parser e (p : Stream.t (Token.t * Loc.t) -> 'a) =
    let f ts = Action.mk (p ts) in do {
      e.estart := fun _ _ -> f;
      e.econtinue := fun _ _ _ _ -> parser [];
      e.edesc := Dparser f
    };

  value clear e =
    do {
      e.estart := fun _ _ -> parser [];
      e.econtinue := fun _ _ _ _ -> parser [];
      e.edesc := Dlevels []
    };

  value obj x = x;

end;
