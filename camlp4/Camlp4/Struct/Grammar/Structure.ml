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

open Sig.Grammar;

module type S = sig
  module Loc          : Sig.Loc;
  module Token        : Sig.Token with module Loc = Loc;
  module Lexer        : Sig.Lexer
                        with module Loc   = Loc
                         and module Token = Token;
  module Context      : Context.S with module Token = Token;
  module Action       : Sig.Grammar.Action;

  type gram =
    { gfilter         : Token.Filter.t;
      gkeywords       : Hashtbl.t string (ref int);
      glexer          : Loc.t -> Stream.t char -> Stream.t (Token.t * Loc.t);
      warning_verbose : ref bool;
      error_verbose   : ref bool };

  type efun = Context.t -> Stream.t (Token.t * Loc.t) -> Action.t;

  type token_pattern = ((Token.t -> bool) * string);

  type internal_entry =
    { egram     : gram;
      ename     : string;
      estart    : mutable int -> efun;
      econtinue : mutable int -> Loc.t -> Action.t -> efun;
      edesc     : mutable desc }
  and desc =
    [ Dlevels of list level
    | Dparser of Stream.t (Token.t * Loc.t) -> Action.t ]
  and level =
    { assoc   : assoc         ;
      lname   : option string ;
      lsuffix : tree          ;
      lprefix : tree          }
  and symbol =
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
    | Stree of tree ]
  and tree =
    [ Node of node
    | LocAct of Action.t and list Action.t
    | DeadEnd ]
  and node =
    { node    : symbol ;
      son     : tree   ;
      brother : tree   };

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

  (* Accessors *)
  value get_filter : gram -> Token.Filter.t;

  (* Useful functions *)
  value using : gram -> string -> unit;
  value removing : gram -> string -> unit;
end;

module Make (Lexer  : Sig.Lexer) = struct
  module Loc = Lexer.Loc;
  module Token = Lexer.Token;
  module Action : Sig.Grammar.Action = struct
    type  t     = Obj.t   ;
    value mk    = Obj.repr;
    value get   = Obj.obj ;
    value getf  = Obj.obj ;
    value getf2 = Obj.obj ;
  end;
  module Lexer = Lexer;

  type gram =
    { gfilter         : Token.Filter.t;
      gkeywords       : Hashtbl.t string (ref int);
      glexer          : Loc.t -> Stream.t char -> Stream.t (Token.t * Loc.t);
      warning_verbose : ref bool;
      error_verbose   : ref bool };

  module Context = Context.Make Token;

  type efun = Context.t -> Stream.t (Token.t * Loc.t) -> Action.t;

  type token_pattern = ((Token.t -> bool) * string);

  type internal_entry =
    { egram     : gram;
      ename     : string;
      estart    : mutable int -> efun;
      econtinue : mutable int -> Loc.t -> Action.t -> efun;
      edesc     : mutable desc }
  and desc =
    [ Dlevels of list level
    | Dparser of Stream.t (Token.t * Loc.t) -> Action.t ]
  and level =
    { assoc   : assoc         ;
      lname   : option string ;
      lsuffix : tree          ;
      lprefix : tree          }
  and symbol =
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
    | Stree of tree ]
  and tree =
    [ Node of node
    | LocAct of Action.t and list Action.t
    | DeadEnd ]
  and node =
    { node    : symbol ;
      son     : tree   ;
      brother : tree   };

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

  value get_filter g = g.gfilter;

  type not_filtered 'a = 'a;
  value using { gkeywords = table; gfilter = filter } kwd =
    let r = try Hashtbl.find table kwd with
            [ Not_found ->
                let r = ref 0 in do { Hashtbl.add table kwd r; r } ]
    in do { Token.Filter.keyword_added filter kwd (r.val = 0);
            incr r };

  value removing { gkeywords = table; gfilter = filter } kwd =
    let r = Hashtbl.find table kwd in
    let () = decr r in
    if r.val = 0 then do {
      Token.Filter.keyword_removed filter kwd;
      Hashtbl.remove table kwd
    } else ();
end;

(*
value iter_entry f e =
  let treated = ref [] in
  let rec do_entry e =
    if List.memq e treated.val then ()
    else do {
      treated.val := [e :: treated.val];
      f e;
      match e.edesc with
      [ Dlevels ll -> List.iter do_level ll
      | Dparser _ -> () ]
    }
  and do_level lev = do { do_tree lev.lsuffix; do_tree lev.lprefix }
  and do_tree =
    fun
    [ Node n -> do_node n
    | LocAct _ _ | DeadEnd -> () ]
  and do_node n = do { do_symbol n.node; do_tree n.son; do_tree n.brother }
  and do_symbol =
    fun
    [ Smeta _ sl _ -> List.iter do_symbol sl
    | Snterm e | Snterml e _ -> do_entry e
    | Slist0 s | Slist1 s | Sopt s -> do_symbol s
    | Slist0sep s1 s2 | Slist1sep s1 s2 -> do { do_symbol s1; do_symbol s2 }
    | Stree t -> do_tree t
    | Sself | Snext | Stoken _ | Stoken_fun _ -> () ]
  in
  do_entry e
;

value fold_entry f e init =
  let treated = ref [] in
  let rec do_entry accu e =
    if List.memq e treated.val then accu
    else do {
      treated.val := [e :: treated.val];
      let accu = f e accu in
      match e.edesc with
      [ Dlevels ll -> List.fold_left do_level accu ll
      | Dparser _ -> accu ]
    }
  and do_level accu lev =
    let accu = do_tree accu lev.lsuffix in
    do_tree accu lev.lprefix
  and do_tree accu =
    fun
    [ Node n -> do_node accu n
    | LocAct _ _ | DeadEnd -> accu ]
  and do_node accu n =
    let accu = do_symbol accu n.node in
    let accu = do_tree accu n.son in
    do_tree accu n.brother
  and do_symbol accu =
    fun
    [ Smeta _ sl _ -> List.fold_left do_symbol accu sl
    | Snterm e | Snterml e _ -> do_entry accu e
    | Slist0 s | Slist1 s | Sopt s -> do_symbol accu s
    | Slist0sep s1 s2 | Slist1sep s1 s2 ->
        let accu = do_symbol accu s1 in
        do_symbol accu s2
    | Stree t -> do_tree accu t
    | Sself | Snext | Stoken _ | Stoken_fun _ -> accu ]
  in
  do_entry init e
;

value is_level_labelled n lev =
  match lev.lname with
  [ Some n1 -> n = n1
  | None -> False ]
;

value tokens g con =
  let list = ref [] in
  do {
    Hashtbl.iter
      (fun (p_con, p_prm) c ->
         if p_con = con then list.val := [(p_prm, c.val) :: list.val] else ())
      g.gtokens;
    list.val
  }
;
*)
