(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type grammar =
  { gtokens : Hashtbl.t Token.pattern (ref int);
    glexer : mutable Token.lexer }
;

type g_entry =
  { egram : grammar;
    ename : string;
    estart : mutable int -> Stream.t Token.t -> Obj.t;
    econtinue : mutable int -> int -> Obj.t -> Stream.t Token.t -> Obj.t;
    edesc : mutable g_desc }
and g_desc =
  [ Dlevels of list g_level
  | Dparser of Stream.t Token.t -> Obj.t ]
and g_level =
  { assoc : g_assoc;
    lname : option string;
    lsuffix : g_tree;
    lprefix : g_tree }
and g_assoc =
  [ NonA
  | RightA
  | LeftA ]
and g_symbol =
  [ Snterm of g_entry
  | Snterml of g_entry and string
  | Slist0 of g_symbol
  | Slist0sep of g_symbol and g_symbol
  | Slist1 of g_symbol
  | Slist1sep of g_symbol and g_symbol
  | Sopt of g_symbol
  | Sself
  | Snext
  | Stoken of Token.pattern
  | Stree of g_tree ]
and g_action = Obj.t
and g_tree =
  [ Node of g_node
  | LocAct of g_action and list g_action
  | DeadEnd ]
and g_node = { node : g_symbol; son : g_tree; brother : g_tree }
;

type position =
  [ First
  | Last
  | Before of string
  | After of string
  | Level of string ]
;

value levels_of_rules :
  g_entry -> option position ->
    list (option string * option g_assoc * list (list g_symbol * g_action)) ->
    list g_level;
value srules : list (list g_symbol * g_action) -> g_symbol;
external action : 'a -> g_action = "%identity";

value delete_rule_in_level_list :
  g_entry -> list g_symbol -> list g_level -> list g_level;

value warning_verbose : ref bool;
