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

type grammar 'te =
  { gtokens : Hashtbl.t Token.pattern (ref int);
    glexer : mutable Token.glexer 'te }
;

type g_entry 'te =
  { egram : grammar 'te;
    ename : string;
    estart : mutable int -> Stream.t 'te -> Obj.t;
    econtinue : mutable int -> int -> Obj.t -> Stream.t 'te -> Obj.t;
    edesc : mutable g_desc 'te }
and g_desc 'te =
  [ Dlevels of list (g_level 'te)
  | Dparser of Stream.t 'te -> Obj.t ]
and g_level 'te =
  { assoc : g_assoc;
    lname : option string;
    lsuffix : g_tree 'te;
    lprefix : g_tree 'te }
and g_assoc =
  [ NonA
  | RightA
  | LeftA ]
and g_symbol 'te =
  [ Smeta of string and list (g_symbol 'te) and Obj.t
  | Snterm of g_entry 'te
  | Snterml of g_entry 'te and string
  | Slist0 of g_symbol 'te
  | Slist0sep of g_symbol 'te and g_symbol 'te
  | Slist1 of g_symbol 'te
  | Slist1sep of g_symbol 'te and g_symbol 'te
  | Sopt of g_symbol 'te
  | Sself
  | Snext
  | Stoken of Token.pattern
  | Stree of g_tree 'te ]
and g_action = Obj.t
and g_tree 'te =
  [ Node of g_node 'te
  | LocAct of g_action and list g_action
  | DeadEnd ]
and g_node 'te =
  { node : g_symbol 'te; son : g_tree 'te; brother : g_tree 'te }
;

type position =
  [ First
  | Last
  | Before of string
  | After of string
  | Level of string ]
;

value levels_of_rules :
  g_entry 'te -> option position ->
    list
      (option string * option g_assoc *
       list (list (g_symbol 'te) * g_action)) ->
    list (g_level 'te);
value srules : list (list (g_symbol 'te) * g_action) -> g_symbol 'te;
external action : 'a -> g_action = "%identity";

value delete_rule_in_level_list :
  g_entry 'te -> list (g_symbol 'te) -> list (g_level 'te) ->
    list (g_level 'te);

value warning_verbose : ref bool;
