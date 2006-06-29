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
  open Structure;

  value empty_entry ename _ _ _ =
    raise (Stream.Error ("entry [" ^ ename ^ "] is empty"));

  value is_level_labelled n lev =
    match lev.lname with
    [ Some n1 -> n = n1
    | None -> False ];

  value warning_verbose = ref True;

  value rec get_token_list entry tokl last_tok tree =
    match tree with
    [ Node {node = (Stoken _ | Skeyword _ as tok); son = son; brother = DeadEnd} ->
        get_token_list entry [last_tok :: tokl] tok son
    | _ ->
        if tokl = [] then None
        else Some (List.rev [last_tok :: tokl], last_tok, tree) ];

  value is_antiquot s =
    let len = String.length s in
    len > 1 && s.[0] = '$';

  value eq_Stoken_ids s1 s2 =
    not (is_antiquot s1) && not (is_antiquot s2) && s1 = s2;

  value logically_eq_symbols entry =
    let rec eq_symbols s1 s2 =
      match (s1, s2) with
      [ (Snterm e1, Snterm e2) -> e1.ename = e2.ename
      | (Snterm e1, Sself) -> e1.ename = entry.ename
      | (Sself, Snterm e2) -> entry.ename = e2.ename
      | (Snterml e1 l1, Snterml e2 l2) -> e1.ename = e2.ename && l1 = l2
      | (Slist0 s1, Slist0 s2) -> eq_symbols s1 s2
      | (Slist0sep s1 sep1, Slist0sep s2 sep2) ->
          eq_symbols s1 s2 && eq_symbols sep1 sep2
      | (Slist1 s1, Slist1 s2) -> eq_symbols s1 s2
      | (Slist1sep s1 sep1, Slist1sep s2 sep2) ->
          eq_symbols s1 s2 && eq_symbols sep1 sep2
      | (Sopt s1, Sopt s2) -> eq_symbols s1 s2
      | (Stree t1, Stree t2) -> eq_trees t1 t2
      | (Stoken (_, s1), Stoken (_, s2)) -> eq_Stoken_ids s1 s2
      | _ -> s1 = s2 ]
    and eq_trees t1 t2 =
      match (t1, t2) with
      [ (Node n1, Node n2) ->
          eq_symbols n1.node n2.node && eq_trees n1.son n2.son &&
          eq_trees n1.brother n2.brother
      | (LocAct _ _ | DeadEnd, LocAct _ _ | DeadEnd) -> True
      | _ -> False ]
    in
    eq_symbols;

  value rec eq_symbol s1 s2 =
    match (s1, s2) with
    [ (Snterm e1, Snterm e2) -> e1 == e2
    | (Snterml e1 l1, Snterml e2 l2) -> e1 == e2 && l1 = l2
    | (Slist0 s1, Slist0 s2) -> eq_symbol s1 s2
    | (Slist0sep s1 sep1, Slist0sep s2 sep2) ->
        eq_symbol s1 s2 && eq_symbol sep1 sep2
    | (Slist1 s1, Slist1 s2) -> eq_symbol s1 s2
    | (Slist1sep s1 sep1, Slist1sep s2 sep2) ->
        eq_symbol s1 s2 && eq_symbol sep1 sep2
    | (Sopt s1, Sopt s2) -> eq_symbol s1 s2
    | (Stree _, Stree _) -> False
    | (Stoken (_, s1), Stoken (_, s2)) -> eq_Stoken_ids s1 s2
    | _ -> s1 = s2 ]
  ;
end;
