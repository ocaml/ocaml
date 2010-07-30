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

(* BEGIN ugly hack.  See 15 lines down.  FIXME *)

type prev_locs = {
  pl_strm : mutable Obj.t;
  pl_locs : mutable list (int * Obj.t)
};

value prev_locs = ref ([] : list prev_locs);

(* END ugly hack FIXME *)

module Make (Structure : Structure.S) = struct
  open Structure;

  value empty_entry ename _ =
    raise (Stream.Error ("entry [" ^ ename ^ "] is empty"));

  value rec stream_map f = parser
    [ [: ` x; strm :] -> [: ` (f x); stream_map f strm :]
    | [: :] -> [: :] ];

(* ******************************************************************* *)
(* Ugly hack to prevent PR#5090.  See how to do this properly after
   the 3.12.0 release.  FIXME.
*)

value keep_prev_loc strm =
  match Stream.peek strm with
  [ None -> [: :]
  | Some (_, init_loc) ->
     let myrecord = { pl_strm = Obj.repr [: :];
                      pl_locs = [(0, Obj.repr init_loc)] }
     in
     let rec go prev_loc = parser
       [ [: `(tok, cur_loc); strm :] -> do {
           myrecord.pl_locs := myrecord.pl_locs
                               @ [ (Stream.count strm, Obj.repr cur_loc) ];
           [: `(tok, {prev_loc; cur_loc}); go cur_loc strm :] }
       | [: :] -> do {
           prev_locs.val := List.filter ((!=) myrecord) prev_locs.val;
           [: :] } ]
     in
     let result = go init_loc strm in
     do {
     prev_locs.val := [myrecord :: prev_locs.val];
     myrecord.pl_strm := Obj.repr result;
     result } ];

value drop_prev_loc strm = stream_map (fun (tok,r) -> (tok,r)) strm;

value get_cur_loc strm =
  match Stream.peek strm with
  [ Some (_,r) -> r.cur_loc
  | None -> Loc.ghost ];

value get_prev_loc strm =
  let c = Stream.count strm in
  let rec drop l =
    match l with
    [ [] -> []
    | [(i, _) :: ll] -> if i < c then drop ll else l ]
  in
  let rec find l =
    match l with
    [ [] -> None
    | [h::t] -> if h.pl_strm == Obj.repr strm then Some h else find t ]
  in
  match find prev_locs.val with
  [ None -> Loc.ghost
  | Some r -> do {
      r.pl_locs := drop r.pl_locs;
      match r.pl_locs with
      [ [] -> Loc.ghost
      | [(i, loc) :: _] ->
 if i = c then (Obj.obj loc : Loc.t) else Loc.ghost ] } ];

(* ******************************************************************* *)
(* END of ugly hack.  This is the previous code.

  value keep_prev_loc strm =
    match Stream.peek strm with
    [ None -> [: :]
    | Some (_,init_loc) ->
      let rec go prev_loc = parser
        [ [: `(tok,cur_loc); strm :] -> [: `(tok,{prev_loc;cur_loc}); go cur_loc strm :]
        | [: :] -> [: :] ]
      in go init_loc strm ];

  value drop_prev_loc strm = stream_map (fun (tok,r) -> (tok,r.cur_loc)) strm;

  value get_cur_loc strm =
    match Stream.peek strm with
    [ Some (_,r) -> r.cur_loc
    | None -> Loc.ghost ];

  value get_prev_loc strm =
    match Stream.peek strm with
    [ Some (_,r) -> r.prev_loc
    | None -> Loc.ghost ];
*)


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
      | (Slist0 s1, Slist0 s2) |
        (Slist1 s1, Slist1 s2) |
        (Sopt s1, Sopt s2) |
        (Stry s1, Stry s2) -> eq_symbols s1 s2
      | (Slist0sep s1 sep1, Slist0sep s2 sep2) |
        (Slist1sep s1 sep1, Slist1sep s2 sep2) ->
          eq_symbols s1 s2 && eq_symbols sep1 sep2
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
    | (Slist0 s1, Slist0 s2) |
      (Slist1 s1, Slist1 s2) |
      (Sopt s1, Sopt s2) |
      (Stry s1, Stry s2) -> eq_symbol s1 s2
    | (Slist0sep s1 sep1, Slist0sep s2 sep2) |
      (Slist1sep s1 sep1, Slist1sep s2 sep2) ->
        eq_symbol s1 s2 && eq_symbol sep1 sep2
    | (Stree _, Stree _) -> False
    | (Stoken (_, s1), Stoken (_, s2)) -> eq_Stoken_ids s1 s2
    | _ -> s1 = s2 ]
  ;
end;
