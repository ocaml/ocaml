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
(*
      value entry e s =
        let rec find_levels =
          fun
          [ [] -> None
          | [lev :: levs] ->
              match find_tree lev.lsuffix with
              [ None ->
                  match find_tree lev.lprefix with
                  [ None -> find_levels levs
                  | x -> x ]
              | x -> x ] ]
        and symbol =
          fun
          [ Snterm e -> if e.ename = s then Some e else None
          | Snterml e _ -> if e.ename = s then Some e else None
          | Smeta _ sl _ -> find_symbol_list sl
          | Slist0 s -> find_symbol s
          | Slist0sep s _ -> find_symbol s
          | Slist1 s -> find_symbol s
          | Slist1sep s _ -> find_symbol s
          | Sopt s -> find_symbol s
          | Stree t -> find_tree t
          | Sself | Snext | Stoken _ | Stoken_fun _ -> None ]
        and symbol_list =
          fun
          [ [s :: sl] ->
              match find_symbol s with
              [ None -> find_symbol_list sl
              | x -> x ]
          | [] -> None ]
        and tree =
          fun
          [ Node {node = s; brother = bro; son = son} ->
              match find_symbol s with
              [ None ->
                  match find_tree bro with
                  [ None -> find_tree son
                  | x -> x ]
              | x -> x ]
          | LocAct _ _ | DeadEnd -> None ]
        in
        match e.edesc with
        [ Dlevels levs ->
            match find_levels levs with
            [ Some e -> e
            | None -> raise Not_found ]
        | Dparser _ -> raise Not_found ]
      ;
*)
