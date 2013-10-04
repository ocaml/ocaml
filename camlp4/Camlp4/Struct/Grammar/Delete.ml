(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

exception Rule_not_found of (string * string);

let () =
  Printexc.register_printer
    (fun
      [ Rule_not_found (symbols, entry) ->
	  let msg = Printf.sprintf "rule %S cannot be found in entry\n%s" symbols entry in
	  Some msg
      | _ -> None ]) in ()
;

module Make (Structure : Structure.S) = struct
  module Tools  = Tools.Make Structure;
  module Parser = Parser.Make Structure;
  module Print = Print.Make Structure;
  open Structure;

value raise_rule_not_found entry symbols =
  let to_string f x =
    let buff = Buffer.create 128 in
    let ppf = Format.formatter_of_buffer buff in
    do {
      f ppf x;
      Format.pp_print_flush ppf ();
      Buffer.contents buff
    } in
    let entry = to_string Print.entry entry in
    let symbols = to_string Print.print_rule symbols in
    raise (Rule_not_found (symbols, entry))
;

(* Deleting a rule *)

(* [delete_rule_in_tree] returns
     [Some (dsl, t)] if success
        [dsl] =
           Some (list of deleted nodes) if branch deleted
           None if action replaced by previous version of action
        [t] = remaining tree
     [None] if failure *)

value delete_rule_in_tree entry =
  let rec delete_in_tree symbols tree =
    match (symbols, tree) with
    [ ([s :: sl], Node n) ->
        if Tools.logically_eq_symbols entry s n.node then delete_son sl n
        else
          match delete_in_tree symbols n.brother with
          [ Some (dsl, t) ->
              Some (dsl, Node {node = n.node; son = n.son; brother = t})
          | None -> None ]
    | ([_ :: _], _) -> None
    | ([], Node n) ->
        match delete_in_tree [] n.brother with
        [ Some (dsl, t) ->
            Some (dsl, Node {node = n.node; son = n.son; brother = t})
        | None -> None ]
    | ([], DeadEnd) -> None
    | ([], LocAct _ []) -> Some (Some [], DeadEnd)
    | ([], LocAct _ [action :: list]) -> Some (None, LocAct action list) ]
  and delete_son sl n =
    match delete_in_tree sl n.son with
    [ Some (Some dsl, DeadEnd) -> Some (Some [n.node :: dsl], n.brother)
    | Some (Some dsl, t) ->
        let t = Node {node = n.node; son = t; brother = n.brother} in
        Some (Some [n.node :: dsl], t)
    | Some (None, t) ->
        let t = Node {node = n.node; son = t; brother = n.brother} in
        Some (None, t)
    | None -> None ]
  in
  delete_in_tree
;
value rec decr_keyw_use gram =
  fun
  [ Skeyword kwd -> removing gram kwd
  | Smeta _ sl _ -> List.iter (decr_keyw_use gram) sl
  | Slist0 s | Slist1 s | Sopt s | Stry s -> decr_keyw_use gram s
  | Slist0sep s1 s2 -> do { decr_keyw_use gram s1; decr_keyw_use gram s2 }
  | Slist1sep s1 s2 -> do { decr_keyw_use gram s1; decr_keyw_use gram s2 }
  | Stree t -> decr_keyw_use_in_tree gram t
  | Sself | Snext | Snterm _ | Snterml _ _ | Stoken _ -> () ]
and decr_keyw_use_in_tree gram =
  fun
  [ DeadEnd | LocAct _ _ -> ()
  | Node n ->
      do {
        decr_keyw_use gram n.node;
        decr_keyw_use_in_tree gram n.son;
        decr_keyw_use_in_tree gram n.brother
      } ]
;
value rec delete_rule_in_suffix entry symbols =
  fun
  [ [lev :: levs] ->
      match delete_rule_in_tree entry symbols lev.lsuffix with
      [ Some (dsl, t) ->
          do {
            match dsl with
            [ Some dsl -> List.iter (decr_keyw_use entry.egram) dsl
            | None -> () ];
            match t with
            [ DeadEnd when lev.lprefix == DeadEnd -> levs
            | _ ->
                let lev =
                  {assoc = lev.assoc; lname = lev.lname; lsuffix = t;
                   lprefix = lev.lprefix}
                in
                [lev :: levs] ]
          }
      | None ->
          let levs = delete_rule_in_suffix entry symbols levs in
          [lev :: levs] ]
  | [] -> raise_rule_not_found entry symbols ]
;

value rec delete_rule_in_prefix entry symbols =
  fun
  [ [lev :: levs] ->
      match delete_rule_in_tree entry symbols lev.lprefix with
      [ Some (dsl, t) ->
          do {
            match dsl with
            [ Some dsl -> List.iter (decr_keyw_use entry.egram) dsl
            | None -> () ];
            match t with
            [ DeadEnd when lev.lsuffix == DeadEnd -> levs
            | _ ->
                let lev =
                  {assoc = lev.assoc; lname = lev.lname;
                   lsuffix = lev.lsuffix; lprefix = t}
                in
                [lev :: levs] ]
          }
      | None ->
          let levs = delete_rule_in_prefix entry symbols levs in
          [lev :: levs] ]
  | [] -> raise_rule_not_found entry symbols ]
;

value rec delete_rule_in_level_list entry symbols levs =
  match symbols with
  [ [Sself :: symbols] -> delete_rule_in_suffix entry symbols levs
  | [Snterm e :: symbols] when e == entry ->
      delete_rule_in_suffix entry symbols levs
  | _ -> delete_rule_in_prefix entry symbols levs ]
;


value delete_rule entry sl =
  match entry.edesc with
  [ Dlevels levs ->
      let levs = delete_rule_in_level_list entry sl levs in
      do {
        entry.edesc := Dlevels levs;
        entry.estart :=
          fun lev strm ->
            let f = Parser.start_parser_of_entry entry in
            do { entry.estart := f; f lev strm };
        entry.econtinue :=
          fun lev bp a strm ->
            let f = Parser.continue_parser_of_entry entry in
            do { entry.econtinue := f; f lev bp a strm }
      }
  | Dparser _ -> () ]
;

end;
