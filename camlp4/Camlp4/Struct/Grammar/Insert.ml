(* -*- camlp4r -*- *)
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
  module Tools = Tools.Make Structure;
  module Parser = Parser.Make Structure;
  open Structure;
  open Format;
  open Sig.Grammar;

  value is_before s1 s2 =
    match (s1, s2) with
    [ (Skeyword _ | Stoken _, Skeyword _ | Stoken _) -> False
    | (Skeyword _ | Stoken _, _) -> True
    | _ -> False ]
  ;
  value rec derive_eps =
    fun
    [ Slist0 _ -> True
    | Slist0sep _ _ -> True
    | Sopt _ -> True
    | Stree t -> tree_derive_eps t
    | Smeta _ _ _ | Slist1 _ | Slist1sep _ _ | Snterm _ | Snterml _ _ | Snext |
      Sself | Stoken _ | Skeyword _ -> False ]
  and tree_derive_eps =
    fun
    [ LocAct _ _ -> True
    | Node {node = s; brother = bro; son = son} ->
        derive_eps s && tree_derive_eps son || tree_derive_eps bro
    | DeadEnd -> False ]
  ;

  value empty_lev lname assoc =
    let assoc =
      match assoc with
      [ Some a -> a
      | None -> LeftA ]
    in
    {assoc = assoc; lname = lname; lsuffix = DeadEnd; lprefix = DeadEnd}
  ;
  value change_lev entry lev n lname assoc =
    let a =
      match assoc with
      [ None -> lev.assoc
      | Some a ->
          do {
            if a <> lev.assoc && entry.egram.warning_verbose.val then do {
              eprintf "<W> Changing associativity of level \"%s\"\n" n;
              flush stderr
            }
            else ();
            a
          } ]
    in
    do {
      match lname with
      [ Some n ->
          if lname <> lev.lname && entry.egram.warning_verbose.val then do {
            eprintf "<W> Level label \"%s\" ignored\n" n; flush stderr
          }
          else ()
      | None -> () ];
      {assoc = a; lname = lev.lname; lsuffix = lev.lsuffix;
      lprefix = lev.lprefix}
    }
  ;
  value change_to_self entry =
    fun
    [ Snterm e when e == entry -> Sself
    | x -> x ]
  ;


  value get_level entry position levs =
    match position with
    [ Some First -> ([], empty_lev, levs)
    | Some Last -> (levs, empty_lev, [])
    | Some (Level n) ->
        let rec get =
          fun
          [ [] ->
              do {
                eprintf "No level labelled \"%s\" in entry \"%s\"\n" n
                  entry.ename;
                flush stderr;
                failwith "Grammar.extend"
              }
          | [lev :: levs] ->
              if Tools.is_level_labelled n lev then ([], change_lev entry lev n, levs)
              else
                let (levs1, rlev, levs2) = get levs in
                ([lev :: levs1], rlev, levs2) ]
        in
        get levs
    | Some (Before n) ->
        let rec get =
          fun
          [ [] ->
              do {
                eprintf "No level labelled \"%s\" in entry \"%s\"\n" n
                  entry.ename;
                flush stderr;
                failwith "Grammar.extend"
              }
          | [lev :: levs] ->
              if Tools.is_level_labelled n lev then ([], empty_lev, [lev :: levs])
              else
                let (levs1, rlev, levs2) = get levs in
                ([lev :: levs1], rlev, levs2) ]
        in
        get levs
    | Some (After n) ->
        let rec get =
          fun
          [ [] ->
              do {
                eprintf "No level labelled \"%s\" in entry \"%s\"\n" n
                  entry.ename;
                flush stderr;
                failwith "Grammar.extend"
              }
          | [lev :: levs] ->
              if Tools.is_level_labelled n lev then ([lev], empty_lev, levs)
              else
                let (levs1, rlev, levs2) = get levs in
                ([lev :: levs1], rlev, levs2) ]
        in
        get levs
    | None ->
        match levs with
        [ [lev :: levs] -> ([], change_lev entry lev "<top>", levs)
        | [] -> ([], empty_lev, []) ] ]
  ;

  value rec check_gram entry =
    fun
    [ Snterm e ->
        if e.egram != entry.egram then do {
          eprintf "\
  Error: entries \"%s\" and \"%s\" do not belong to the same grammar.\n"
            entry.ename e.ename;
          flush stderr;
          failwith "Grammar.extend error"
        }
        else ()
    | Snterml e _ ->
        if e.egram != entry.egram then do {
          eprintf "\
  Error: entries \"%s\" and \"%s\" do not belong to the same grammar.\n"
            entry.ename e.ename;
          flush stderr;
          failwith "Grammar.extend error"
        }
        else ()
    | Smeta _ sl _ -> List.iter (check_gram entry) sl
    | Slist0sep s t -> do { check_gram entry t; check_gram entry s }
    | Slist1sep s t -> do { check_gram entry t; check_gram entry s }
    | Slist0 s -> check_gram entry s
    | Slist1 s -> check_gram entry s
    | Sopt s -> check_gram entry s
    | Stree t -> tree_check_gram entry t
    | Snext | Sself | Stoken _ | Skeyword _ -> () ]
  and tree_check_gram entry =
    fun
    [ Node {node = n; brother = bro; son = son} ->
        do {
          check_gram entry n;
          tree_check_gram entry bro;
          tree_check_gram entry son
        }
    | LocAct _ _ | DeadEnd -> () ]
  ;
  value get_initial =
    fun
    [ [Sself :: symbols] -> (True, symbols)
    | symbols -> (False, symbols) ]
  ;


  value insert_tokens gram symbols =
    let rec insert =
      fun
      [ Smeta _ sl _ -> List.iter insert sl
      | Slist0 s -> insert s
      | Slist1 s -> insert s
      | Slist0sep s t -> do { insert s; insert t }
      | Slist1sep s t -> do { insert s; insert t }
      | Sopt s -> insert s
      | Stree t -> tinsert t
      | Skeyword kwd -> using gram kwd
      | Snterm _ | Snterml _ _ | Snext | Sself | Stoken _ -> () ]
    and tinsert =
      fun
      [ Node {node = s; brother = bro; son = son} ->
          do { insert s; tinsert bro; tinsert son }
      | LocAct _ _ | DeadEnd -> () ]
    in
    List.iter insert symbols
  ;

  value insert_tree entry gsymbols action tree =
    let rec insert symbols tree =
      match symbols with
      [ [s :: sl] -> insert_in_tree s sl tree
      | [] ->
          match tree with
          [ Node {node = s; son = son; brother = bro} ->
              Node {node = s; son = son; brother = insert [] bro}
          | LocAct old_action action_list ->
              let () =
                if entry.egram.warning_verbose.val then
                  eprintf "<W> Grammar extension: in [%s] some rule has been masked@."
                          entry.ename
                else ()
              in LocAct action [old_action :: action_list]
          | DeadEnd -> LocAct action [] ] ]
    and insert_in_tree s sl tree =
      match try_insert s sl tree with
      [ Some t -> t
      | None -> Node {node = s; son = insert sl DeadEnd; brother = tree} ]
    and try_insert s sl tree =
      match tree with
      [ Node {node = s1; son = son; brother = bro} ->
          if Tools.eq_symbol s s1 then
            let t = Node {node = s1; son = insert sl son; brother = bro} in
            Some t
          else if is_before s1 s || derive_eps s && not (derive_eps s1) then
            let bro =
              match try_insert s sl bro with
              [ Some bro -> bro
              | None ->
                  Node {node = s; son = insert sl DeadEnd; brother = bro} ]
            in
            let t = Node {node = s1; son = son; brother = bro} in
            Some t
          else
            match try_insert s sl bro with
            [ Some bro ->
                let t = Node {node = s1; son = son; brother = bro} in
                Some t
            | None -> None ]
      | LocAct _ _ | DeadEnd -> None ]
    and insert_new =
      fun
      [ [s :: sl] -> Node {node = s; son = insert_new sl; brother = DeadEnd}
      | [] -> LocAct action [] ]
    in
    insert gsymbols tree
  ;
  value insert_level entry e1 symbols action slev =
    match e1 with
    [ True ->
        {assoc = slev.assoc; lname = slev.lname;
        lsuffix = insert_tree entry symbols action slev.lsuffix;
        lprefix = slev.lprefix}
    | False ->
        {assoc = slev.assoc; lname = slev.lname; lsuffix = slev.lsuffix;
        lprefix = insert_tree entry symbols action slev.lprefix} ]
  ;

  value levels_of_rules entry position rules =
    let elev =
      match entry.edesc with
      [ Dlevels elev -> elev
      | Dparser _ ->
          do {
            eprintf "Error: entry not extensible: \"%s\"\n" entry.ename;
            flush stderr;
            failwith "Grammar.extend"
          } ]
    in
    if rules = [] then elev
    else
      let (levs1, make_lev, levs2) = get_level entry position elev in
      let (levs, _) =
        List.fold_left
          (fun (levs, make_lev) (lname, assoc, level) ->
            let lev = make_lev lname assoc in
            let lev =
              List.fold_left
                (fun lev (symbols, action) ->
                    let symbols = List.map (change_to_self entry) symbols in
                    do {
                      List.iter (check_gram entry) symbols;
                      let (e1, symbols) = get_initial symbols;
                      insert_tokens entry.egram symbols;
                      insert_level entry e1 symbols action lev
                    })
                lev level
            in
            ([lev :: levs], empty_lev))
          ([], make_lev) rules
      in
      levs1 @ List.rev levs @ levs2
    ;

    value extend entry (position, rules) =
      let elev = levels_of_rules entry position rules in
      do {
        entry.edesc := Dlevels elev;
        entry.estart :=
          fun lev c strm ->
            let f = Parser.start_parser_of_entry entry in
            do { entry.estart := f; f lev c strm };
        entry.econtinue :=
          fun lev bp a c strm ->
            let f = Parser.continue_parser_of_entry entry in
            do { entry.econtinue := f; f lev bp a c strm }
      };

  end;
