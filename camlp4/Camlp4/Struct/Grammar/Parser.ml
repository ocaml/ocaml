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
  module Tools  = Tools.Make Structure;
  module Failed = Failed.Make Structure;
  module Print = Print.Make Structure;
  open Structure;
  open Sig.Grammar;

  module StreamOrig = Stream;

  value njunk strm n =
    for i = 1 to n do Stream.junk strm done;

  value loc_bp = Tools.get_cur_loc;
  value loc_ep = Tools.get_prev_loc;
  value drop_prev_loc = Tools.drop_prev_loc;

  value add_loc bp parse_fun strm =
    let x = parse_fun strm in
    let ep = loc_ep strm in
    let loc = Loc.merge bp ep in
    (x, loc);

  value stream_peek_nth strm n =
    let rec loop i = fun
      [ [x :: xs] -> if i = 1 then Some x else loop (i - 1) xs
      | [] -> None ]
    in
    loop n (Stream.npeek n strm);

  (* We don't want Stream's functions to be used implictly. *)
  module Stream = struct
    type t 'a = StreamOrig.t 'a;
    exception Failure = StreamOrig.Failure;
    exception Error = StreamOrig.Error;
    value peek = StreamOrig.peek;
    value junk = StreamOrig.junk;
  end;

  value level_number entry lab =
    let rec lookup levn =
      fun
      [ [] -> failwith ("unknown level " ^ lab)
      | [lev :: levs] ->
          if Tools.is_level_labelled lab lev then levn else lookup (succ levn) levs ]
    in
    match entry.edesc with
    [ Dlevels elev -> lookup 0 elev
    | Dparser _ -> raise Not_found ]
  ;
  value strict_parsing = ref False;
  value strict_parsing_warning = ref False;

  value rec top_symb entry =
    fun
    [ Sself | Snext -> Snterm entry
    | Snterml e _ -> Snterm e
    | Slist1sep s sep -> Slist1sep (top_symb entry s) sep
    | _ -> raise Stream.Failure ]
  ;

  value top_tree entry =
    fun
    [ Node {node = s; brother = bro; son = son} ->
        Node {node = top_symb entry s; brother = bro; son = son}
    | LocAct _ _ | DeadEnd -> raise Stream.Failure ]
  ;

  value entry_of_symb entry =
    fun
    [ Sself | Snext -> entry
    | Snterm e -> e
    | Snterml e _ -> e
    | _ -> raise Stream.Failure ]
  ;

  value continue entry loc a s son p1 =
    parser
      [: a = (entry_of_symb entry s).econtinue 0 loc a;
        act = p1 ?? Failed.tree_failed entry a s son :] ->
        Action.mk (fun _ -> Action.getf act a)
  ;

  (* PR#4603, PR#4330, PR#4551:
     Here loc_bp replaced get_loc_ep to fix all these bugs.
     If you do change it again look at these bugs. *)
  value skip_if_empty bp strm =
    if loc_bp strm = bp then Action.mk (fun _ -> raise Stream.Failure)
    else
      raise Stream.Failure
  ;

  value do_recover parser_of_tree entry nlevn alevn loc a s son =
    parser
    [ [: a = parser_of_tree entry nlevn alevn (top_tree entry son) :] -> a
    | [: a = skip_if_empty loc :] -> a
    | [: a =
          continue entry loc a s son
            (parser_of_tree entry nlevn alevn son) :] ->
        a ]
  ;


  value recover parser_of_tree entry nlevn alevn loc a s son strm =
    if strict_parsing.val then raise (Stream.Error (Failed.tree_failed entry a s son))
    else
      let _ =
        if strict_parsing_warning.val then begin
            let msg = Failed.tree_failed entry a s son;
            Format.eprintf "Warning: trying to recover from syntax error";
            if entry.ename <> "" then Format.eprintf " in [%s]" entry.ename else ();
            Format.eprintf "\n%s%a@." msg Loc.print loc;
        end else () in
      do_recover parser_of_tree entry nlevn alevn loc a s son strm
  ;

  value rec parser_of_tree entry nlevn alevn =
    fun
    [ DeadEnd -> parser []
    | LocAct act _ -> parser [: :] -> act
    | Node {node = Sself; son = LocAct act _; brother = DeadEnd} ->
        parser [: a = entry.estart alevn :] -> Action.getf act a
    | Node {node = Sself; son = LocAct act _; brother = bro} ->
        let p2 = parser_of_tree entry nlevn alevn bro in
        parser
        [ [: a = entry.estart alevn :] -> Action.getf act a
        | [: a = p2 :] -> a ]
    | Node {node = s; son = son; brother = DeadEnd} ->
        let tokl =
          match s with
          [ Stoken _ | Skeyword _ -> Tools.get_token_list entry [] s son
          | _ -> None ]
        in
        match tokl with
        [ None ->
            let ps = parser_of_symbol entry nlevn s in
            let p1 = parser_of_tree entry nlevn alevn son in
            let p1 = parser_cont p1 entry nlevn alevn s son in
            fun strm ->
              let bp = loc_bp strm in
              match strm with parser
              [: a = ps; act = p1 bp a :] -> Action.getf act a
        | Some (tokl, last_tok, son) ->
            let p1 = parser_of_tree entry nlevn alevn son in
            let p1 = parser_cont p1 entry nlevn alevn last_tok son in
            parser_of_token_list p1 tokl ]
    | Node {node = s; son = son; brother = bro} ->
        let tokl =
          match s with
          [ Stoken _ | Skeyword _ -> Tools.get_token_list entry [] s son
          | _ -> None ]
        in
        match tokl with
        [ None ->
            let ps = parser_of_symbol entry nlevn s in
            let p1 = parser_of_tree entry nlevn alevn son in
            let p1 = parser_cont p1 entry nlevn alevn s son in
            let p2 = parser_of_tree entry nlevn alevn bro in
            fun strm ->
              let bp = loc_bp strm in
              match strm with parser
              [ [: a = ps; act = p1 bp a :] -> Action.getf act a
              | [: a = p2 :] -> a ]
        | Some (tokl, last_tok, son) ->
            let p1 = parser_of_tree entry nlevn alevn son in
            let p1 = parser_cont p1 entry nlevn alevn last_tok son in
            let p1 = parser_of_token_list p1 tokl in
            let p2 = parser_of_tree entry nlevn alevn bro in
            parser
            [ [: a = p1 :] -> a
            | [: a = p2 :] -> a ] ] ]
  and parser_cont p1 entry nlevn alevn s son loc a =
    parser
    [ [: a = p1 :] -> a
    | [: a = recover parser_of_tree entry nlevn alevn loc a s son :] -> a
    | [: :] -> raise (Stream.Error (Failed.tree_failed entry a s son)) ]
  and parser_of_token_list p1 tokl =
    loop 1 tokl where rec loop n =
      fun
      [ [Stoken (tematch, _) :: tokl] ->
          match tokl with
          [ [] ->
              let ps strm =
                match stream_peek_nth strm n with
                [ Some (tok, _) when tematch tok -> (njunk strm n; Action.mk tok)
                | _ -> raise Stream.Failure ]
              in
              fun strm ->
                let bp = loc_bp strm in
                match strm with parser
                [: a = ps; act = p1 bp a :] -> Action.getf act a
          | _ ->
              let ps strm =
                match stream_peek_nth strm n with
                [ Some (tok, _) when tematch tok -> tok
                | _ -> raise Stream.Failure ]
              in
              let p1 = loop (n + 1) tokl in
              parser [: tok = ps; s :] ->
                let act = p1 s in Action.getf act tok ]
      | [Skeyword kwd :: tokl] ->
          match tokl with
          [ [] ->
              let ps strm =
                match stream_peek_nth strm n with
                [ Some (tok, _) when Token.match_keyword kwd tok ->
                    (njunk strm n; Action.mk tok)
                | _ -> raise Stream.Failure ]
              in
              fun strm ->
                let bp = loc_bp strm in
                match strm with parser
                [: a = ps; act = p1 bp a :] -> Action.getf act a
          | _ ->
              let ps strm =
                match stream_peek_nth strm n with
                [ Some (tok, _) when Token.match_keyword kwd tok -> tok
                | _ -> raise Stream.Failure ]
              in
              let p1 = loop (n + 1) tokl in
              parser [: tok = ps; s :] ->
                let act = p1 s in Action.getf act tok ]
      | _ -> invalid_arg "parser_of_token_list" ]
  and parser_of_symbol entry nlevn =
    fun
    [ Smeta _ symbl act ->
        let act = Obj.magic act entry symbl in
        let pl = List.map (parser_of_symbol entry nlevn) symbl in
          Obj.magic (List.fold_left (fun act p -> Obj.magic act p) act pl)
    | Slist0 s ->
        let ps = parser_of_symbol entry nlevn s in
        let rec loop al =
          parser
          [ [: a = ps; s :] -> loop [a :: al] s
          | [: :] -> al ]
        in
        parser [: a = loop [] :] -> Action.mk (List.rev a)
    | Slist0sep symb sep ->
        let ps = parser_of_symbol entry nlevn symb in
        let pt = parser_of_symbol entry nlevn sep in
        let rec kont al =
          parser
          [ [: v = pt; a = ps ?? Failed.symb_failed entry v sep symb;
               s :] ->
              kont [a :: al] s
          | [: :] -> al ]
        in
        parser
        [ [: a = ps; s :] -> Action.mk (List.rev (kont [a] s))
        | [: :] -> Action.mk [] ]
    | Slist1 s ->
        let ps = parser_of_symbol entry nlevn s in
        let rec loop al =
          parser
          [ [: a = ps; s :] -> loop [a :: al] s
          | [: :] -> al ]
        in
        parser [: a = ps; s :] -> Action.mk (List.rev (loop [a] s))
    | Slist1sep symb sep ->
        let ps = parser_of_symbol entry nlevn symb in
        let pt = parser_of_symbol entry nlevn sep in
        let rec kont al =
          parser
          [ [: v = pt;
              a =
                parser
                [ [: a = ps :] -> a
                | [: a = parse_top_symb entry symb :] -> a
                | [: :] ->
                    raise (Stream.Error (Failed.symb_failed entry v sep symb)) ];
              s :] ->
              kont [a :: al] s
          | [: :] -> al ]
        in
        parser [: a = ps; s :] -> Action.mk (List.rev (kont [a] s))
    | Sopt s ->
        let ps = parser_of_symbol entry nlevn s in
        parser
        [ [: a = ps :] -> Action.mk (Some a)
        | [: :] -> Action.mk None ]
    | Stree t ->
        let pt = parser_of_tree entry 1 0 t in
        fun strm ->
          let bp = loc_bp strm in
          match strm with parser
          [: (act, loc) = add_loc bp pt :] ->
            Action.getf act loc
    | Snterm e -> parser [: a = e.estart 0 :] -> a
    | Snterml e l ->
        parser [: a = e.estart (level_number e l) :] -> a
    | Sself -> parser [: a = entry.estart 0 :] -> a
    | Snext -> parser [: a = entry.estart nlevn :] -> a
    | Skeyword kwd ->
        parser
        [: `(tok, _) when Token.match_keyword kwd tok :] ->
           Action.mk tok
    | Stoken (f, _) ->
        parser
        [: `(tok,_) when f tok :] -> Action.mk tok ]
  and parse_top_symb entry symb strm =
    parser_of_symbol entry 0 (top_symb entry symb) strm;

  value rec start_parser_of_levels entry clevn =
    fun
    [ [] -> fun _ -> parser []
    | [lev :: levs] ->
        let p1 = start_parser_of_levels entry (succ clevn) levs in
        match lev.lprefix with
        [ DeadEnd -> p1
        | tree ->
            let alevn =
              match lev.assoc with
              [ LeftA | NonA -> succ clevn
              | RightA -> clevn ]
            in
            let p2 = parser_of_tree entry (succ clevn) alevn tree in
            match levs with
            [ [] ->
                fun levn strm ->
                  let bp = loc_bp strm in
                  match strm with parser
                  [: (act, loc) = add_loc bp p2; strm :] ->
                    let a = Action.getf act loc in
                    entry.econtinue levn loc a strm
            | _ ->
                fun levn strm ->
                  if levn > clevn then p1 levn strm
                  else
                    let bp = loc_bp strm in
                    match strm with parser
                    [ [: (act, loc) = add_loc bp p2 :] ->
                        let a = Action.getf act loc in
                        entry.econtinue levn loc a strm
                    | [: act = p1 levn :] -> act ] ] ] ]
  ;

  value start_parser_of_entry entry =
    debug gram "start_parser_of_entry: @[<2>%a@]@." Print.entry entry in
    match entry.edesc with
    [ Dlevels [] -> Tools.empty_entry entry.ename
    | Dlevels elev -> start_parser_of_levels entry 0 elev
    | Dparser p -> fun _ -> p ]
  ;
  value rec continue_parser_of_levels entry clevn =
    fun
    [ [] -> fun _ _ _ -> parser []
    | [lev :: levs] ->
        let p1 = continue_parser_of_levels entry (succ clevn) levs in
        match lev.lsuffix with
        [ DeadEnd -> p1
        | tree ->
            let alevn =
              match lev.assoc with
              [ LeftA | NonA -> succ clevn
              | RightA -> clevn ]
            in
            let p2 = parser_of_tree entry (succ clevn) alevn tree in
            fun levn bp a strm ->
              if levn > clevn then p1 levn bp a strm
              else
                match strm with parser
                [ [: act = p1 levn bp a :] -> act
                | [: (act, loc) = add_loc bp p2 :] ->
                    let a = Action.getf2 act a loc in
                    entry.econtinue levn loc a strm ] ] ]
  ;

  value continue_parser_of_entry entry =
    debug gram "continue_parser_of_entry: @[<2>%a@]@." Print.entry entry in
    match entry.edesc with
    [ Dlevels elev ->
        let p = continue_parser_of_levels entry 0 elev in
        fun levn bp a ->
          parser
          [ [: a = p levn bp a :] -> a
          | [: :] -> a ]
    | Dparser _ -> fun _ _ _ -> parser [] ]
  ;

end;
