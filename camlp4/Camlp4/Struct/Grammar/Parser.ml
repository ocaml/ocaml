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
  open Sig.Grammar.Structure;

  module Stream = struct
    include Stream;
    value junk strm = Context.junk strm;
    value count strm = Context.bp strm;
  end;

  value add_loc c bp parse_fun strm =
    let x = parse_fun strm in
    let ep = Context.loc_ep c in
    let loc = Loc.merge bp ep in
    (x, loc);

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

  value continue entry loc a s c son p1 =
    parser
      [: a = (entry_of_symb entry s).econtinue 0 loc a c;
        act = p1 ?? Failed.tree_failed entry a s son :] ->
        Action.mk (fun _ -> Action.getf act a)
  ;

  value skip_if_empty c bp p strm =
    (* if Stream.count strm == bp then Action.mk (fun _ -> p strm) *)
    if Context.loc_ep c == bp then Action.mk (fun _ -> p strm)
    else raise Stream.Failure
  ;

  value do_recover parser_of_tree entry nlevn alevn loc a s c son =
    parser
    [ [: a = parser_of_tree entry nlevn alevn c (top_tree entry son) :] -> a
    | [: a = skip_if_empty c loc (parser []) :] -> a
    | [: a =
          continue entry loc a s c son
            (parser_of_tree entry nlevn alevn c son) :] ->
        a ]
  ;


  value recover parser_of_tree entry nlevn alevn loc a s c son strm =
    if strict_parsing.val then raise (Stream.Error (Failed.tree_failed entry a s son))
    else
      let _ =
        if strict_parsing_warning.val then
          do {
            let msg = Failed.tree_failed entry a s son;
            Format.eprintf "Warning: trying to recover from syntax error";
            if entry.ename <> "" then Format.eprintf " in [%s]" entry.ename else ();
            Format.eprintf "\n%s%a@." msg Loc.print loc;
        } else () in
      do_recover parser_of_tree entry nlevn alevn loc a s c son strm
  ;

  value rec parser_of_tree entry nlevn alevn c =
    fun
    [ DeadEnd -> parser []
    | LocAct act _ -> parser [: :] -> act
    | Node {node = Sself; son = LocAct act _; brother = DeadEnd} ->
        parser [: a = entry.estart alevn c :] -> Action.getf act a
    | Node {node = Sself; son = LocAct act _; brother = bro} ->
        let p2 = parser_of_tree entry nlevn alevn c bro in
        parser
        [ [: a = entry.estart alevn c :] -> Action.getf act a
        | [: a = p2 :] -> a ]
    | Node {node = s; son = son; brother = DeadEnd} ->
        let tokl =
          match s with
          [ Stoken _ | Skeyword _ -> Tools.get_token_list entry [] s son
          | _ -> None ]
        in
        match tokl with
        [ None ->
            let ps = parser_of_symbol entry nlevn c s in
            let p1 = parser_of_tree entry nlevn alevn c son in
            let p1 = parser_cont p1 entry nlevn alevn s c son in
            parser bp [: a = ps; act = p1 bp a :] -> Action.getf act a
        | Some (tokl, last_tok, son) ->
            let p1 = parser_of_tree entry nlevn alevn c son in
            let p1 = parser_cont p1 entry nlevn alevn last_tok c son in
            parser_of_token_list p1 tokl c ]
    | Node {node = s; son = son; brother = bro} ->
        let tokl =
          match s with
          [ Stoken _ | Skeyword _ -> Tools.get_token_list entry [] s son
          | _ -> None ]
        in
        match tokl with
        [ None ->
            let ps = parser_of_symbol entry nlevn c s in
            let p1 = parser_of_tree entry nlevn alevn c son in
            let p1 = parser_cont p1 entry nlevn alevn s c son in
            let p2 = parser_of_tree entry nlevn alevn c bro in
            parser bp
            [ [: a = ps; act = p1 bp a :] -> Action.getf act a
            | [: a = p2 :] -> a ]
        | Some (tokl, last_tok, son) ->
            let p1 = parser_of_tree entry nlevn alevn c son in
            let p1 = parser_cont p1 entry nlevn alevn last_tok c son in
            let p1 = parser_of_token_list p1 tokl c in
            let p2 = parser_of_tree entry nlevn alevn c bro in
            parser
            [ [: a = p1 :] -> a
            | [: a = p2 :] -> a ] ] ]
  and parser_cont p1 entry nlevn alevn s c son loc a =
    parser
    [ [: a = p1 :] -> a
    | [: a = recover parser_of_tree entry nlevn alevn loc a s c son :] -> a
    | [: :] -> raise (Stream.Error (Failed.tree_failed entry a s son)) ]
  and parser_of_token_list p1 tokl c =
    loop 1 tokl where rec loop n =
      fun
      [ [Stoken (tematch, _) :: tokl] ->
          match tokl with
          [ [] ->
              let ps _ =
                match Context.peek_nth c n with
                [ Some (tok, _) when tematch tok -> do { Context.njunk c n; Action.mk tok }
                | _ -> raise Stream.Failure ]
              in parser bp [: a = ps; act = p1 bp a :] -> Action.getf act a
          | _ ->
              let ps _ =
                match Context.peek_nth c n with
                [ Some (tok, _) when tematch tok -> tok
                | _ -> raise Stream.Failure ]
              in
              let p1 = loop (n + 1) tokl in
              parser [: tok = ps; s :] -> let act = p1 s in Action.getf act tok ]
      | [Skeyword kwd :: tokl] ->
          match tokl with
          [ [] ->
              let ps _ =
                match Context.peek_nth c n with
                [ Some (tok, _) when Token.match_keyword kwd tok ->
                    do { Context.njunk c n; Action.mk tok }
                | _ -> raise Stream.Failure ]
              in parser bp [: a = ps; act = p1 bp a :] -> Action.getf act a
          | _ ->
              let ps _ =
                match Context.peek_nth c n with
                [ Some (tok, _) when Token.match_keyword kwd tok -> tok
                | _ -> raise Stream.Failure ]
              in
              let p1 = loop (n + 1) tokl in
              parser
                [: tok = ps; s :] ->
                  let act = p1 s in Action.getf act tok ]
      | _ -> invalid_arg "parser_of_token_list" ]
  and parser_of_symbol entry nlevn c =
    fun
    [ Smeta _ _symbl _act ->
        failwith "FIXME"
        (* let act = (magic "parser_of_symbol: act" act : 'a -> 'b -> 'c) entry symbl in *)
        (* Action.mk *)
          (* (List.fold_left *)
            (* (fun act symb -> magic "parser_of_symbol: act2" act (parser_of_symbol entry nlevn c symb)) *)
            (* act symbl) *)
    | Slist0 s ->
        let ps = parser_of_symbol entry nlevn c s in
        let rec loop al =
          parser
          [ [: a = ps; s :] -> loop [a :: al] s
          | [: :] -> al ]
        in
        parser [: a = loop [] :] -> Action.mk (List.rev a)
    | Slist0sep symb sep ->
        let ps = parser_of_symbol entry nlevn c symb in
        let pt = parser_of_symbol entry nlevn c sep in
        let rec kont al =
          parser
          [ [: v = pt; a = ps ?? Failed.symb_failed entry v sep symb; s :] ->
              kont [a :: al] s
          | [: :] -> al ]
        in
        parser
        [ [: a = ps; s :] -> Action.mk (List.rev (kont [a] s))
        | [: :] -> Action.mk [] ]
    | Slist1 s ->
        let ps = parser_of_symbol entry nlevn c s in
        let rec loop al =
          parser
          [ [: a = ps; s :] -> loop [a :: al] s
          | [: :] -> al ]
        in
        parser [: a = ps; s :] -> Action.mk (List.rev (loop [a] s))
    | Slist1sep symb sep ->
        let ps = parser_of_symbol entry nlevn c symb in
        let pt = parser_of_symbol entry nlevn c sep in
        let rec kont al =
          parser
          [ [: v = pt;
              a =
                parser
                [ [: a = ps :] -> a
                | [: a = parse_top_symb' entry symb c :] -> a
                | [: :] ->
                    raise (Stream.Error (Failed.symb_failed entry v sep symb)) ];
              s :] ->
              kont [a :: al] s
          | [: :] -> al ]
        in
        parser [: a = ps; s :] -> Action.mk (List.rev (kont [a] s))
    | Sopt s ->
        let ps = parser_of_symbol entry nlevn c s in
        parser
        [ [: a = ps :] -> Action.mk (Some a)
        | [: :] -> Action.mk None ]
    | Stree t ->
        let pt = parser_of_tree entry 1 0 c t in
        parser bp [: (act, loc) = add_loc c bp pt :] -> Action.getf act loc
    | Snterm e -> parser [: a = e.estart 0 c :] -> a
    | Snterml e l -> parser [: a = e.estart (level_number e l) c :] -> a
    | Sself -> parser [: a = entry.estart 0 c :] -> a
    | Snext -> parser [: a = entry.estart nlevn c :] -> a
    | Skeyword kwd ->
        parser
        [: `(tok, _) when Token.match_keyword kwd tok :] -> Action.mk tok
    | Stoken (f, _) -> parser [: `(tok, _) when f tok :] -> Action.mk tok ]
  and parse_top_symb' entry symb c =
    parser_of_symbol entry 0 c (top_symb entry symb)
  and parse_top_symb entry symb =
    fun strm ->
      let c = Context.mk strm
      in parse_top_symb' entry symb c (Context.stream c);

  value rec start_parser_of_levels entry clevn c =
    fun
    [ [] -> fun _ -> parser []
    | [lev :: levs] ->
        let p1 = start_parser_of_levels entry (succ clevn) c levs in
        match lev.lprefix with
        [ DeadEnd -> p1
        | tree ->
            let alevn =
              match lev.assoc with
              [ LeftA | NonA -> succ clevn
              | RightA -> clevn ]
            in
            let p2 = parser_of_tree entry (succ clevn) alevn c tree in
            match levs with
            [ [] ->
                fun levn ->
                  parser bp
                  [: (act, loc) = add_loc c bp p2; strm :] ->
                    let a = Action.getf act loc in
                    entry.econtinue levn loc a c strm
            | _ ->
                fun levn strm ->
                  if levn > clevn then p1 levn strm
                  else
                    match strm with parser bp
                    [ [: (act, loc) = add_loc c bp p2 :] ->
                        let a = Action.getf act loc in
                        entry.econtinue levn loc a c strm
                    | [: act = p1 levn :] -> act ] ] ] ]
  ;

  value start_parser_of_entry entry =
    debug gram "start_parser_of_entry: @[<2>%a@]@." Print.entry entry in
    match entry.edesc with
    [ Dlevels [] -> Tools.empty_entry entry.ename
    | Dlevels elev -> fun levn c ->
        start_parser_of_levels entry 0 c elev levn
    | Dparser p -> fun _ _ strm -> p strm ]
  ;
  value rec continue_parser_of_levels entry clevn c =
    fun
    [ [] -> fun _ _ _ -> parser []
    | [lev :: levs] ->
        let p1 = continue_parser_of_levels entry (succ clevn) c levs in
        match lev.lsuffix with
        [ DeadEnd -> p1
        | tree ->
            let alevn =
              match lev.assoc with
              [ LeftA | NonA -> succ clevn
              | RightA -> clevn ]
            in
            let p2 = parser_of_tree entry (succ clevn) alevn c tree in
            fun levn bp a strm ->
              if levn > clevn then p1 levn bp a strm
              else
                match strm with parser bp
                [ [: act = p1 levn bp a :] -> act
                | [: (act, loc) = add_loc c bp p2 :] ->
                    let a = Action.getf2 act a loc in
                    entry.econtinue levn loc a c strm ] ] ]
  ;

  value continue_parser_of_entry entry =
    debug gram "continue_parser_of_entry: @[<2>%a@]@." Print.entry entry in
    match entry.edesc with
    [ Dlevels elev ->
        let p = continue_parser_of_levels entry 0 in
        fun levn bp a c ->
          parser
          [ [: a = p c elev levn bp a :] -> a
          | [: :] -> a ]
    | Dparser _ -> fun _ _ _ _ -> parser [] ]
  ;

end;
