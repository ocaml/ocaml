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

open Stdpp;
open Gramext;

open Format;

value rec flatten_tree =
  fun
  [ DeadEnd -> []
  | LocAct _ _ -> [[]]
  | Node {node = n; brother = b; son = s} ->
      List.map (fun l -> [n :: l]) (flatten_tree s) @ flatten_tree b ]
;

value print_str s = print_string ("\"" ^ String.escaped s ^ "\"");

value rec print_symbol =
  fun
  [ Slist0 s -> do { print_string "LIST0"; print_string " "; print_symbol1 s }
  | Slist0sep s t ->
      do {
        print_string "LIST0";
        print_string " ";
        print_symbol1 s;
        print_string " SEP ";
        print_symbol1 t
      }
  | Slist1 s -> do { print_string "LIST1"; print_string " "; print_symbol1 s }
  | Slist1sep s t ->
      do {
        print_string "LIST1";
        print_string " ";
        print_symbol1 s;
        print_string " SEP ";
        print_symbol1 t
      }
  | Sopt s -> do { print_string "OPT "; print_symbol1 s }
  | Stoken (con, prm) when con <> "" && prm <> "" ->
      do { print_string con; print_space (); print_str prm }
  | Snterml e l ->
      do {
        print_string e.ename;
        print_space ();
        print_string "LEVEL";
        print_space ();
        print_str l
      }
  | s -> print_symbol1 s ]
and print_symbol1 =
  fun
  [ Stoken ("", s) -> print_str s
  | Snterm e -> print_string e.ename
  | Sself -> print_string "SELF"
  | Snext -> print_string "NEXT"
  | Stoken (con, "") -> print_string con
  | Stree t -> print_level print_space (flatten_tree t)
  | s -> do { print_string "("; print_symbol s; print_string ")" } ]
and print_rule symbols =
  do {
    open_hovbox 0;
    let _ =
      List.fold_left
        (fun sep symbol ->
           do {
             sep ();
             print_symbol symbol;
             fun () -> do { print_string ";"; print_space () }
           })
        (fun () -> ()) symbols
    in
    close_box ()
  }
and print_level print_space rules =
  do {
    open_hovbox 0;
    print_string "[ ";
    let _ =
      List.fold_left
        (fun sep rule ->
           do {
             sep ();
             print_rule rule;
             fun () -> do { print_space (); print_string "| " }
           })
        (fun () -> ()) rules
    in
    print_string " ]";
    close_box ()
  }
;

value print_levels elev =
  let _ =
    List.fold_left
      (fun sep lev ->
         let rules =
           List.map (fun t -> [Sself :: t]) (flatten_tree lev.lsuffix) @
             flatten_tree lev.lprefix
         in
         do {
           sep ();
           open_hovbox 2;
           match lev.lname with
           [ Some n ->
               do {
                 print_string ("\"" ^ String.escaped n ^ "\"");
                 print_break 1 2
               }
           | _ -> () ];
           match lev.assoc with
           [ LeftA -> print_string "LEFTA"
           | RightA -> print_string "RIGHTA"
           | NonA -> print_string "NONA" ];
           close_box ();
           print_break 1 2;
           print_level force_newline rules;
           fun () -> do { print_cut (); print_string "| " }
         })
      (fun () -> ()) elev
  in
  ()
;

value print_entry e =
  do {
    open_vbox 0;
    print_string "[ ";
    match e.edesc with
    [ Dlevels elev -> print_levels elev
    | Dparser _ -> print_string "<parser>" ];
    print_string " ]";
    close_box ();
    print_newline ()
  }
;

type g = Gramext.grammar;

external grammar_obj : g -> grammar = "%identity";

value floc = ref (fun _ -> failwith "internal error when computing location");
value loc_of_token_interval bp ep =
  if bp == ep then
    if bp == 0 then (0, 1)
    else
      let a = snd (floc.val (bp - 1)) in
      (a, a + 1)
  else
    let (bp1, bp2) = floc.val bp in
    let (ep1, ep2) = floc.val (pred ep) in
    (if bp1 < ep1 then bp1 else ep1, if bp2 > ep2 then bp2 else ep2)
;

value rec name_of_symbol entry =
  fun
  [ Snterm e -> "[" ^ e.ename ^ "]"
  | Snterml e l -> "[" ^ e.ename ^ " level " ^ l ^ "]"
  | Sself | Snext -> "[" ^ entry.ename ^ "]"
  | Stoken tok -> entry.egram.glexer.Token.text tok
  | _ -> "???" ]
;

value rec get_token_list entry tokl last_tok tree =
  match tree with
  [ Node {node = (Stoken tok as s); son = son; brother = DeadEnd} ->
      match entry.egram.glexer.Token.tparse tok with
      [ Some _ ->
          if tokl = [] then None
          else Some (List.rev [last_tok :: tokl], last_tok, tree)
      | None -> get_token_list entry [last_tok :: tokl] tok son ]
  | _ ->
      if tokl = [] then None
      else Some (List.rev [last_tok :: tokl], last_tok, tree) ]
;

value rec name_of_symbol_failed entry =
  fun
  [ Slist0 s -> name_of_symbol_failed entry s
  | Slist0sep s _ -> name_of_symbol_failed entry s
  | Slist1 s -> name_of_symbol_failed entry s
  | Slist1sep s _ -> name_of_symbol_failed entry s
  | Sopt s -> name_of_symbol_failed entry s
  | Stree t -> name_of_tree_failed entry t
  | s -> name_of_symbol entry s ]
and name_of_tree_failed entry =
  fun
  [ Node {node = s; brother = bro; son = son} ->
      let tokl =
        match s with
        [ Stoken tok when entry.egram.glexer.Token.tparse tok = None ->
            get_token_list entry [] tok son
        | _ -> None ]
      in
      match tokl with
      [ None ->
          let txt = name_of_symbol_failed entry s in
          let txt =
            match (s, son) with
            [ (Sopt _, Node _) -> txt ^ " or " ^ name_of_tree_failed entry son
            | _ -> txt ]
          in
          let txt =
            match bro with
            [ DeadEnd | LocAct _ _ -> txt
            | _ -> txt ^ " or " ^ name_of_tree_failed entry bro ]
          in
          txt
      | Some (tokl, last_tok, son) ->
          List.fold_left
            (fun s tok ->
               (if s = "" then "" else s ^ " ") ^
                 entry.egram.glexer.Token.text tok)
            "" tokl ]
  | DeadEnd | LocAct _ _ -> "???" ]
;

value search_tree_in_entry prev_symb tree =
  fun
  [ Dlevels levels ->
      let rec search_levels =
        fun
        [ [] -> tree
        | [level :: levels] ->
            match search_level level with
            [ Some tree -> tree
            | None -> search_levels levels ] ]
      and search_level level =
        match search_tree level.lsuffix with
        [ Some t -> Some (Node {node = Sself; son = t; brother = DeadEnd})
        | None -> search_tree level.lprefix ]
      and search_tree t =
        if tree <> DeadEnd && t == tree then Some t
        else
          match t with
          [ Node n ->
              match search_symbol n.node with
              [ Some symb ->
                  Some (Node {node = symb; son = n.son; brother = DeadEnd})
              | None ->
                  match search_tree n.son with
                  [ Some t ->
                      Some (Node {node = n.node; son = t; brother = DeadEnd})
                  | None -> search_tree n.brother ] ]
          | _ -> None ]
      and search_symbol symb =
        match symb with
        [ Snterm _ | Snterml _ _ | Slist0 _ | Slist0sep _ _ | Slist1 _ |
          Slist1sep _ _ | Sopt _ | Stoken _ | Stree _
          when symb == prev_symb ->
            Some symb
        | Slist0 symb ->
            match search_symbol symb with
            [ Some symb -> Some (Slist0 symb)
            | None -> None ]
        | Slist0sep symb sep ->
            match search_symbol symb with
            [ Some symb -> Some (Slist0sep symb sep)
            | None ->
                match search_symbol sep with
                [ Some sep -> Some (Slist0sep symb sep)
                | None -> None ] ]
        | Slist1 symb ->
            match search_symbol symb with
            [ Some symb -> Some (Slist1 symb)
            | None -> None ]
        | Slist1sep symb sep ->
            match search_symbol symb with
            [ Some symb -> Some (Slist1sep symb sep)
            | None ->
                match search_symbol sep with
                [ Some sep -> Some (Slist1sep symb sep)
                | None -> None ] ]
        | Sopt symb ->
            match search_symbol symb with
            [ Some symb -> Some (Sopt symb)
            | None -> None ]
        | Stree t ->
            match search_tree t with
            [ Some t -> Some (Stree t)
            | None -> None ]
        | _ -> None ]
      in
      search_levels levels
  | _ -> tree ]
;

value error_verbose = ref False;

value tree_failed entry prev_symb_result prev_symb tree =
  let txt = name_of_tree_failed entry tree in
  let txt =
    match prev_symb with
    [ Slist0 s ->
        let txt1 = name_of_symbol_failed entry s in
        txt1 ^ " or " ^ txt ^ " expected"
    | Slist1 s ->
        let txt1 = name_of_symbol_failed entry s in
        txt1 ^ " or " ^ txt ^ " expected"
    | Slist0sep s sep ->
        match Obj.magic prev_symb_result with
        [ [] ->
            let txt1 = name_of_symbol_failed entry s in
            txt1 ^ " or " ^ txt ^ " expected"
        | _ ->
            let txt1 = name_of_symbol_failed entry sep in
            txt1 ^ " or " ^ txt ^ " expected" ]
    | Slist1sep s sep ->
        match Obj.magic prev_symb_result with
        [ [] ->
            let txt1 = name_of_symbol_failed entry s in
            txt1 ^ " or " ^ txt ^ " expected"
        | _ ->
            let txt1 = name_of_symbol_failed entry sep in
            txt1 ^ " or " ^ txt ^ " expected" ]
    | Sopt _ | Stree _ -> txt ^ " expected"
    | _ -> txt ^ " expected after " ^ name_of_symbol entry prev_symb ]
  in
  do {
    if error_verbose.val then do {
      let tree = search_tree_in_entry prev_symb tree entry.edesc in
      set_formatter_out_channel stderr;
      open_vbox 0;
      print_newline ();
      print_string "----------------------------------";
      print_newline ();
      printf "Parse error in entry [%s], rule:" entry.ename;
      print_break 0 2;
      open_vbox 0;
      print_level force_newline (flatten_tree tree);
      close_box ();
      print_newline ();
      print_string "----------------------------------";
      print_newline ();
      close_box ();
      print_newline ()
    }
    else ();
    txt ^ " (in [" ^ entry.ename ^ "])"
  }
;

value symb_failed entry prev_symb_result prev_symb symb =
  let tree = Node {node = symb; brother = DeadEnd; son = DeadEnd} in
  tree_failed entry prev_symb_result prev_symb tree
;

external app : Obj.t -> 'a = "%identity";

value is_level_labelled n lev =
  match lev.lname with
  [ Some n1 -> n = n1
  | None -> False ]
;

value level_number entry lab =
  let rec lookup levn =
    fun
    [ [] -> failwith ("unknown level " ^ lab)
    | [lev :: levs] ->
        if is_level_labelled lab lev then levn else lookup (succ levn) levs ]
  in
  match entry.edesc with
  [ Dlevels elev -> lookup 0 elev
  | Dparser _ -> raise Not_found ]
;

value rec top_symb entry =
  fun
  [ Sself | Snext -> Snterm entry
  | Snterml e _ -> Snterm e
  | Slist1sep s sep -> Slist1sep (top_symb entry s) sep
  | _ -> raise Stream.Failure ]
;

value entry_of_symb entry =
  fun
  [ Sself | Snext -> entry
  | Snterm e -> e
  | Snterml e _ -> e
  | _ -> raise Stream.Failure ]
;

value top_tree entry =
  fun
  [ Node {node = s; brother = bro; son = son} ->
      Node {node = top_symb entry s; brother = bro; son = son}
  | _ -> raise Stream.Failure ]
;

value skip_if_empty bp p strm =
  if Stream.count strm == bp then Gramext.action (fun a -> p strm)
  else raise Stream.Failure
;

value continue entry bp a s son p1 =
  parser
    [: a = (entry_of_symb entry s).econtinue 0 bp a;
       act = p1 ? tree_failed entry a s son :] ->
      Gramext.action (fun _ -> app act a)
;

value do_recover parser_of_tree entry nlevn alevn bp a s son =
  parser
  [ [: a = parser_of_tree entry nlevn alevn (top_tree entry son) :] -> a
  | [: a = skip_if_empty bp (parser []) :] -> a
  | [: a =
         continue entry bp a s son
           (parser_of_tree entry nlevn alevn son) :] ->
      a ]
;

value strict_parsing = ref False;

value recover parser_of_tree entry nlevn alevn bp a s son strm =
  if strict_parsing.val then raise (Stream.Error (tree_failed entry a s son))
  else do_recover parser_of_tree entry nlevn alevn bp a s son strm
;

value std_token_parse =
  fun
  [ (p_con, "") -> parser [: `(con, prm) when con = p_con :] -> prm
  | (p_con, p_prm) ->
      parser [: `(con, prm) when con = p_con && prm = p_prm :] -> prm ]
;

value peek_nth n strm =
  let list = Stream.npeek n strm in
  let rec loop list n =
    match (list, n) with
    [ ([x :: _], 1) -> Some x
    | ([_ :: l], n) -> loop l (n - 1)
    | ([], _) -> None ]
  in
  loop list n
;

value rec parser_of_tree entry nlevn alevn =
  fun
  [ DeadEnd -> parser []
  | LocAct act _ -> parser [: :] -> act
  | Node {node = Sself; son = LocAct act _; brother = DeadEnd} ->
      parser [: a = entry.estart alevn :] -> app act a
  | Node {node = Sself; son = LocAct act _; brother = bro} ->
      let p2 = parser_of_tree entry nlevn alevn bro in
      parser
      [ [: a = entry.estart alevn :] -> app act a
      | [: a = p2 :] -> a ]
  | Node {node = s; son = son; brother = DeadEnd} ->
      let tokl =
        match s with
        [ Stoken tok when entry.egram.glexer.Token.tparse tok = None ->
            get_token_list entry [] tok son
        | _ -> None ]
      in
      match tokl with
      [ None ->
          let ps = parser_of_symbol entry nlevn s in
          let p1 = parser_of_tree entry nlevn alevn son in
          let p1 = parser_cont p1 entry nlevn alevn s son in
          parser bp [: a = ps; act = p1 bp a :] -> app act a
      | Some (tokl, last_tok, son) ->
          let p1 = parser_of_tree entry nlevn alevn son in
          let p1 = parser_cont p1 entry nlevn alevn (Stoken last_tok) son in
          parser_of_token_list p1 tokl ]
  | Node {node = s; son = son; brother = bro} ->
      let tokl =
        match s with
        [ Stoken tok when entry.egram.glexer.Token.tparse tok = None ->
            get_token_list entry [] tok son
        | _ -> None ]
      in
      match tokl with
      [ None ->
          let ps = parser_of_symbol entry nlevn s in
          let p1 = parser_of_tree entry nlevn alevn son in
          let p1 = parser_cont p1 entry nlevn alevn s son in
          let p2 = parser_of_tree entry nlevn alevn bro in
          parser bp
          [ [: a = ps; act = p1 bp a :] -> app act a
          | [: a = p2 :] -> a ]
      | Some (tokl, last_tok, son) ->
          let p1 = parser_of_tree entry nlevn alevn son in
          let p1 = parser_cont p1 entry nlevn alevn (Stoken last_tok) son in
          let p1 = parser_of_token_list p1 tokl in
          let p2 = parser_of_tree entry nlevn alevn bro in
          parser
          [ [: a = p1 :] -> a
          | [: a = p2 :] -> a ] ] ]
and parser_cont p1 entry nlevn alevn s son bp a =
  parser
  [ [: a = p1 :] -> a
  | [: a = recover parser_of_tree entry nlevn alevn bp a s son :] -> a
  | [: :] -> raise (Stream.Error (tree_failed entry a s son)) ]
and parser_of_token_list p1 tokl =
  loop 1 tokl where rec loop n =
    fun
    [ [(p_con, "")] ->
        let ps strm =
          match peek_nth n strm with
          [ Some (con, prm) when p_con = "ANY" || con = p_con ->
              do { for i = 1 to n do { Stream.junk strm }; Obj.repr prm }
          | _ -> raise Stream.Failure ]
        in
        parser bp [: a = ps; act = p1 bp a :] -> app act a
    | [(p_con, p_prm)] ->
        let ps strm =
          match peek_nth n strm with
          [ Some (con, prm)
            when (p_con = "ANY" || con = p_con) && prm = p_prm ->
              do { for i = 1 to n do { Stream.junk strm }; Obj.repr prm }
          | _ -> raise Stream.Failure ]
        in
        parser bp [: a = ps; act = p1 bp a :] -> app act a
    | [(p_con, "") :: tokl] ->
        let ps strm =
          match peek_nth n strm with
          [ Some (con, prm) when p_con = "ANY" || con = p_con -> prm
          | _ -> raise Stream.Failure ]
        in
        let p1 = loop (n + 1) tokl in
        parser
          [: a = ps; s :] ->
            let act = p1 s in
            app act a
    | [(p_con, p_prm) :: tokl] ->
        let ps strm =
          match peek_nth n strm with
          [ Some (con, prm)
            when (p_con = "ANY" || con = p_con) && prm = p_prm ->
              prm
          | _ -> raise Stream.Failure ]
        in
        let p1 = loop (n + 1) tokl in
        parser
          [: a = ps; s :] ->
            let act = p1 s in
            app act a
    | [] -> assert False ]
and parser_of_symbol entry nlevn =
  fun
  [ Slist0 s ->
      let ps = parser_of_symbol entry nlevn s in
      let rec loop al =
        parser
        [ [: a = ps; s :] -> loop [a :: al] s
        | [: :] -> al ]
      in
      parser [: a = loop [] :] -> Obj.repr (List.rev a)
  | Slist0sep symb sep ->
      let ps = parser_of_symbol entry nlevn symb in
      let pt = parser_of_symbol entry nlevn sep in
      let rec kont al =
        parser
        [ [: v = pt; a = ps ? symb_failed entry v sep symb; s :] ->
            kont [a :: al] s
        | [: :] -> al ]
      in
      parser
      [ [: a = ps; s :] -> Obj.repr (List.rev (kont [a] s))
      | [: :] -> Obj.repr [] ]
  | Slist1 s ->
      let ps = parser_of_symbol entry nlevn s in
      let rec loop al =
        parser
        [ [: a = ps; s :] -> loop [a :: al] s
        | [: :] -> al ]
      in
      parser [: a = ps; s :] -> Obj.repr (List.rev (loop [a] s))
  | Slist1sep symb sep ->
      let ps = parser_of_symbol entry nlevn symb in
      let pt = parser_of_symbol entry nlevn sep in
      let rec kont al =
        parser
        [ [: v = pt;
             a =
               parser
               [ [: a = ps :] -> a
               | [: a =
                      parser_of_symbol entry nlevn (top_symb entry symb) :] ->
                   a
               | [: :] ->
                   raise (Stream.Error (symb_failed entry v sep symb)) ];
             s :] ->
            kont [a :: al] s
        | [: :] -> al ]
      in
      parser [: a = ps; s :] -> Obj.repr (List.rev (kont [a] s))
  | Sopt s ->
      let ps = parser_of_symbol entry nlevn s in
      parser
      [ [: a = ps :] -> Obj.repr (Some a)
      | [: :] -> Obj.repr None ]
  | Stree t ->
      let pt = parser_of_tree entry 1 0 t in
      parser bp
        [: a = pt :] ep ->
          let loc = loc_of_token_interval bp ep in
          app a loc
  | Snterm e -> parser [: a = e.estart 0 :] -> a
  | Snterml e l -> parser [: a = e.estart (level_number e l) :] -> a
  | Sself -> parser [: a = entry.estart 0 :] -> a
  | Snext -> parser [: a = entry.estart nlevn :] -> a
  | Stoken ("ANY", v) ->
      if v = "" then parser [: `(_, x) :] -> Obj.repr x
      else parser [: `(_, x) when x = v :] -> Obj.repr x
  | Stoken tok ->
      match entry.egram.glexer.Token.tparse tok with
      [ Some f -> (Obj.magic f : Stream.t Token.t -> Obj.t)
      | None ->
          (Obj.magic (std_token_parse tok) : Stream.t Token.t -> Obj.t) ] ]
;

value rec continue_parser_of_levels entry clevn =
  fun
  [ [] -> fun levn bp a -> parser []
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
              [ [: a = p1 levn bp a :] -> a
              | [: act = p2 :] ep ->
                  let a = app act a (loc_of_token_interval bp ep) in
                  entry.econtinue levn bp a strm ] ] ]
;

value rec start_parser_of_levels entry clevn =
  fun
  [ [] -> fun levn -> parser []
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
                match strm with parser bp
                [ [: act = p2 :] ep ->
                    let a = app act (loc_of_token_interval bp ep) in
                    entry.econtinue levn bp a strm ]
          | _ ->
              fun levn strm ->
                if levn > clevn then p1 levn strm
                else
                  match strm with parser bp
                  [ [: act = p2 :] ep ->
                      let a = app act (loc_of_token_interval bp ep) in
                      entry.econtinue levn bp a strm
                  | [: a = p1 levn :] -> a ] ] ] ]
;

value continue_parser_of_entry entry =
  match entry.edesc with
  [ Dlevels elev ->
      let p = continue_parser_of_levels entry 0 elev in
      fun levn bp a ->
        parser
        [ [: a = p levn bp a :] -> a
        | [: :] -> a ]
  | Dparser p -> fun levn bp a -> parser [] ]
;

value empty_entry ename levn strm =
  raise (Stream.Error ("entry [" ^ ename ^ "] is empty"))
;

value start_parser_of_entry entry =
  match entry.edesc with
  [ Dlevels [] -> empty_entry entry.ename
  | Dlevels elev -> start_parser_of_levels entry 0 elev
  | Dparser p -> fun levn strm -> p strm ]
;

value parse_parsable entry efun (cs, (ts, fun_loc)) =
  let restore =
    let old_floc = floc.val in
    fun () -> floc.val := old_floc
  in
  do {
    floc.val := fun_loc;
    try
      let r = efun ts in
      do { restore (); r }
    with
    [ Stream.Failure ->
        let loc =
          try fun_loc (Stream.count ts) with _ ->
            (Stream.count cs, Stream.count cs + 1)
        in
        do {
          restore ();
          raise_with_loc loc
            (Stream.Error ("illegal begin of " ^ entry.ename))
        }
    | Stream.Error _ as exc ->
        let loc =
          try fun_loc (Stream.count ts) with _ ->
            (Stream.count cs, Stream.count cs + 1)
        in
        do { restore (); raise_with_loc loc exc }
    | exc ->
        let loc = (Stream.count cs, Stream.count cs + 1) in
        do { restore (); raise_with_loc loc exc } ]
  }
;

value wrap_parse entry efun cs =
  let parsable = (cs, entry.egram.glexer.Token.func cs) in
  parse_parsable entry efun parsable
;

value create_toktab () = Hashtbl.create 301;
value create lexer = {gtokens = create_toktab (); glexer = lexer};

(* Extend syntax *)

value extend_entry entry position rules =
  try
    let elev = Gramext.levels_of_rules entry position rules in
    do {
      entry.edesc := Dlevels elev;
      entry.estart :=
        fun lev strm ->
          let f = start_parser_of_entry entry in
          do { entry.estart := f; f lev strm };
      entry.econtinue :=
        fun lev bp a strm ->
          let f = continue_parser_of_entry entry in
          do { entry.econtinue := f; f lev bp a strm }
    }
  with
  [ Token.Error s ->
      do {
        Printf.eprintf "Lexer initialization error.\n%s\n"
          (String.capitalize s);
        flush stderr;
        failwith "Grammar.extend"
      } ]
;

value extend entry_rules_list =
  let gram = ref None in
  List.iter
    (fun (entry, position, rules) ->
       do {
         match gram.val with
         [ Some g ->
             if g != entry.egram then do {
               Printf.eprintf "Error: entries with different grammars\n";
               flush stderr;
               failwith "Grammar.extend"
             }
             else ()
         | None -> gram.val := Some entry.egram ];
         extend_entry entry position rules
       })
    entry_rules_list
;

(* Deleting a rule *)

value delete_rule entry sl =
  match entry.edesc with
  [ Dlevels levs ->
      let levs = Gramext.delete_rule_in_level_list entry sl levs in
      do {
        entry.edesc := Dlevels levs;
        entry.estart :=
          fun lev strm ->
            let f = start_parser_of_entry entry in
            do { entry.estart := f; f lev strm };
        entry.econtinue :=
          fun lev bp a strm ->
            let f = continue_parser_of_entry entry in
            do { entry.econtinue := f; f lev bp a strm }
      }
  | _ -> () ]
;

(* Unsafe *)

value clear_entry e =
  do {
    e.estart := fun _ -> parser [];
    e.econtinue := fun _ _ _ -> parser [];
    match e.edesc with
    [ Dlevels _ -> e.edesc := Dlevels []
    | Dparser _ -> () ]
  }
;

value reinit_gram g lexer = do { Hashtbl.clear g.gtokens; g.glexer := lexer };

module Unsafe =
  struct value clear_entry = clear_entry; value reinit_gram = reinit_gram; end
;

exception EntryFound of g_entry;
value find_entry e s =
  let rec find_levels levs =
    try
      do {
        List.iter
          (fun lev -> do { find_tree lev.lsuffix; find_tree lev.lprefix })
          levs;
        raise Not_found
      }
    with
    [ EntryFound e -> e
    | _ -> raise Not_found ]
  and find_symbol =
    fun
    [ Snterm e -> if e.ename = s then raise (EntryFound e) else ()
    | Snterml e _ -> if e.ename = s then raise (EntryFound e) else ()
    | Slist0 s -> find_symbol s
    | Slist0sep s _ -> find_symbol s
    | Slist1 s -> find_symbol s
    | Slist1sep s _ -> find_symbol s
    | Sopt s -> find_symbol s
    | Stree t -> find_tree t
    | _ -> () ]
  and find_tree =
    fun
    [ Node {node = s; brother = bro; son = son} ->
        do { find_symbol s; find_tree bro; find_tree son }
    | _ -> () ]
  in
  match e.edesc with
  [ Dlevels levs -> find_levels levs
  | Dparser _ -> raise Not_found ]
;

value of_entry e = e.egram;

module Entry =
  struct
    type e 'a = g_entry;
    value create g n =
      {egram = g; ename = n; estart = empty_entry n;
       econtinue _ _ _ = parser []; edesc = Dlevels []}
    ;
    value parse (entry : e 'a) cs : 'a =
      Obj.magic (wrap_parse entry (entry.estart 0) cs)
    ;
    value parse_token (entry : e 'a) ts : 'a = Obj.magic (entry.estart 0 ts);
    value name e = e.ename;
    value of_parser g n (p : Stream.t Token.t -> 'a) : e 'a =
      {egram = g; ename = n; estart _ = Obj.magic p;
       econtinue _ _ _ = parser []; edesc = Dparser (Obj.magic p)}
    ;
    external obj : e 'a -> Gramext.g_entry = "%identity";
    value print e = print_entry (obj e);
    value find e = Obj.magic (find_entry (obj e));
  end
;

value tokens g con =
  let g = grammar_obj g in
  let list = ref [] in
  do {
    Hashtbl.iter
      (fun (p_con, p_prm) c ->
         if p_con = con then list.val := [(p_prm, c.val) :: list.val] else ())
      g.gtokens;
    list.val
  }
;

value warning_verbose = Gramext.warning_verbose;

(* Functorial interface *)

module type LexerType = sig value lexer : Token.lexer; end;

module type S =
  sig
    type parsable = 'x;
    value parsable : Stream.t char -> parsable;
    value tokens : string -> list (string * int);
    module Entry :
      sig
        type e 'a = 'x;
        value create : string -> e 'a;
        value parse : e 'a -> parsable -> 'a;
        value parse_token : e 'a -> Stream.t Token.t -> 'a;
        value name : e 'a -> string;
        value of_parser : string -> (Stream.t Token.t -> 'a) -> e 'a;
        value print : e 'a -> unit;
        external obj : e 'a -> Gramext.g_entry = "%identity";
      end
    ;
    module Unsafe :
      sig
        value reinit_gram : Token.lexer -> unit;
        value clear_entry : Entry.e 'a -> unit;
      end
    ;
    value extend :
      Entry.e 'a -> option Gramext.position ->
        list
          (option string * option Gramext.g_assoc *
           list (list Gramext.g_symbol * Gramext.g_action)) ->
        unit;
    value delete_rule : Entry.e 'a -> list Gramext.g_symbol -> unit;
  end
;

module Make (L : LexerType) : S =
  struct
    type parsable =
      (Stream.t char * (Stream.t Token.t * Token.location_function))
    ;
    value gram = create L.lexer;
    value parsable cs = (cs, L.lexer.Token.func cs);
    value tokens = tokens gram;
    module Entry =
      struct
        type e 'a = g_entry;
        value create n =
          {egram = gram; ename = n; estart = empty_entry n;
           econtinue _ _ _ = parser []; edesc = Dlevels []}
        ;
        value parse (e : e 'a) p : 'a =
          Obj.magic (parse_parsable e (e.estart 0) p)
        ;
        value parse_token (e : e 'a) ts : 'a = Obj.magic (e.estart 0 ts);
        value name e = e.ename;
        value of_parser n (p : Stream.t Token.t -> 'a) : e 'a =
          {egram = gram; ename = n; estart _ = Obj.magic p;
           econtinue _ _ _ = parser []; edesc = Dparser (Obj.magic p)}
        ;
        external obj : e 'a -> Gramext.g_entry = "%identity";
        value print e = print_entry (obj e);
      end
    ;
    module Unsafe =
      struct
        value reinit_gram = Unsafe.reinit_gram gram;
        value clear_entry = Unsafe.clear_entry;
      end
    ;
    value extend = extend_entry;
    value delete_rule e r = delete_rule (Entry.obj e) r;
  end
;
